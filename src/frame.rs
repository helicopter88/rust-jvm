use std::borrow::Borrow;
use std::collections::VecDeque;
use std::rc::Rc;

use crate::class::Class;
use crate::enums::{Arithmetic, BootstrapMethod, ConstantPool, LocalVariable};
use crate::vm::VM;

type Stack = VecDeque<LocalVariable>;

pub struct Frame {
    ip: u32,
    code: Vec<u8>,
    locals: Vec<LocalVariable>,
    stack: Stack,
    class: Rc<Class>,
    method_name: String,
    vm: Rc<VM>,
    bootstrap_methods: Vec<BootstrapMethod>,
    will_execute_native_method: bool,
}

fn execute_iload(op: u8, locals: &Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
{
    const ZEROTH_VARIABLE: u8 = 0x1A;
    let variable_to_load = op - ZEROTH_VARIABLE;
    match locals.get(variable_to_load as usize).ok_or("Missing local")? {
        LocalVariable::Int(num) => {
            stack.push_front(LocalVariable::Int(*num));
            Ok(())
        }
        def => Err(format!("Wrong type for iload ({:x}), var_idx={} {:#?}", op, variable_to_load, def))
    }
}

fn execute_aload(op: u8, locals: &Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
{
    const ZEROTH_VARIABLE: u8 = 0x2A;
    let variable_to_load = op - ZEROTH_VARIABLE;
    match locals.get(variable_to_load as usize).ok_or("Missing local")? {
        LocalVariable::Reference(addr) => {
            stack.push_front(LocalVariable::Reference(*addr));
            Ok(())
        }
        def => Err(format!("Wrong type for aload {:x} {:#?}", op, def))
    }
}

fn execute_astore(op: u8, locals: &mut Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
{
    const ZEROTH_VARIABLE: usize = 0x4B;
    let variable_to_store: usize = op as usize - ZEROTH_VARIABLE;
    let item = stack.pop_front().ok_or("Empty stack")?;
    match item {
        LocalVariable::Reference(addr) => {
            locals[variable_to_store] = LocalVariable::Reference(addr);
            Ok(())
        }
        LocalVariable::ReturnAddress(addr) => {
            locals[variable_to_store] = LocalVariable::ReturnAddress(addr);
            Ok(())
        }
        def => Err(format!("Wrong type for astore ({:x}) {:#?}", op, def))
    }
}

fn execute_istore(op: u8, locals: &mut Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
{
    const ZEROTH_VARIABLE: usize = 0x3B;
    let variable_to_store: usize = op as usize - ZEROTH_VARIABLE;
    let item = stack.pop_front().ok_or("Empty stack")?;
    match item {
        LocalVariable::Int(addr) => {
            locals[variable_to_store] = LocalVariable::Int(addr);
            Ok(())
        }
        def => Err(format!("Wrong type for istore ({:x}) {:#?}", op, def))
    }
}

fn execute_load_const(op: u8, stack: &mut Stack)
{
    const NEGATIVE_ONE_STARTING_POINT: i32 = 0x3;
    stack.push_front(LocalVariable::Int(op as i32 - NEGATIVE_ONE_STARTING_POINT))
}


fn execute_add(stack: &mut Stack) -> Result<(), String>
{
    let b = stack.pop_front().ok_or("Missing stack variable 1")?;
    let a = stack.pop_front().ok_or("Missing stack variable 2")?;

    stack.push_front(LocalVariable::sum(&a, &b)?);

    Ok(())
}


fn execute_sub(stack: &mut Stack) -> Result<(), String>
{
    let b = stack.pop_front().ok_or("Missing stack variable 1")?;
    let a = stack.pop_front().ok_or("Missing stack variable 2")?;

    stack.push_front(LocalVariable::sub(&a, &b)?);

    Ok(())
}

fn execute_if(op: u8, stack: &mut Stack) -> Result<bool, String> {
    let b = stack.pop_front().ok_or("Missing stack variable 1")?;
    let a = if op >= 0x99 && op <=
        0x9e { LocalVariable::Int(0) } else { stack.pop_front().ok_or("Missing stack variable 2")? };
    match op {
        0x95 | 0x99 => { Ok(a == b) }
        0xa0 | 0x9a => { Ok(a != b) }
        0xa1 | 0x9b => { Ok(a < b) }
        0xa2 | 0x9c => { Ok(a >= b) }
        0xa3 | 0x9d => { Ok(a > b) }
        0xa4 | 0x9e => { Ok(a <= b) }
        _ => Err(format!("Unexpected op {}", op))
    }
}

fn parse_type(prototype: &str) -> Result<(usize, String), String>
{
    let mut ret_idx = 0;
    let mut in_class = false;
    let mut count = 0;
    for char in prototype.chars()
    {
        ret_idx += 1;
        if char == '(' {
            continue;
        }
        if in_class {
            if char == ';' {
                in_class = false;
            }
            continue;
        }
        if char == ')'
        {
            break;
        }
        if char == 'L'
        {
            in_class = true;
        }
        count += 1;
    };
    Ok((count, prototype.split_at(ret_idx).1.to_string()))
}

fn execute_iconv(op: u8, stack: &mut Stack) -> Result<(), String> {
    let int = stack.pop_front().ok_or("Empty stack")?;
    stack.push_front(match int {
        LocalVariable::Int(num) => {
            match op {
                0x85 => { Ok(LocalVariable::Long(num as i64)) }
                0x86 => { Ok(LocalVariable::Float(num as f32)) }
                0x87 => { Ok(LocalVariable::Double(num as f64)) }
                0x91 => { Ok(LocalVariable::Byte(num as u8)) }
                0x92 => { Ok(LocalVariable::Char(num as u8 as char)) }
                0x93 => { Ok(LocalVariable::Short(num as i16)) }
                _ => Err(format!("Unexpected op, {:x}", op))
            }
        }
        _ => Err(format!("Expected Int, found {:?}", int))
    }?);
    Ok(())
}

fn read_u16_from_vec(data: &Vec<u8>, starting_point: usize) -> u16 {
    let mut d = [0 as u8; 2];
    for (idx, a) in data[starting_point..starting_point + 2].iter().enumerate() {
        d[idx] = *a;
    }
    u16::from_be_bytes(d)
}

impl Frame {
    pub fn new(vm: &Rc<VM>, class: &Rc<Class>, method_name: String, local_variables: Vec<LocalVariable>) -> Result<Frame, String>
    {
        dbg!(format!("Will try to create a frame for {}::{}({:?})", class.name, method_name, local_variables));
        let mut f = Frame {
            ip: 0,
            code: vec![],
            locals: vec![],
            stack: VecDeque::new(),
            class: class.clone(),
            method_name,
            vm: vm.clone(),
            bootstrap_methods: vec![],
            will_execute_native_method: false,
        };
        for attrib in &class.attributes {
            if attrib.name.as_str() == "BootstrapMethods" {
                let num_bootstraps = read_u16_from_vec(attrib.data.borrow(), 0);

                let mut index = 2;
                for _ in 0..num_bootstraps {
                    let method_ref = read_u16_from_vec(&attrib.data, index);
                    index += 2;
                    let num_arguments = read_u16_from_vec(&attrib.data, index);
                    index += 2;
                    let mut args = vec![0 as u16; num_arguments as usize];
                    for _ in 0..num_arguments {
                        args.push(read_u16_from_vec(&attrib.data, index));
                        index += 2
                    }
                    f.bootstrap_methods.push(BootstrapMethod::new(
                        method_ref,
                        num_arguments,
                        args,
                    ));
                }
            }
        }

        for method in &class.methods
        {
            if method.name == f.method_name {
                for attrib in &method.attributes {
                    if attrib.name.as_str() == "Code" && attrib.data.len() > 8 {
                        let mut d = [0 as u8; 2];
                        for (idx, a) in attrib.data[2..4].iter().enumerate() {
                            d[idx] = *a;
                        }
                        let max_locals = u16::from_be_bytes(d);
                        f.code = attrib.data[8..].to_vec();
                        f.locals = vec![LocalVariable::Void(); max_locals as usize + 1];
                        for (idx, arg) in local_variables.iter().enumerate() {
                            f.locals[idx] = arg.clone();
                        }
                        return Ok(f);
                    }
                }
            }
        }
        if vm.native_methods.borrow().contains_key(&VM::make_native_method_name(&class.name, &f.method_name.clone())) {
            dbg!("Native method {}", &f.method_name.clone());
            for arg in local_variables.iter() {
                f.locals.push(arg.clone());
            }
            f.will_execute_native_method = true;
            return Ok(f);
        }

        Err(format!("Method not found: {}", f.method_name))
    }

    fn resolve_method_handle_ref(&self, idx: u16) -> Result<(String, (String, String)), String>
    {
        let cp = &self.class.constant_pool;

        let ref_ = &cp[idx as usize];
        match ref_ {
            ConstantPool::MethodHandle(kind, reference_) => {
                let reference = *reference_ as usize;
                match kind {
                    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 => {
                        match cp.get(reference).ok_or(format!("Pointed to non existing thing: {}", reference).to_string())? {
                            ConstantPool::FieldRef(class_idx, nat) | ConstantPool::MethodRef(class_idx, nat) | ConstantPool::InterfaceMethodRef(class_idx, nat) => {
                                Ok((self.vm.resolve_string(*class_idx as usize), self.find_name_and_type(*nat)?))
                            }
                            d => { Err(format!("Unexpected field {:?} for kind: {}", d, kind).to_string()) }
                        }
                    }
                    _ => { Err(format!("Unexpected kind: {}", kind).to_string()) }
                }
            }
            def => { Err(format!("Expected MethodHandle, got {:#?}", def).to_string()) }
        }
    }

    fn find_method_or_field(&self, idx: u16) -> Result<(String, (String, String)), String>
    {
        let cp = &self.class.constant_pool;

        let ref_ = &cp[idx as usize];
        match ref_ {
            ConstantPool::MethodRef(class_idx, method_idx) => {
                Ok((self.vm.resolve_string(*class_idx as usize),
                    self.find_name_and_type(*method_idx)?))
            }
            ConstantPool::FieldRef(class_idx, method_idx) => {
                Ok((self.vm.resolve_string(*class_idx as usize),
                    self.find_name_and_type(*method_idx)?))
            }
            def => Err(format!("Found {:#?} which is not a method or field", def))
        }
    }

    fn find_name_and_type(&self, idx: u16) -> Result<(String, String), String>
    {
        let cp = &self.class.constant_pool;

        let ref_ = &cp[idx as usize];
        match ref_ {
            ConstantPool::NameAndTypeIndex(name_idx, type_idx) => {
                Ok((self.vm.resolve_string(*name_idx as usize),
                    self.vm.resolve_string(*type_idx as usize)))
            }
            def => Err(format!("Found {:#?} which is not a NaT", def))
        }
    }
    fn find_invoke_dynamic(&self, idx: u16) -> Result<(u16, (String, String)), String>
    {
        let cp = &self.class.constant_pool;

        let ref_ = &cp[idx as usize];
        match ref_ {
            ConstantPool::InvokeDynamic(method_attr, name_and_type) => {
                Ok((*method_attr,
                    self.find_name_and_type(*name_and_type)?))
            }
            ConstantPool::Dynamic(method_attr, name_and_type) => {
                Ok((*method_attr,
                    self.find_name_and_type(*name_and_type)?))
            }
            def => Err(format!("Found {:#?} which is not a dynamic or invoke dynamic", def))
        }
    }

    pub fn get_index_byte(&mut self) -> u16
    {
        let mut buf: [u8; 2] = [0; 2];
        for x in 1..=2
        {
            buf[x - 1] = self.code[self.ip as usize + x]
        }
        let idx = u16::from_be_bytes(buf);
        self.ip += 2;
        return idx;
    }
    pub fn get_short(&mut self) -> u8
    {
        let mut buf: [u8; 1] = [0; 1];
        for x in 1..2
        {
            buf[x - 1] = self.code[self.ip as usize + x]
        }
        let idx = u8::from_be_bytes(buf);
        self.ip += 1;
        return idx;
    }
    pub fn exec(&mut self) -> Result<LocalVariable, String>
    {
        if self.will_execute_native_method
        {
            let method = self.vm.native_methods.borrow().get(&VM::make_native_method_name(&self.class.name, &self.method_name.clone())).ok_or("This should never be possible")?;
            return Ok(method(&self.vm, self.locals.as_slice()));
        }
        loop {
            let curr_ip = self.ip;
            let op = self.code[curr_ip as usize];
            dbg!(format!("OP: {}/{:x}({}) stack: {:?} locals: {:?}", &self.method_name, op, op, &self.stack, &self.locals));
            match op {
                0x2 | 0x3 | 0x4 | 0x5 | 0x6 | 0x7 | 0x8 /* iconst_<d> */ => {
                    execute_load_const(op, &mut self.stack);
                }
                0x12 /*ldc*/ => {
                    let idx = self.get_short();
                    match self.class.constant_pool.get(idx as usize).ok_or(format!("Couldn't find item in cp at {}", idx))? {
                        ConstantPool::Integer(num) => { self.stack.push_front(LocalVariable::Int(*num)); }
                        ConstantPool::StringIndex(s_idx) => { self.stack.push_front(LocalVariable::Reference(*s_idx)); }
                        def => return Err(format!("Unexpected item={:?}", def))
                    };
                }
                0x1A | 0x1B | 0x1C | 0x1D/* iload_<n> */ => {
                    execute_iload(op, &self.locals, &mut self.stack)?
                }
                0x10 => /*bipush*/ {
                    let byte = self.get_short() as i32;
                    self.stack.push_front(LocalVariable::Int(byte));
                }
                0x2A | 0x2B | 0x2C | 0x2D/* aload_<n> */ => {
                    execute_aload(op, &self.locals, &mut self.stack)?
                }
                0x3b | 0x3c | 0x3d | 0x3e => {
                    execute_istore(op, &mut self.locals, &mut self.stack)?
                }
                0x4b | 0x4c | 0x4d | 0x4e => {
                    execute_astore(op, &mut self.locals, &mut self.stack)?
                }
                0x60 | 0x61 | 0x62 | 0x63 /* iadd, ladd, fadd, dadd */ => {
                    execute_add(&mut self.stack)?
                }
                0x64 | 0x65 | 0x66 | 0x67 => {
                    execute_sub(&mut self.stack)?
                }
                0x91 | 0x92 | 0x93 | 0x85 | 0x86 | 0x87 /*iconv*/ => {
                    execute_iconv(op, &mut self.stack);
                }
                0xa7 /*goto*/ => {
                    let idx = self.get_index_byte();
                    self.ip += idx as u32;
                }
                0x9f | 0xa0 | 0xa1 | 0xa2 | 0xa3 | 0xa4 | 0x99 | 0x9a | 0x9b | 0x9c | 0x9d | 0x9e => /*if_i<cmp>*/{
                    let idx = self.get_index_byte();
                    if execute_if(op, &mut self.stack)? {
                        self.ip += idx as u32;
                    }
                }
                0xba /*invokeVirtual */ => {
                    let idx = self.get_index_byte();
                    let _ = self.get_index_byte();
                    let (bootstrap_idx, (method_name, method_type)) = self.find_invoke_dynamic(idx)?;
                    let method_handle = self.bootstrap_methods.get(bootstrap_idx as usize).ok_or("Non existing bootstrap method")?;
                    let method_ref = self.resolve_method_handle_ref(method_handle.method_ref)?;

                    let class_name_ref = &method_ref.0;
                    let mut new_stack = vec![];
                    let class = &self.vm.classes.borrow().get(class_name_ref)
                        .ok_or(format!("Could not find class {:#?}", class_name_ref).to_string())?
                        .clone();
                    let (argc, _ret) = parse_type(&method_type)?;
                    for _ in 0..argc {
                        new_stack.push(self.stack.pop_front().ok_or("Empty stack when calling new method")?);
                    }
                    new_stack.reverse();
                    let new_frame = Frame::new(&self.vm, &class, method_name.clone(), new_stack.clone())?;
                    let mut n_frame = new_frame;
                    let ret = n_frame.exec()?;
                    match ret {
                        LocalVariable::Void() => {}
                        def => {
                            self.locals.push(def)
                        }
                    };
                }
                0xB2 | 0xB6 | 0xB7 | 0xB8 /* getStatic, invokeVirtual, invokeSpecial, invokeStatic */ => {
                    let idx = self.get_index_byte();
                    let (class_name, (method_name, method_type)) = self.find_method_or_field(idx)?;
                    let class_name_ref = &class_name;
                    let mut new_stack = vec![];
                    let class = &self.vm.classes.borrow().get(class_name_ref)
                        .ok_or(format!("Could not find class {:#?}", class_name_ref).to_string())?
                        .clone();
                    let (argc, _ret) = parse_type(&method_type)?;

                    match op {
                        0xB2 => {
                            let mut count = 1;
                            for field in &class.fields {
                                if field.name == method_name
                                {
                                    self.stack.push_front(LocalVariable::Reference(count))
                                }
                                count += 1;
                            }
                        }
                        0xB6 | 0xB7  /* invokeVirtual, invokeSpecial */ => {
                            for _ in 0..=argc {
                                new_stack.push(self.stack.pop_front().ok_or("Empty stack when calling new method")?);
                            }
                            new_stack.reverse();
                            let new_frame = Frame::new(&self.vm, &class, method_name.clone(), new_stack.clone())?;
                            let mut n_frame = new_frame;
                            let ret = n_frame.exec()?;
                            match ret {
                                LocalVariable::Void() => {}
                                def => {
                                    self.stack.push_front(def)
                                }
                            };
                        }
                        0xB8 /* invokeStatic */ => {
                            for _ in 0..argc {
                                new_stack.push(self.stack.pop_front().ok_or("Empty stack when calling new method")?);
                            }

                            new_stack.reverse();
                            let new_frame = Frame::new(&self.vm, &class, method_name.clone(), new_stack.clone())?;
                            let mut n_frame = new_frame;
                            let ret = n_frame.exec()?;
                            match ret {
                                LocalVariable::Void() => { dbg!("Ignoring void"); }
                                def => {
                                    self.stack.push_front(def)
                                }
                            };
                        }
                        _ => {}
                    }
                }
                0x57 /* pop */ => {
                    self.stack.pop_front().ok_or("Empty stack when popping")?;
                }
                0x59 /* dup */ =>
                    {
                        let item = self.stack.pop_front()
                            .ok_or("Empty stack when duplicating")?;
                        self.stack.push_front(item.clone());
                        self.stack.push_front(item.clone());
                    }
                0xAC /* ireturn */ => {
                    let ret = self.stack.pop_front()
                        .ok_or("Empty stack when returning int")?;
                    return Ok(ret);
                }
                0xB1 /* return */ => {
                    return Ok(LocalVariable::Void());
                }

                0xBB /* new */ => {
                    let idx = self.get_index_byte();
                    let class_name = self.vm.resolve_string(idx as usize);

                    self.stack.push_front(LocalVariable::Reference((self.vm.new_object(class_name)) as u16))
                }

                def => {
                    return Err(format!("Couldn't execute {:x}-{}", def, def).to_string());
                }
            }
            self.ip += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::enums::LocalVariable;
    use crate::frame::{execute_add, execute_iload, execute_load_const, execute_sub, Stack};

    #[test]
    fn load_constants() {
        let mut stack = VecDeque::<LocalVariable>::new();
        execute_load_const(0x2, &mut stack);
        assert!(!stack.is_empty());
        assert_eq!(1, stack.len());
        assert_eq!(LocalVariable::Int(-1), stack.pop_front().unwrap());
        execute_load_const(0x2, &mut stack);
        execute_load_const(0x3, &mut stack);
        execute_load_const(0x4, &mut stack);
        execute_load_const(0x5, &mut stack);
        execute_load_const(0x6, &mut stack);
        assert!(!stack.is_empty());
        assert_eq!(5, stack.len());
        for number in &[3, 2, 1, 0]
        {
            assert_eq!(LocalVariable::Int(*number), stack.pop_front().unwrap());
        }
    }

    #[test]
    fn load_locals() {
        let mut stack = VecDeque::<LocalVariable>::new();
        let locals = vec![LocalVariable::Int(3)];
        {
            assert!(execute_iload(0x1A, &locals, &mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Int(3), stack.pop_front().unwrap());
        }
        {
            assert!(execute_iload(0x1B, &locals, &mut stack).is_err());
            assert!(stack.is_empty());
            assert_eq!(0, stack.len());
        }
        {
            let locals = vec![LocalVariable::Int(3), LocalVariable::Boolean(true)];
            assert!(execute_iload(0x1B, &locals, &mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Boolean(true), stack.pop_front().unwrap());
        }
    }

    #[test]
    fn test_sum() {
        {
            let mut stack = Stack::from(vec![LocalVariable::Int(2), LocalVariable::Int(2)]);
            assert!(execute_add(&mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Int(4), stack.pop_front().unwrap());
        }
        {
            let mut stack = Stack::from(vec![LocalVariable::Long(2 << 32), LocalVariable::Long(3 << 32)]);
            assert!(execute_add(&mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Long(5 << 32), stack.pop_front().unwrap());
        }
        {
            let mut stack = Stack::from(vec![LocalVariable::Long(2 << 32), LocalVariable::Int(3)]);
            assert!(execute_add(&mut stack).is_err());
            assert!(stack.is_empty());
        }
    }

    #[test]
    fn test_sub() {
        {
            let mut stack = Stack::from(vec![LocalVariable::Int(2), LocalVariable::Int(2)]);
            assert!(execute_sub(&mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Int(0), stack.pop_front().unwrap());
        }
        {
            let mut stack = Stack::from(vec![LocalVariable::Long(2 << 32), LocalVariable::Long(3 << 32)]);
            assert!(execute_sub(&mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Long(1 << 32), stack.pop_front().unwrap());
        }
        {
            let mut stack = Stack::from(vec![LocalVariable::Long(2 << 32), LocalVariable::Int(3)]);
            assert!(execute_sub(&mut stack).is_err());
            assert!(stack.is_empty());
        }
    }
}