use std::borrow::{Borrow, BorrowMut};
use std::collections::VecDeque;
use std::rc::Rc;

use crate::class::Class;
use crate::enums::{Arithmetic, BootstrapMethod, ConstantPool, Field, LocalVariable, ReferenceKind};
use crate::vm::VM;
use crate::enums::ReferenceKind::ObjectReference;
use std::ops::Neg;

type Stack = VecDeque<LocalVariable>;

#[derive(Clone)]
pub struct Frame {
    ip: u32,
    code: Vec<u8>,
    locals: Vec<LocalVariable>,
    stack: Stack,
    class: Rc<Class>,
    method_name: String,
    method_type: String,
    pub(crate) vm: Rc<VM>,
    bootstrap_methods: Vec<BootstrapMethod>,
    will_execute_native_method: bool,
}

fn execute_iload_n(op: u8, locals: &Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
{
    const ZEROTH_VARIABLE: u8 = 0x1A;
    let variable_to_load = op - ZEROTH_VARIABLE;
    match locals.get(variable_to_load as usize).ok_or("Missing local")?.to_lv_int() {
        LocalVariable::Int(num) => {
            stack.push_front(LocalVariable::Int(num));
            Ok(())
        }
        def => Err(format!("Wrong type for iload ({:x}), var_idx={} {:#?}", op, variable_to_load, def))
    }
}

fn execute_lload_n(op: u8, locals: &Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
{
    const ZEROTH_VARIABLE: u8 = 0x1E;
    let variable_to_load = op - ZEROTH_VARIABLE;
    match locals.get(variable_to_load as usize).ok_or("Missing local")? {
        LocalVariable::Long(num) => {
            stack.push_front(LocalVariable::Long(*num));
            Ok(())
        }
        def => Err(format!("Wrong type for iload ({:x}), var_idx={} {:#?}", op, variable_to_load, def))
    }
}

fn execute_fload_n(op: u8, locals: &Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
{
    const ZEROTH_VARIABLE: u8 = 0x22;
    let variable_to_load = op - ZEROTH_VARIABLE;
    match locals.get(variable_to_load as usize).ok_or(format!("Missing local at idx, {}", variable_to_load))? {
        LocalVariable::Float(num) => {
            stack.push_front(LocalVariable::Float(*num));
            Ok(())
        }
        def => Err(format!("Wrong type for fload ({:x}), var_idx={} {:#?}", op, variable_to_load, def))
    }
}

fn execute_aload(op: u8, locals: &Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
{
    const ZEROTH_VARIABLE: u8 = 0x2A;
    let variable_to_load = op - ZEROTH_VARIABLE;
    match locals.get(variable_to_load as usize).ok_or("Missing local")? {
        LocalVariable::Reference(addr) => {
            stack.push_front(LocalVariable::Reference(addr.clone()));
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

fn execute_load_integer_const(op: u8, stack: &mut Stack)
{
    const NEGATIVE_ONE_STARTING_POINT: i32 = 0x3;
    stack.push_front(LocalVariable::Int(op as i32 - NEGATIVE_ONE_STARTING_POINT))
}

fn execute_load_long_const(op: u8, stack: &mut Stack)
{
    const NEGATIVE_ONE_STARTING_POINT: i64 = 0x1e;
    stack.push_front(LocalVariable::Long(op as i64 - NEGATIVE_ONE_STARTING_POINT))
}

fn execute_load_float_const(op: u8, stack: &mut Stack)
{
    const NEGATIVE_ONE_STARTING_POINT: f32 = 0xb as f32;
    stack.push_front(LocalVariable::Float(op as f32 - NEGATIVE_ONE_STARTING_POINT))
}

fn execute_load_double_const(op: u8, stack: &mut Stack)
{
    match op {
        0xe => { stack.push_front(LocalVariable::Double(0.0)) }
        0xf => { stack.push_front(LocalVariable::Double(1.0)) }
        _ => {}
    }
}

fn execute_add(stack: &mut Stack) -> Result<(), String>
{
    let b = stack.pop_front().ok_or("ADD Missing stack variable 1")?;
    let a = stack.pop_front().ok_or("ADD Missing stack variable 2")?;

    stack.push_front(LocalVariable::sum(&a, &b)?);

    Ok(())
}


fn execute_sub(stack: &mut Stack) -> Result<(), String>
{
    let a = stack.pop_front().ok_or("SUB Missing stack variable 2")?;
    let b = stack.pop_front().ok_or("SUB Missing stack variable 1")?;

    stack.push_front(LocalVariable::sub(&a, &b)?);

    Ok(())
}

fn execute_bitwise(op: u8, stack: &mut Stack) -> Result<(), String>
{
    let a = stack.pop_front().ok_or("SUB Missing stack variable 2")?;
    let b = stack.pop_front().ok_or("SUB Missing stack variable 1")?;
    match op {
        0x7e => {
            stack.push_front(LocalVariable::and(&b, &a)?);
        }
        _ => { panic!("Wtf, op={:x}", op) }
    }
    Ok(())
}

fn execute_if_internal(op: u8, a: LocalVariable, b: LocalVariable) -> Result<bool, String>
{
    println!("Comparing {:?} with {:?}->{}", a, b, b == a);
    match op {
        0x9f | 0x99 | 0xa5 => { Ok(a == b) }
        0xa0 | 0x9a | 0xa6 => { Ok(a != b) }
        0xa1 | 0x9b => { Ok(a < b) }
        0xa2 | 0x9c => { Ok(a >= b) }
        0xa3 | 0x9d => { Ok(a > b) }
        0xa4 | 0x9e => { Ok(a <= b) }
        _ => Err(format!("Unexpected op {}", op))
    }
}

fn execute_if(op: u8, stack: &mut Stack) -> Result<bool, String> {
    let a = stack.pop_front().ok_or("IF Missing stack variable 1")?;
    if op >= 0x99 && op <= 0x9e {
        let zero = if let LocalVariable::Boolean(_) = a { LocalVariable::Boolean(false) } else { LocalVariable::Int(0) };
        return execute_if_internal(op, a, zero);
    }
    let b = stack.pop_front().ok_or("IF Missing stack variable 2")?;
    return execute_if_internal(op, b, a);
}

fn execute_if_null(op: u8, stack: &mut Stack) -> Result<bool, String> {
    let a = stack.pop_front().ok_or("IF-NULL Missing stack variable 1")?;
    if let LocalVariable::Reference(addr) = a {
        match op {
            0xC6 => {
                Ok(ReferenceKind::Null() == addr)
            }
            0xC7 => {
                Ok(ReferenceKind::Null() != addr)
            }
            _ => Err(format!("Unexpected op {}", op))
        }
    } else {
        Err(format!("Wrong type {:?}", a))
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
        if char == '(' || char == '[' {
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
    pub fn new(vm: &Rc<VM>, class: &Rc<Class>, method_name: &str, local_variables: Vec<LocalVariable>, method_type: &str) -> Result<Frame, String>
    {
        let exec_method = |f: &mut Frame, m: &Field| -> Result<Frame, String> {
            for attrib in &m.attributes {
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
                    return Ok(f.clone());
                }
            }
            return Err(format!("Couldn't find code for method {}: {} ({:?}/{:?})", method_name, method_type, &m.attributes, &m.flags));
        };
        println!("Will try to create a frame for {}::{}{} called with: {:?}", class.name, method_name, method_type, local_variables);
        let mut f = Frame {
            ip: 0,
            code: vec![],
            locals: vec![],
            stack: VecDeque::new(),
            class: class.clone(),
            method_name: method_name.to_string(),
            method_type: method_type.to_string(),
            vm: vm.clone(),
            bootstrap_methods: vec![],
            will_execute_native_method: false,
        };

        if vm.native_methods.borrow().contains_key(&VM::make_native_method_name(&class.name, method_name)) {
            dbg!("Native method {}", method_name);
            for arg in local_variables.iter() {
                f.locals.push(arg.clone());
            }
            f.will_execute_native_method = true;
            return Ok(f);
        }

        for method in &class.methods
        {
            println!("Candidate method: '{}' -> '{}' - I am looking for '{}' -> '{}'", method.name, method.descriptor, method_name, method_type);
            if method.name.eq(method_name) && method.descriptor.eq(method_type) {
                return exec_method(&mut f, method);
            }
        }

        let maybe_super_class = vm.get_class(&class.super_class);
        if let Some(super_class) = maybe_super_class
        {
            return Frame::new(&vm, &super_class, method_name, local_variables, method_type);
        }

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


        Err(format!("Method not found: {} {}", method_name, method_type))
    }

    pub(crate) fn resolve_string(&self, idx: usize) -> String
    {
        return match &self.class.constant_pool[idx] {
            ConstantPool::JvmString(str) => { str.to_string() }
            ConstantPool::ClassIndex(recursive_idx) => {
                self.resolve_string((*recursive_idx) as usize)
            }
            def => {
                dbg!("Wtf Requested index: {} which is instead a {:#?}", idx, def);
                "".to_string()
            }
        };
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
                                Ok((self.resolve_string(*class_idx as usize), self.find_name_and_type(*nat)?))
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
                Ok((self.resolve_string(*class_idx as usize),
                    self.find_name_and_type(*method_idx)?))
            }
            ConstantPool::FieldRef(class_idx, method_idx) => {
                Ok((self.resolve_string(*class_idx as usize),
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
                Ok((self.resolve_string(*name_idx as usize),
                    self.resolve_string(*type_idx as usize)))
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

    pub fn get_u16(&mut self) -> u16
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
    pub fn get_u8(&mut self) -> u8
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
            return method(&self.class, &self, self.locals.as_slice());
        }
        loop {
            let curr_ip = self.ip;
            let op = self.code[curr_ip as usize];
            dbg!(&self.class.name, &self.method_name, &self.method_type, op, curr_ip, &self.stack, &self.locals);
            match op {
                0x0 /* nop */ => {}
                0x1 /* aload_null */ => { self.stack.push_front(LocalVariable::Reference(ReferenceKind::Null())) }
                0x2 | 0x3 | 0x4 | 0x5 | 0x6 | 0x7 | 0x8 /* iconst_<d> */ => {
                    execute_load_integer_const(op, &mut self.stack);
                }
                0xb | 0xc | 0xd /* fload_<d> */ => {
                    execute_load_float_const(op, &mut self.stack);
                }
                0xe | 0xf /* dload_<d> */ => {
                    execute_load_double_const(op, &mut self.stack);
                }
                0x15 /* iload */ => {
                    let index = self.get_u8() as usize;
                    let value_var = self.locals[index].clone();
                    if let LocalVariable::Int(i) = &value_var {
                        println!("Found variable of the right type idx={}, variable found={:?}", index, &value_var);
                        self.stack.push_front(value_var);
                        Ok(())
                    } else {
                        Err(format!("Could not find local variable of the right type, idx={}, variable found={:?}", index, &value_var))
                    }?;
                }
                0x17 /* fload */ => {
                    let index = self.get_u8() as usize;
                    let value_var = self.locals[index].clone();
                    if let LocalVariable::Float(i) = &value_var {
                        println!("Found variable of the right type idx={}, variable found={:?}", index, &value_var);
                        self.stack.push_front(value_var);
                        Ok(())
                    } else {
                        Err(format!("Could not find local variable of the right type, idx={}, variable found={:?}", index, &value_var))
                    }?;
                }
                0x1A | 0x1B | 0x1C | 0x1D/* iload_<n> */ => {
                    execute_iload_n(op, &self.locals, &mut self.stack)?
                }
                0x22 | 0x23 | 0x24 | 0x25/* iload_<n> */ => {
                    execute_fload_n(op, &self.locals, &mut self.stack)?
                }
                0x1e | 0x1f | 0x20 | 0x21 /* lload_<d> */ => {
                    execute_load_long_const(op, &mut self.stack);
                }
                0x12 | 0x13 /*ldc*/ => {
                    let idx = match op {
                        0x12 => { self.get_u8() as usize }
                        0x13 => { self.get_u16() as usize }
                        _ => { panic!("Can't happen"); }
                    };
                    match self.class.constant_pool.get(idx).ok_or(format!("Couldn't find item in cp at {}", idx))? {
                        ConstantPool::Integer(num) => { self.stack.push_front(LocalVariable::Int(*num)); }
                        ConstantPool::Float(f) => { self.stack.push_front(LocalVariable::Float(*f)) }
                        ConstantPool::Long(num) => { self.stack.push_front(LocalVariable::Long(*num)); }
                        ConstantPool::Double(f) => { self.stack.push_front(LocalVariable::Double(*f)) }
                        ConstantPool::StringIndex(_s_idx) => {
                            self.stack.push_front(LocalVariable::Reference(ReferenceKind::ObjectReference(self.vm.new_object("java/lang/String"))));
                        }
                        ConstantPool::ClassIndex(idx) => {
                            self.stack.push_front(LocalVariable::Reference(ReferenceKind::ClassReference(self.resolve_string(*idx as usize))))
                        }
                        def => return Err(format!("Unexpected item={:?}", def))
                    };
                }
                0x74 /* ineg */ => {
                    let value_var = self.stack.pop_front().ok_or("Empty stack when finding value")?;
                    if let LocalVariable::Int(i) = value_var
                    {
                        self.stack.push_front(LocalVariable::Int(i.neg()));
                    }
                }
                0x7e /* iand */ => {
                    execute_bitwise(op, &mut self.stack)?
                }
                0x84 /* iinc */ => {
                    let index = self.get_u8() as usize;
                    let constant = self.get_u8() as i8 as i32;
                    let value_var = self.locals[index].clone();
                    if let LocalVariable::Int(i) = value_var {
                        self.locals[index] = LocalVariable::Int(i + constant);
                        Ok(())
                    } else {
                        Err(format!("Variable={:?} at index={} could not be increased by constant={}", value_var, index, constant))
                    }?
                }
                0x19 /* aload */ => {
                    let index = self.get_u8() as usize;
                    let value_var = self.locals[index].clone();
                    if let LocalVariable::Reference(i) = &value_var {
                        self.stack.push_front(value_var);
                    }
                }
                0x2A | 0x2B | 0x2C | 0x2D/* aload_<n> */ => {
                    execute_aload(op, &self.locals, &mut self.stack)?;
                    println!("Stack is {:?}", self.stack);
                }
                0x10 => /*bipush */ {
                    let byte = self.get_u8() as i8 as i32;
                    println!("This is the number {}", byte);
                    self.stack.push_front(LocalVariable::Int(byte));
                }
                0x11 => /* sipush */ {
                    let short = self.get_u16() as i32;
                    self.stack.push_front(LocalVariable::Int(short));
                }
                0x36 => {
                    let index = self.get_u8() as usize;
                    let value_var = self.stack.pop_front().ok_or("Empty stack finding value")?;
                    if let LocalVariable::Int(i) = value_var {
                        self.locals[index] = value_var;
                    }
                }
                0x3b | 0x3c | 0x3d | 0x3e => {
                    execute_istore(op, &mut self.locals, &mut self.stack)?
                }
                0x3a /* astore */ => {
                    let idx = self.get_u8();
                    let obj_ref = self.stack.pop_front().ok_or("No value for astore")?;
                    self.locals[idx as usize] = obj_ref;

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
                0x68 => {
                    let a_var = self.stack.pop_front().ok_or("Empty stack finding a")?;
                    let b_var = self.stack.pop_front().ok_or("Empty stack finding b")?;
                    if let LocalVariable::Int(a) = a_var {
                        if let LocalVariable::Int(b) = b_var {
                            self.stack.push_front(LocalVariable::Int(a * b))
                        }
                    }
                }
                0x6c => {
                    let a_var = self.stack.pop_front().ok_or("Empty stack finding a")?;
                    let b_var = self.stack.pop_front().ok_or("Empty stack finding b")?;
                    if let LocalVariable::Int(a) = a_var {
                        if let LocalVariable::Int(b) = b_var {
                            self.stack.push_front(LocalVariable::Int(b / a))
                        }
                    } else {
                        Err("Wuth")?
                    }
                }
                0x78 /* ishl */ => {
                    let value_var = self.stack.pop_front().ok_or("Empty stack finding value")?;
                    let shift_var = self.stack.pop_front().ok_or("Empty stack finding shift")?;
                    if let LocalVariable::Int(value) = value_var {
                        if let LocalVariable::Int(shift) = shift_var {
                            self.stack.push_front(LocalVariable::Int(value << shift));
                            Ok(())
                        } else {
                            Err("ISHL Expected integer SHIFT")
                        }?;
                        Ok(())
                    } else {
                        Err("ISHL Expected Integer VALUE")
                    }?
                }
                0x7a /* ishr */ => {
                    let shift_var = self.stack.pop_front().ok_or("Empty stack finding shift")?.to_lv_int();
                    let value_var = self.stack.pop_front().ok_or("Empty stack finding value")?.to_lv_int();
                    if let LocalVariable::Int(value) = value_var {
                        if let LocalVariable::Int(shift) = shift_var {
                            self.stack.push_front(LocalVariable::Int(value >> shift));
                            Ok(())
                        } else {
                            Err("LSHR Expected integer SHIFT")
                        }?;
                        Ok(())
                    } else {
                        Err("LSHR Expected Integer SHIFT")
                    }?
                }
                0x7c /* iushr */ => {
                    let shift_var = self.stack.pop_front().ok_or("Empty stack finding shift")?.to_lv_int();
                    let value_var = self.stack.pop_front().ok_or("Empty stack finding value")?.to_lv_int();
                    if let LocalVariable::Int(value) = value_var {
                        if let LocalVariable::Int(shift) = shift_var {
                            self.stack.push_front(LocalVariable::Int(value >> shift));
                            Ok(())
                        } else {
                            Err("LSHR Expected integer SHIFT")
                        }?;
                        Ok(())
                    } else {
                        Err("LSHR Expected Integer SHIFT")
                    }?
                }
                0x7d /* lushr */ => {
                    let value_var = self.stack.pop_front().ok_or("Empty stack finding value")?;
                    let shift_var = self.stack.pop_front().ok_or("Empty stack finding shift")?.to_lv_int();
                    if let LocalVariable::Long(value) = value_var {
                        if let LocalVariable::Int(shift) = shift_var {
                            self.stack.push_front(LocalVariable::Long(value >> shift));
                            Ok(())
                        } else {
                            Err("LUSHR Expected integer SHIFT")
                        }?;
                        Ok(())
                    } else {
                        Err("LUSHR Expected Long VALUE")
                    }?
                }
                0x82 /* ixor */ => {
                    let value2 = self.stack.pop_front().ok_or("ixor Empty stack finding value")?;
                    let value1 = self.stack.pop_front().ok_or("ixor Empty stack finding shift")?;
                    if let LocalVariable::Int(v1) = value1 {
                        if let LocalVariable::Int(v2) = value2 {
                            self.stack.push_front(LocalVariable::Int(v1 ^ v2));
                            Ok(())
                        } else {
                            Err("IXOR Expected integer VALUE2")
                        }?;
                        Ok(())
                    } else {
                        Err("IXOR Expected integer VALUE1")
                    }?
                }
                0x91 | 0x92 | 0x93 | 0x85 | 0x86 | 0x87 /*iconv*/ => {
                    execute_iconv(op, &mut self.stack)?
                }
                0xa7 /*goto*/ => {
                    let idx = self.get_u16() as i16;
                    println!("GOTO: Will jump from={}(curr_ip {}) offset={} -> {}", self.ip, curr_ip as i16, idx, idx + curr_ip as i16);
                    self.ip = (idx + curr_ip as i16 - 1) as u32;
                    dbg!(self.ip);
                }
                0x9f | 0xa0 | 0xa1 | 0xa2 | 0xa3 | 0xa4 | 0x99 | 0x9a | 0x9b | 0x9c | 0x9d | 0x9e | 0xa5 | 0xa6 => /*if_i<cmp>*/{
                    let idx = self.get_u16() as i16 as i32;
                    if execute_if(op, &mut self.stack)? {
                        println!("IF Will jump from={} offset={}, curr_ip={}, jump_to={}", self.ip, idx, curr_ip, curr_ip + idx as u32 - 1);
                        self.ip = (idx + curr_ip as i32) as u32 - 1;
                    }
                }
                0x95 | 0x96 => /*fcmpg, fcmpl*/{
                    let value2 = self.stack.pop_front().ok_or("fcmp Could not find variable 2")?;

                    let value1 = self.stack.pop_front().ok_or("fcmp Could not find variable 1")?;
                    if let LocalVariable::Float(var1) = value1 {
                        if let LocalVariable::Float(var2) = value2 {
                            if var1 > var2
                            {
                                self.stack.push_front(LocalVariable::Int(1))
                            } else if var1 == var2
                            {
                                self.stack.push_front(LocalVariable::Int(0))
                            } else {
                                self.stack.push_front(LocalVariable::Int(-1))
                            }
                            Ok(())
                        } else {
                            Err(format!("Variable2 was not a float {:?}", value2))
                        }
                    } else {
                        Err(format!("Variable1 was not a float {:?}", value1))
                    }?
                }
                0xba /*invokeVirtual */ => {
                    let idx = self.get_u16();
                    let _ = self.get_u16();
                    let (bootstrap_idx, (method_name, method_type)) = self.find_invoke_dynamic(idx)?;
                    let method_handle = self.bootstrap_methods.get(bootstrap_idx as usize).ok_or("Non existing bootstrap method")?;
                    let method_ref = self.resolve_method_handle_ref(method_handle.method_ref)?;

                    let class_name_ref = &method_ref.0;
                    let mut new_stack = vec![];
                    let class = &self.vm.get_class(class_name_ref)
                        .ok_or(format!("Could not find class {:#?}", class_name_ref).to_string())?
                        .clone();
                    let (argc, ret) = parse_type(&method_type)?;
                    for _ in 0..argc {
                        new_stack.push(self.stack.pop_front().ok_or(format!("Empty stack when calling new method, {}", ret))?);
                    }
                    new_stack.reverse();
                    let new_frame = Frame::new(&self.vm, &class, &method_name, new_stack.clone(), &method_type)?;
                    let mut n_frame = new_frame;
                    let ret = n_frame.exec()?;
                    match ret {
                        LocalVariable::Void() => {}
                        def => {
                            self.locals.push(def)
                        }
                    };
                }
                0xB2 /*getstatic*/ => {
                    let idx = self.get_u16();
                    let (class_name, (field_name, field_type)) = self.find_method_or_field(idx)?;
                    let class_name_ref = &class_name;
                    println!("Finding {}:{}({})", class_name, field_name, field_type);
                    let instance = self.vm.new_object(class_name_ref);
                    let new_object = self.vm.objects.borrow()
                        .get(instance)
                        .ok_or("Couldn't find the object I just created")?.clone();
                    println!("These are the fields objects: {:?} {:?}", new_object, new_object.fields.borrow());
                    let new_field = new_object.fields.borrow()
                        .get(&field_name)
                        .ok_or(format!("Non-existing field, field={}", field_name))?.clone();
                    self.stack.push_front(new_field);
                }
                0xB4 | 0xB5 | 0xB6 | 0xB7 | 0xB8 /* getfield, putfield, invokeVirtual, invokeSpecial, invokeStatic */ => {
                    let idx = self.get_u16();
                    let (class_name, (method_name, method_type)) = self.find_method_or_field(idx)?;

                    let class_name_ref = &class_name;
                    let mut new_stack = vec![];
                    let class = &self.vm.get_class(class_name_ref)
                        .ok_or(format!("Could not find class {:#?}", class_name_ref).to_string())?
                        .clone();
                    let (argc, ret) = parse_type(&method_type)?;

                    match op {
                        0xB4 => {
                            let class_ref = self.stack.pop_front().ok_or("Empty stack when getting field")?;
                            match class_ref {
                                LocalVariable::Reference(ref_kind) => {
                                    if let ReferenceKind::ObjectReference(obj_ref) = ref_kind {
                                        let obj_ref = self.vm.objects
                                            .borrow()
                                            .get(obj_ref as usize)
                                            .ok_or("Class did not exist")?.clone();
                                        let field_val = self.vm.find_field(&obj_ref, &method_name, &method_type)?;
                                        println!("This is what I found: {}:{:?} out of {:?}:{:?} {:?}", &method_name, &field_val, &ref_kind, &obj_ref, obj_ref.fields.borrow());
                                        self.stack.push_front(field_val);
                                        Ok(())
                                    } else {
                                        Err(format!("Unexpected type {:?}", ref_kind))
                                    }
                                }
                                _ => Err(format!("Unexpected type {:?}", class_ref))
                            }?
                        }
                        0xB5 => {
                            let field_val = self.stack.pop_front().ok_or("Empty stack when retrieving field")?;
                            let class_ref = self.stack.pop_front().ok_or("Empty stack when putting field")?;
                            match class_ref {
                                LocalVariable::Reference(obj_ref) => {
                                    if let ReferenceKind::ObjectReference(obj_ref) = obj_ref {
                                        println!("Inserted {}{:?} into {:?}", &method_name, &field_val, obj_ref);
                                        self.vm.objects
                                            .borrow()
                                            .get(obj_ref)
                                            .ok_or("Object did not exist")?.borrow_mut()
                                            .put_field(&method_name, field_val);
                                        Ok(())
                                    } else {
                                        Err(format!("Unexpected type {:?}", obj_ref))
                                    }
                                }
                                _ => Err(format!("Unexpected type {:?}", class_ref))
                            }?
                        }
                        0xB6 /* invokeVirtual */ => {
                            for _ in 0..=argc {
                                new_stack.push(self.stack.pop_front().ok_or(format!("Empty stack when calling new method, {} {} {}", method_name, method_type, ret))?);
                            }
                            new_stack.reverse();
                            let ret = Frame::new(&self.vm, &class, &method_name, new_stack.clone(), &method_type)?.exec()?;

                            match ret {
                                LocalVariable::Void() => {}
                                def => {
                                    self.stack.push_front(def)
                                }
                            };
                        }
                        0xB7 /* invokeSpecial */ => {
                            for _ in 0..=argc {
                                new_stack.push(self.stack.pop_front().ok_or(format!("Empty stack when calling new method, {} {} {}", method_name, method_type, ret))?);
                            }
                            new_stack.reverse();
                            if class_name_ref != &self.class.name
                            {
                                println!("This is another class name - {} -> {}", class_name_ref, &self.class.name);
                                let this_ref = &new_stack[0];
                                if let LocalVariable::Reference(ObjectReference(this_idx)) = this_ref {
                                    let this_instance = &self.vm.objects.borrow().get(*this_idx).unwrap().clone();
                                    if this_instance.get_super_instance().is_none()
                                    {
                                        let super_instance_idx = self.vm.new_object(class_name_ref);
                                        this_instance.put_super_instance(super_instance_idx);
                                    }
                                    println!("New stack[0] was: {:?}->{:?}", &new_stack[0], LocalVariable::Reference(ObjectReference(this_instance.get_super_instance().unwrap())));
                                    new_stack[0] = LocalVariable::Reference(ObjectReference(this_instance.get_super_instance().unwrap()));
                                } else {
                                    panic!("Wtf")
                                }
                                //new_stack[0] = ;
                            }
                            let ret = Frame::new(&self.vm, &class, &method_name, new_stack.clone(), &method_type)?.exec()?;

                            match ret {
                                LocalVariable::Void() => {}
                                def => {
                                    self.stack.push_front(def)
                                }
                            };
                        }
                        0xB8 /* invokeStatic */ => {
                            for _ in 1..=argc {
                                new_stack.push(self.stack.pop_front().ok_or("Empty stack when calling new method")?);
                            }

                            new_stack.reverse();
                            let new_frame = Frame::new(&self.vm, &class, &method_name, new_stack.clone(), &method_type)?;
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
                0xAC /* ireturn */ | 0xB0  /* areturn */ => {
                    let ret = self.stack.pop_front()
                        .ok_or("Empty stack when returning")?;
                    return Ok(ret);
                }
                0xB1 /* return */ => {
                    return Ok(LocalVariable::Void());
                }

                0xBB /* new */ => {
                    let idx = self.get_u16();
                    let class_name = self.resolve_string(idx as usize);

                    self.stack.push_front(LocalVariable::Reference(ReferenceKind::ObjectReference(self.vm.new_object(&class_name))))
                }
                0xC6 | 0xC7 /*if(non)null */ => {
                    let jump_idx = self.get_u16() as i16;
                    if execute_if_null(op, &mut self.stack)? {
                        println!("Will jump from {} offset= {}", self.ip, jump_idx);
                        self.ip = curr_ip + jump_idx as u32 - 1;
                    }
                }
                0x37 /* lstore */ => {
                    let idx = self.get_u8();
                    let store_var = self.stack.pop_front().ok_or("No variable to store")?;
                    if let LocalVariable::Long(count) = store_var {
                        self.locals[idx as usize] = store_var;
                    }
                }
                // Begin array area
                0xBC /*newarray*/ => {
                    let count_var = self.stack.pop_front().ok_or("No count for array")?;
                    if let LocalVariable::Int(count) = count_var {
                        let arr_type = self.get_u8();
                        self.stack.push_front(LocalVariable::Reference(ReferenceKind::ArrayReference(self.vm.new_array(arr_type, count as usize))))
                    } else {
                        panic!("Wtf {:?}", count_var)
                    }
                }
                0xBE /*arraylenght */ => {
                    let array_ref = self.stack.pop_front().ok_or("No array ref")?;
                    if let LocalVariable::Reference(ReferenceKind::ArrayReference(idx)) = array_ref {
                        if let Some(arr) = self.vm.arrays.borrow().get(idx) {
                            self.stack.push_front(LocalVariable::Int(arr.borrow().len() as i32));
                        }
                    }
                    if let LocalVariable::Reference(ReferenceKind::Null()) = array_ref {
                        self.stack.push_front(LocalVariable::Int(0));
                    }
                }
                0x32 | 0x33 | 0x34 | 0x35 /* aaload, baload, caload, saload */ => {
                    let index_var = self.stack.pop_front().ok_or("No index for array")?;
                    let array_ref = self.stack.pop_front().ok_or("No array_ref")?;
                    if let LocalVariable::Int(idx) = index_var {
                        if let LocalVariable::Reference(ReferenceKind::ArrayReference(arr_idx)) = array_ref {
                            let item = self.vm.arrays.borrow()[arr_idx].borrow()[idx as usize].clone();
                            match item {
                                LocalVariable::Reference(_) => {
                                    if op == 0x32 {
                                        self.stack.push_front(item);
                                        Ok(())
                                    } else {
                                        Err(format!("Unexpected op={:x} for ref load={:?}", op, &item))
                                    }
                                }
                                LocalVariable::Boolean(_) | LocalVariable::Byte(_) => {
                                    if op == 0x33 {
                                        self.stack.push_front(item.to_lv_int());
                                        Ok(())
                                    } else {
                                        Err(format!("Unexpected op={:x} for boolean/byte load={:?}", op, &item))
                                    }
                                }
                                LocalVariable::Char(_) => {
                                    if op == 0x34 {
                                        self.stack.push_front(item.to_lv_int());
                                        Ok(())
                                    } else {
                                        Err(format!("Unexpected op={:x} for char load={:?}", op, &item))
                                    }
                                }

                                LocalVariable::Short(_) => {
                                    if op == 0x35 {
                                        self.stack.push_front(item.to_lv_int());
                                        Ok(())
                                    } else {
                                        Err(format!("Unexpected op={:x} for short load={:?}", op, &item))
                                    }
                                }
                                _ => { Err(format!("Unexpected item={:?} for load, currently implemented are baload, caload and saload", &item)) }
                            }?;
                            Ok(())
                        } else {
                            Err("Couldn't find an array")
                        }?
                    }
                }
                0x52 | 0x53 | 0x54 | 0x55 /* <d,a,b,c>astore */ => {
                    let value_var = self.stack.pop_front().ok_or("No value for array")?;
                    let index_var = self.stack.pop_front().ok_or("No index for array")?;
                    let array_ref = self.stack.pop_front().ok_or("No array_ref")?;

                    let index: usize = match index_var {
                        LocalVariable::Int(index) => { Ok(index as usize) }
                        _ => Err("Unexpected index")
                    }?;

                    let arr = match array_ref {
                        LocalVariable::Reference(ReferenceKind::ArrayReference(a)) => { Ok(a) }
                        _ => Err("Unexpected array ref")
                    }?;
                    match op {
                        0x42 => {
                            let value = match value_var {
                                LocalVariable::Double(value) => { Ok(value) }
                                _ => Err("Unexpected value")
                            }?;
                            self.vm.set_array_element(arr, index, LocalVariable::Double(value));
                        }
                        0x53 => {
                            let value = match value_var {
                                LocalVariable::Reference(value) => { Ok(value) }
                                _ => Err("Unexpected value")
                            }?;
                            self.vm.set_array_element(arr, index, LocalVariable::Reference(value));
                        }
                        0x54 /* bastore */ => {
                            let value = match value_var {
                                LocalVariable::Byte(value) => { Ok(value) }
                                LocalVariable::Boolean(value) => { Ok(value as u8) }
                                def => Err(format!("Unexpected value={:?}", def))
                            }?;
                            println!("Hello I am about to set {} to {}", index, value);
                            self.vm.set_array_element(arr, index, LocalVariable::Char(value as u8 as char));
                        }
                        0x55 /* castore */ => {
                            let value = match value_var {
                                LocalVariable::Int(value) => { Ok(value) }
                                _ => Err("Unexpected value")
                            }?;

                            self.vm.set_array_element(arr, index, LocalVariable::Byte(value as u8));
                        }
                        _ => { panic!("Impossible"); }
                    }
                }

                0xC2 /* monitorenter */ => {
                    self.stack.pop_front().ok_or("Empty stack when popping")?;
                    println!("Monitor entered");
                }
                0xc0 /* checkcast */ => {
                    let cp = self.get_u16();
                    let thing = &self.class.constant_pool[cp as usize];
                    if let ConstantPool::ClassIndex(t) = thing {

                    }
                    println!("Thing is {:?}", thing);
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
    use crate::frame::{execute_add, execute_iload_n, execute_load_const, execute_sub, Stack};

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
            assert!(execute_iload_n(0x1A, &locals, &mut stack).is_ok());
            assert!(!stack.is_empty());
            assert_eq!(1, stack.len());
            assert_eq!(LocalVariable::Int(3), stack.pop_front().unwrap());
        }
        {
            assert!(execute_iload_n(0x1B, &locals, &mut stack).is_err());
            assert!(stack.is_empty());
            assert_eq!(0, stack.len());
        }
        {
            let locals = vec![LocalVariable::Int(3), LocalVariable::Boolean(true)];
            assert!(execute_iload_n(0x1B, &locals, &mut stack).is_ok());
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