use std::borrow::Borrow;
use std::collections::VecDeque;
use std::ops::Neg;
use anyhow::anyhow;

use crate::rustvm::class::ClassRef;
use crate::rustvm::enums::{BootstrapMethod, ConstantPool, Field, LocalVariable, ReferenceKind};
use crate::rustvm::enums::LocalVariable::Void;
use crate::rustvm::enums::ReferenceKind::ObjectReference;
use crate::rustvm::executors::*;
use crate::rustvm::vm::VM;

pub(crate) type Stack = VecDeque<LocalVariable>;


#[derive(Debug)]
pub(crate) struct Frame {
    ip: u32,
    code: Vec<u8>,
    locals: Vec<LocalVariable>,
    pub(crate) stack: Stack,
    pub(crate) class: ClassRef,
    method_name: String,
    method_type: String,
    bootstrap_methods: Vec<BootstrapMethod>,
    pub(crate) will_execute_native_method: bool,
}


#[derive(Debug)]
pub(crate) enum ExecutionResult
{
    FunctionCallResult(LocalVariable),
    Invoke(Frame)
}

fn parse_type(prototype: &str) -> Result<(usize, String), anyhow::Error>
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


fn read_u16_from_vec(data: &Vec<u8>, starting_point: usize) -> u16 {
    let mut d = [0u8; 2];
    for (idx, a) in data[starting_point..starting_point + 2].iter().enumerate() {
        d[idx] = *a;
    }
    u16::from_be_bytes(d)
}

impl Frame {
    pub fn new(class: ClassRef, method_name: &str, local_variables: Vec<LocalVariable>, method_type: &str, vm: &mut VM) -> Result<Frame, anyhow::Error>
    {
        let exec_method = |f: &mut Frame, m: &Field| -> Result<(), anyhow::Error> {
            for attrib in &m.attributes {

                if attrib.name.as_str() == "Code" && attrib.data.len() > 8 {
                    let mut d = [0u8; 2];
                    for (idx, a) in attrib.data[2..4].iter().enumerate() {
                        d[idx] = *a;
                    }
                    let max_locals = u16::from_be_bytes(d);
                    f.code = attrib.data[8..].to_vec();
                    f.locals = vec![LocalVariable::Void(); max_locals as usize + 1];
                    for (idx, arg) in local_variables.iter().enumerate() {
                        f.locals[idx] = arg.clone();
                    }
                    return Ok(());
                }
            }
            return Err(anyhow!("Couldn't find code for method {}: {} ({:?}/{:?})", method_name, method_type, &m.attributes, &m.flags));
        };
        let borrowed_name = &class.name;
        println!("Will try to create a frame for {}::{}{} called with: {:?}", borrowed_name, method_name, method_type, local_variables);
        let mut f = Frame {
            ip: 0,
            code: vec![],
            locals: vec![],
            stack: VecDeque::new(),
            class: class.clone(),
            method_name: method_name.to_string(),
            method_type: method_type.to_string(),
            bootstrap_methods: vec![],
            will_execute_native_method: false,
        };

        if vm.native_methods.contains_key(&VM::make_native_method_name(&borrowed_name, method_name)) {
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
                exec_method(&mut f, method)?;
                return Ok(f);
            }
        }

        if !class.super_class.is_empty()
        {
            let super_class = vm.get_class(&class.super_class)?;
            return Frame::new(super_class.clone(), method_name, local_variables, method_type, vm);
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
                    let mut args = vec![0u16; num_arguments as usize];
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


        Err(anyhow!("Method not found: {} {}", method_name, method_type))
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

    pub fn exec_native(&mut self, vm: &mut VM) -> Result<ExecutionResult, anyhow::Error>
    {
        let class = self.class.clone();
        let locals = &self.locals.clone();
        let method = vm.native_methods.get(&VM::make_native_method_name(&self.class.name, &self.method_name)).ok_or(anyhow::Error::msg("This should never be possible"))?.clone();
        return Ok(ExecutionResult::FunctionCallResult(method(class, self, locals, vm)?));
    }
    pub fn exec(&mut self, vm: &mut VM) -> Result<ExecutionResult, anyhow::Error>
    {
        loop {
            let curr_ip = self.ip;
            let op = self.code[curr_ip as usize];
            println!("Class={}, Method={}, Type={}, op={:#X}, ip={}, stack={:#?}, locals={:#?}", &self.class.name, &self.method_name, &self.method_type, op, curr_ip, &self.stack, &self.locals);
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
                    if let LocalVariable::Int(_i) = &value_var {
                        println!("Found variable of the right type idx={}, variable found={:?}", index, &value_var);
                        self.stack.push_front(value_var);
                        Ok(())
                    } else {
                        Err(anyhow!("Could not find local variable of the right type, idx={}, variable found={:?}", index, &value_var))
                    }?;
                }
                0x17 /* fload */ => {
                    let index = self.get_u8() as usize;
                    let value_var = self.locals[index].clone();
                    if let LocalVariable::Float(_) = &value_var {
                        println!("Found variable of the right type idx={}, variable found={:?}", index, &value_var);
                        self.stack.push_front(value_var);
                        Ok(())
                    } else {
                        Err(anyhow!("Could not find local variable of the right type, idx={}, variable found={:?}", index, &value_var))
                    }?;
                }
                0x1A | 0x1B | 0x1C | 0x1D/* iload_<n> */ => {
                    execute_iload_n(op, &self.locals, &mut self.stack)?
                }
                0x1E | 0x1F | 0x20 | 0x21 /* lload_<n> */ => {
                    execute_lload_n(op, &self.locals, &mut self.stack)?
                }
                0x22 | 0x23 | 0x24 | 0x25/* iload_<n> */ => {
                    execute_fload_n(op, &self.locals, &mut self.stack)?
                }
                0x9 | 0xa /* lconst_<i> */ => {
                    execute_load_long_const(op, &mut self.stack);
                }
                0x12 | 0x13 /*ldc*/ => {
                    let idx = match op {
                        0x12 => { Ok(self.get_u8() as usize) }
                        0x13 => { Ok(self.get_u16() as usize) }
                        _ => { Err(anyhow!("Can't happen")) }
                    }?;
                    match &self.class.constant_pool.get(idx).ok_or(anyhow!("Couldn't find item in cp at {}", idx))? {
                        ConstantPool::Integer(num) => { self.stack.push_front(LocalVariable::Int(*num)); }
                        ConstantPool::Float(f) => { self.stack.push_front(LocalVariable::Float(*f)) }
                        ConstantPool::Long(num) => { self.stack.push_front(LocalVariable::Long(*num)); }
                        ConstantPool::Double(f) => { self.stack.push_front(LocalVariable::Double(*f)) }
                        ConstantPool::StringIndex(_s_idx) => {
                            self.stack.push_front(LocalVariable::Reference(ObjectReference(vm.new_object("java/lang/String")?)));
                        }
                        ConstantPool::ClassIndex(idx) => {
                            self.stack.push_front(LocalVariable::Reference(ReferenceKind::ClassReference(self.class.resolve_string(*idx as usize))))
                        }
                        def => return Err(anyhow!("Unexpected item={:?}", def))
                    };
                }
                0x74 /* ineg */ => {
                    let value_var = self.stack.pop_front().ok_or(anyhow!("Empty stack when finding value"))?;
                    if let LocalVariable::Int(i) = value_var
                    {
                        self.stack.push_front(LocalVariable::Int(i.neg()));
                    }
                }
                0x7e /* iand */ => {
                    execute_bitwise(op, &mut self.stack)?
                }
                0x79 /* ishl */ => {
                    execute_bitwise(op, &mut self.stack)?;
                }
                0x84 /* iinc */ => {
                    let index = self.get_u8() as usize;
                    let constant = self.get_u8() as i8 as i32;
                    let value_var = self.locals[index].clone();
                    if let LocalVariable::Int(i) = value_var {
                        self.locals[index] = LocalVariable::Int(i + constant);
                        Ok(())
                    } else {
                        Err(anyhow::Error::msg(format!("Variable={:?} at index={} could not be increased by constant={}", value_var, index, constant)))
                    }?
                }
                0x19 /* aload */ => {
                    let index = self.get_u8() as usize;
                    let value_var = self.locals[index].clone();
                    if let LocalVariable::Reference(_) = &value_var {
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
                    let value_var = self.stack.pop_front().ok_or(anyhow::Error::msg("Empty stack finding value"))?;
                    if let LocalVariable::Int(_) = value_var {
                        self.locals[index] = value_var;
                    }
                }
                0x3b | 0x3c | 0x3d | 0x3e => {
                    execute_istore(op, &mut self.locals, &mut self.stack)?
                }
                0x3a /* astore */ => {
                    let idx = self.get_u8();
                    let obj_ref = self.stack.pop_front().ok_or(anyhow::Error::msg("No value for astore"))?;
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
                    let a_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding a"))?;
                    let b_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding b"))?;
                    if let LocalVariable::Int(a) = a_var {
                        if let LocalVariable::Int(b) = b_var {
                            self.stack.push_front(LocalVariable::Int(a * b))
                        }
                    }
                }
                0x6c => {
                    let a_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding a"))?;
                    let b_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding b"))?;
                    if let LocalVariable::Int(a) = a_var {
                        if let LocalVariable::Int(b) = b_var {
                            self.stack.push_front(LocalVariable::Int(b / a))
                        }
                    } else {
                        Err(anyhow!("Did not find integers for division"))?
                    }
                }
                0x78 /* ishl */ => {
                    let value_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding value"))?;
                    let shift_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding shift"))?;
                    if let LocalVariable::Int(value) = value_var {
                        if let LocalVariable::Int(shift) = shift_var {
                            self.stack.push_front(LocalVariable::Int(value << shift));
                            Ok(())
                        } else {
                            Err(anyhow!("ISHL Expected integer SHIFT"))
                        }?;
                        Ok(())
                    } else {
                        Err(anyhow!("ISHL Expected Integer VALUE"))
                    }?
                }
                0x7a /* ishr */ => {
                    let shift_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding shift"))?.to_lv_int();
                    let value_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding value"))?.to_lv_int();
                    if let LocalVariable::Int(value) = value_var {
                        if let LocalVariable::Int(shift) = shift_var {
                            self.stack.push_front(LocalVariable::Int(value >> shift));
                            Ok(())
                        } else {
                            Err(anyhow!("LSHR Expected integer SHIFT"))
                        }?;
                        Ok(())
                    } else {
                        Err(anyhow!("LSHR Expected Integer SHIFT"))
                    }?
                }
                0x7c /* iushr */ => {
                    let shift_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding shift"))?.to_lv_int();
                    let value_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding value"))?.to_lv_int();
                    if let LocalVariable::Int(value) = value_var {
                        if let LocalVariable::Int(shift) = shift_var {
                            self.stack.push_front(LocalVariable::Int(value >> shift));
                            Ok(())
                        } else {
                            Err(anyhow!("LSHR Expected integer SHIFT"))
                        }?;
                        Ok(())
                    } else {
                        Err(anyhow!("LSHR Expected Integer SHIFT"))
                    }?
                }
                0x7d /* lushr */ => {
                    let value_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding value"))?;
                    let shift_var = self.stack.pop_front().ok_or(anyhow!("Empty stack finding shift"))?.to_lv_int();
                    if let LocalVariable::Long(value) = value_var {
                        if let LocalVariable::Int(shift) = shift_var {
                            self.stack.push_front(LocalVariable::Long(value >> shift));
                            Ok(())
                        } else {
                            Err(anyhow!("LUSHR Expected integer SHIFT"))
                        }?;
                        Ok(())
                    } else {
                        Err(anyhow!("LUSHR Expected Long VALUE"))
                    }?
                }
                0x82 /* ixor */ => {
                    let value2 = self.stack.pop_front().ok_or(anyhow!("ixor Empty stack finding value"))?;
                    let value1 = self.stack.pop_front().ok_or(anyhow!("ixor Empty stack finding shift"))?;
                    if let LocalVariable::Int(v1) = value1 {
                        if let LocalVariable::Int(v2) = value2 {
                            self.stack.push_front(LocalVariable::Int(v1 ^ v2));
                            Ok(())
                        } else {
                            Err(anyhow!("IXOR Expected integer VALUE2"))
                        }?;
                        Ok(())
                    } else {
                        Err(anyhow!("IXOR Expected integer VALUE1"))
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
                    let value2 = self.stack.pop_front().ok_or(anyhow!("fcmp Could not find variable 2"))?;

                    let value1 = self.stack.pop_front().ok_or(anyhow!("fcmp Could not find variable 1"))?;
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
                            Err(anyhow!("Variable2 was not a float {:?}", value2))
                        }
                    } else {
                        Err(anyhow!("Variable1 was not a float {:?}", value1))
                    }?
                }
                0xba /*invokeVirtual */ => {
                    let idx = self.get_u16();
                    let _ = self.get_u16();
                    let (bootstrap_idx, (method_name, method_type)) = self.class.find_invoke_dynamic(idx)?;
                    let method_handle = self.bootstrap_methods.get(bootstrap_idx as usize).ok_or(anyhow!("Non existing bootstrap method"))?;
                    let method_ref = self.class.resolve_method_handle_ref(method_handle.method_ref)?;

                    let class_name_ref = &method_ref.0;
                    let mut new_stack = vec![];
                    let class = vm.get_class(class_name_ref)?
                        .clone();
                    let (argc, ret) = parse_type(&method_type)?;
                    for _ in 0..argc {
                        new_stack.push(self.stack.pop_front().ok_or(anyhow!("Empty stack when calling new method, {}", ret))?);
                    }
                    new_stack.reverse();
                    let new_frame = Frame::new(class.clone(), &method_name, new_stack.clone(), &method_type, vm)?;
                    self.ip += 1;
                    return Ok(ExecutionResult::Invoke(new_frame));
                }
                0xB2 /*getstatic*/ => {
                    let idx = self.get_u16();

                    self.stack.push_front(self.class.get_static_field(idx)?);
                }
                0xB3 /* putstatic */ =>
                    {
                        let idx = self.get_u16();
                        let field_val = self.stack.pop_front().ok_or(anyhow!("Empty stack when getting field value"))?;
                        self.class.put_static_field(idx, field_val)?;
                    }
                0xB4 | 0xB5 | 0xB6 | 0xB7 | 0xB8 /* getfield, putfield, invokeVirtual, invokeSpecial, invokeStatic */ => {
                    let idx = self.get_u16();
                    let (class_name, (method_name, method_type)) = self.class.find_method_or_field(idx)?;

                    let class_name_ref = &class_name;
                    let mut new_stack = vec![];
                    let class = vm.get_class(class_name_ref)?
                        .clone();
                    let (argc, ret) = parse_type(&method_type)?;

                    match op {
                        0xB4 => {
                            let class_ref = self.stack.pop_front().ok_or(anyhow!("Empty stack when getting field"))?;
                            match class_ref {
                                LocalVariable::Reference(ref_kind) => {
                                    if let ObjectReference(obj_ref) = ref_kind {
                                        let field_val = vm.find_field(obj_ref, &method_name, &method_type)?;
                                        self.stack.push_front(field_val);
                                        Ok(())
                                    } else {
                                        Err(anyhow!("Unexpected type {:?}", ref_kind))
                                    }
                                }
                                _ => Err(anyhow!("Unexpected type {:?}", class_ref))
                            }?
                        }
                        0xB5 => {
                            let field_val = self.stack.pop_front().ok_or(anyhow!("Empty stack when retrieving field"))?;
                            let class_ref = self.stack.pop_front().ok_or(anyhow!("Empty stack when putting field"))?;
                            match class_ref {
                                LocalVariable::Reference(obj_ref) => {
                                    if let ObjectReference(obj_ref) = obj_ref {
                                        println!("Inserted {}{:?} into {:?}", &method_name, &field_val, obj_ref);
                                        {
                                            vm.objects.get_mut(obj_ref)
                                                .ok_or(anyhow!("Object did not exist"))?
                                                .put_field(&method_name, field_val);
                                        }
                                        Ok(())
                                    } else {
                                        Err(anyhow!("Unexpected type {:?}", obj_ref))
                                    }
                                }
                                _ => Err(anyhow!("Unexpected type {:?}", class_ref))
                            }?
                        }
                        0xB6 /* invokeVirtual */ => {
                            for _ in 0..=argc {
                                new_stack.push(self.stack.pop_front().ok_or(anyhow!("Empty stack when calling new method, {} {} {}", method_name, method_type, ret))?);
                            }
                            new_stack.reverse();
                            let ret = Frame::new(class, &method_name, new_stack.clone(), &method_type, vm)?;
                            self.ip += 1;
                            return Ok(ExecutionResult::Invoke(ret));
                        }
                        0xB7 /* invokeSpecial */ => {
                            for _ in 0..=argc {
                                new_stack.push(self.stack.pop_front().ok_or(anyhow!("Empty stack when calling new method, {} {} {}", method_name, method_type, ret))?);
                            }
                            new_stack.reverse();
                            if class_name_ref != &self.class.name
                            {
                                println!("This is another class name - {} -> {}", class_name_ref, &self.class.name);
                                let this_ref = new_stack[0].clone();
                                if let LocalVariable::Reference(ObjectReference(this_idx)) = this_ref {
                                    let super_class = vm.create_superclass(this_idx, class_name_ref)?;
                                    println!("New stack[0] was: {:?}->{:?}", &new_stack[0], super_class);
                                    new_stack[0] = super_class;
                                    Ok(())
                                } else {
                                    Err(anyhow!("Wrong type in the stack"))
                                }?
                            }
                            let ret = Frame::new(class, &method_name, new_stack, &method_type, vm)?;
                            self.ip += 1;
                            return Ok(ExecutionResult::Invoke(ret));
                        }
                        0xB8 /* invokeStatic */ => {
                            for _ in 1..=argc {
                                new_stack.push(self.stack.pop_front().ok_or(anyhow!("Empty stack when calling new method, argc={}, stack_size={}", argc, self.stack.len()))?);
                            }

                            new_stack.reverse();
                            let new_frame = Frame::new(class, &method_name, new_stack, &method_type, vm)?;
                            self.ip += 1;
                            return Ok(ExecutionResult::Invoke(new_frame));
                        }
                        _ => {}
                    }
                }

                0x57 /* pop */ => {
                    self.stack.pop_front().ok_or(anyhow!("Empty stack when popping"))?;
                }
                0x59 /* dup */ =>
                    {
                        let item = self.stack.pop_front()
                            .ok_or(anyhow!("Empty stack when duplicating"))?;
                        self.stack.push_front(item.clone());
                        self.stack.push_front(item.clone());
                    }
                0xAC /* ireturn */ | 0xB0  /* areturn */ => {
                    let ret = self.stack.pop_front()
                        .ok_or(anyhow!("Empty stack when returning"))?;
                    return Ok(ExecutionResult::FunctionCallResult(ret));
                }
                0xB1 /* return */ => {
                    return Ok(ExecutionResult::FunctionCallResult(Void()));
                }

                0xBB /* new */ => {
                    let idx = self.get_u16();
                    let class_name = self.class.resolve_string(idx as usize);

                    self.stack.push_front(LocalVariable::Reference(ObjectReference(vm.new_object(&class_name)?)));
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
                    let store_var = self.stack.pop_front().ok_or(anyhow!("No variable to store"))?;
                    if let LocalVariable::Long(_count) = store_var {
                        self.locals[idx as usize] = store_var;
                    }
                }
                // Begin array area
                0xBC | 0xBD /*newarray, anewarray */ => {
                    let count_var = self.stack.pop_front().ok_or(anyhow!("No count for array"))?;
                    if let LocalVariable::Int(count) = count_var {
                        let arr_type = if op == 0xBD { 12 } else { self.get_u8() };
                        self.stack.push_front(LocalVariable::Reference(ReferenceKind::ArrayReference(vm.new_array(arr_type, count as usize)?)));
                        Ok(())
                    } else {
                        Err(anyhow!("Wtf {:?}", count_var))
                    }?
                }
                0xBE /*arraylength */ => {
                    let array_ref = self.stack.pop_front().ok_or(anyhow!("No array ref"))?;
                    if let LocalVariable::Reference(ReferenceKind::ArrayReference(idx)) = array_ref {
                        if let Some(arr) = vm.arrays.get(idx) {
                            self.stack.push_front(LocalVariable::Int(arr.len() as i32));
                        }
                    }
                    if let LocalVariable::Reference(ReferenceKind::Null()) = array_ref {
                        self.stack.push_front(LocalVariable::Int(0));
                    }
                }
                0x32 | 0x33 | 0x34 | 0x35 /* aaload, baload, caload, saload */ => {
                    let index_var = self.stack.pop_front().ok_or(anyhow!("No index for array"))?;
                    let array_ref = self.stack.pop_front().ok_or(anyhow!("No array_ref"))?;
                    if let LocalVariable::Int(idx) = index_var {
                        if let LocalVariable::Reference(ReferenceKind::ArrayReference(arr_idx)) = array_ref {
                            let item = vm.arrays[arr_idx][idx as usize].clone();
                            match item {
                                LocalVariable::Reference(_) => {
                                    if op == 0x32 {
                                        self.stack.push_front(item);
                                        Ok(())
                                    } else {
                                        Err(anyhow!("Unexpected op={:x} for ref load={:?}", op, &item))
                                    }
                                }
                                LocalVariable::Boolean(_) | LocalVariable::Byte(_) => {
                                    if op == 0x33 {
                                        self.stack.push_front(item.to_lv_int());
                                        Ok(())
                                    } else {
                                        Err(anyhow!("Unexpected op={:x} for boolean/byte load={:?}", op, &item))
                                    }
                                }
                                LocalVariable::Char(_) => {
                                    if op == 0x34 {
                                        self.stack.push_front(item.to_lv_int());
                                        Ok(())
                                    } else {
                                        Err(anyhow!("Unexpected op={:x} for char load={:?}", op, &item))
                                    }
                                }

                                LocalVariable::Short(_) => {
                                    if op == 0x35 {
                                        self.stack.push_front(item.to_lv_int());
                                        Ok(())
                                    } else {
                                        Err(anyhow!("Unexpected op={:x} for short load={:?}", op, &item))
                                    }
                                }
                                _ => { Err(anyhow!("Unexpected item={:?} for load, currently implemented are baload, caload and saload", &item)) }
                            }?;
                            Ok(())
                        } else {
                            Err(anyhow!("Couldn't find an array, {:?}", array_ref))
                        }?
                    }
                }
                0x52 | 0x53 | 0x54 | 0x55 /* <d,a,b,c>astore */ => {
                    let value_var = self.stack.pop_front().ok_or(anyhow!("No value for array"))?;
                    let index_var = self.stack.pop_front().ok_or(anyhow!("No index for array"))?;
                    let array_ref = self.stack.pop_front().ok_or(anyhow!("No array_ref"))?;

                    let index: usize = match index_var {
                        LocalVariable::Int(index) => { Ok(index as usize) }
                        _ => Err(anyhow!("Unexpected index"))
                    }?;

                    let arr = match array_ref {
                        LocalVariable::Reference(ReferenceKind::ArrayReference(a)) => { Ok(a) }
                        _ => Err(anyhow!("Unexpected array ref"))
                    }?;
                    match op {
                        0x42 => {
                            let value = match value_var {
                                LocalVariable::Double(value) => { Ok(value) }
                                _ => Err(anyhow!("Unexpected value"))
                            }?;
                            vm.set_array_element(arr, index, LocalVariable::Double(value));
                            Ok(())

                        }
                        0x53 => {
                            let value = match value_var {
                                LocalVariable::Reference(value) => { Ok(value) }
                                _ => Err(anyhow!("Unexpected value"))
                            }?;
                            vm.set_array_element(arr, index, LocalVariable::Reference(value));
                            Ok(())
                        }
                        0x54 /* bastore */ => {
                            let value = match value_var {
                                LocalVariable::Byte(value) => { Ok(value) }
                                LocalVariable::Boolean(value) => { Ok(value as u8) }
                                def => Err(anyhow!("Unexpected value={:?}", def))
                            }?;
                            println!("Hello I am about to set {} to {}", index, value);
                            vm.set_array_element(arr, index, LocalVariable::Char(value as u8 as char));
                            Ok(())

                        }
                        0x55 /* castore */ => {
                            let value = match value_var {
                                LocalVariable::Int(value) => { Ok(value) }
                                _ => Err(anyhow!("Unexpected value"))
                            }?;

                            vm.set_array_element(arr, index, LocalVariable::Byte(value as u8));
                            Ok(())
                        }
                        _ => { Err(anyhow!("Impossible")) }
                    }?
                }

                0xC2 /* monitorenter */ => {
                    self.stack.pop_front().ok_or(anyhow!("Empty stack when popping"))?;
                    println!("Monitor entered");
                }
                0xc0 /* checkcast */ => {
                    let cp = self.get_u16();
                    let thing = &self.class.constant_pool[cp as usize];
                    if let ConstantPool::ClassIndex(_t) = thing {}
                    println!("Thing is {:?}", thing);
                }
                def => {
                    return Err(anyhow!(format!("Couldn't execute {:x}-{}", def, def).to_string()));
                }
            }

            self.ip += 1;
        }
    }
}
