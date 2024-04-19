use crate::rustvm::enums::{Arithmetic, LocalVariable, ReferenceKind};
pub(crate) use crate::rustvm::frame::Stack;

pub(super) fn execute_iload_n(op: u8, locals: &Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
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

pub(crate) fn execute_lload_n(op: u8, locals: &Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
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

pub(crate) fn execute_fload_n(op: u8, locals: &Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
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

pub(crate) fn execute_aload(op: u8, locals: &Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
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

pub(crate) fn execute_astore(op: u8, locals: &mut Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
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

pub(crate) fn execute_istore(op: u8, locals: &mut Vec<LocalVariable>, stack: &mut Stack) -> Result<(), String>
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

pub(super) fn execute_load_integer_const(op: u8, stack: &mut Stack)
{
    const NEGATIVE_ONE_STARTING_POINT: i32 = 0x3;
    stack.push_front(LocalVariable::Int(op as i32 - NEGATIVE_ONE_STARTING_POINT))
}

pub(crate) fn execute_load_long_const(op: u8, stack: &mut Stack)
{
    const NEGATIVE_ONE_STARTING_POINT: i64 = 0x1e;
    stack.push_front(LocalVariable::Long(op as i64 - NEGATIVE_ONE_STARTING_POINT))
}

pub(crate) fn execute_load_float_const(op: u8, stack: &mut Stack)
{
    const NEGATIVE_ONE_STARTING_POINT: f32 = 0xb as f32;
    stack.push_front(LocalVariable::Float(op as f32 - NEGATIVE_ONE_STARTING_POINT))
}

pub(crate) fn execute_load_double_const(op: u8, stack: &mut Stack)
{
    match op {
        0xe => { stack.push_front(LocalVariable::Double(0.0)) }
        0xf => { stack.push_front(LocalVariable::Double(1.0)) }
        _ => {}
    }
}

pub(super) fn execute_add(stack: &mut Stack) -> Result<(), String>
{
    let b = stack.pop_front().ok_or("ADD Missing stack variable 1")?;
    let a = stack.pop_front().ok_or("ADD Missing stack variable 2")?;

    stack.push_front(LocalVariable::sum(&a, &b)?);

    Ok(())
}


pub(crate) fn execute_sub(stack: &mut Stack) -> Result<(), String>
{
    let a = stack.pop_front().ok_or("SUB Missing stack variable 1")?;
    let b = stack.pop_front().ok_or("SUB Missing stack variable 2")?;

    stack.push_front(LocalVariable::sub(&b, &a)?);

    Ok(())
}

pub(crate) fn execute_bitwise(op: u8, stack: &mut Stack) -> Result<(), String>
{
    let a = stack.pop_front().ok_or("SUB Missing stack variable 2")?;
    let b = stack.pop_front().ok_or("SUB Missing stack variable 1")?;
    match op {
        0x7e => {
            stack.push_front(LocalVariable::and(&b, &a)?);
        }
        0x79 => {
            stack.push_front(LocalVariable::shl(&b, &a)?);
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

pub(crate) fn execute_if(op: u8, stack: &mut Stack) -> Result<bool, String> {
    let a = stack.pop_front().ok_or("IF Missing stack variable 1")?;
    if op >= 0x99 && op <= 0x9e {
        let zero = if let LocalVariable::Boolean(_) = a { LocalVariable::Boolean(false) } else { LocalVariable::Int(0) };
        return execute_if_internal(op, a, zero);
    }
    let b = stack.pop_front().ok_or("IF Missing stack variable 2")?;
    return execute_if_internal(op, b, a);
}

pub(crate) fn execute_if_null(op: u8, stack: &mut Stack) -> Result<bool, String> {
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


pub(crate) fn execute_iconv(op: u8, stack: &mut Stack) -> Result<(), String> {
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
