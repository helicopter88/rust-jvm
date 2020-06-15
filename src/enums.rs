use std::fmt;
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub(crate) struct Attribute {
    pub(crate) name: String,
    pub(crate) data: Vec<u8>,
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Attribute").field("name", &self.name).finish()?;
        Ok(())
    }
}

#[derive(Debug, PartialOrd, PartialEq, Clone)]
pub(crate) enum ConstantPool
{
    Empty(),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    FieldRef(u16, u16),
    MethodRef(u16, u16),
    InterfaceMethodRef(u16, u16),
    ClassIndex(u16),
    NameAndTypeIndex(u16, u16),
    StringIndex(u16),
    DescIndex(u16),
    JvmString(String),
    MethodHandle(u8, u16),
    MethodType(u16),
    Dynamic(u16, u16),
    InvokeDynamic(u16, u16),
    Module(u16),
    Package(u16)
}

#[derive(Debug, Clone)]
pub(crate) struct Field {
    flags: u16,
    pub(crate) name: String,
    descriptor: String,
    pub(crate) attributes: Vec<Attribute>,
}

impl Field {
    pub(crate) fn new(flags: u16, name: String, descriptor: String, attributes: Vec<Attribute>) -> Self
    {
        Field {
            flags,
            name,
            descriptor,
            attributes,
        }
    }
}

pub trait Arithmetic<T>
{
    fn sum(a: &T, b: &T) -> Result<T, String>;
    fn sub(a: &T, b: &T) -> Result<T, String>;
}
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum LocalVariable {
    Void(),
    Boolean(bool),
    Byte(u8),
    Char(char),
    Short(i16),
    Int(i32),
    Float(f32),
    Reference(u16),
    ReturnAddress(u16),
    Long(i64),
    Double(f64)
}

#[derive(Debug)]
pub(crate) struct BootstrapMethod {
    pub(crate) method_ref: u16,
    num_args: u16,
    arguments: Vec<u16>
}

impl BootstrapMethod {
    pub(crate) fn new(method_ref: u16, num_args: u16, arguments: Vec<u16>) -> Self {
        Self {
            method_ref,
            num_args,
            arguments
        }
    }
}

impl Arithmetic<LocalVariable> for LocalVariable {
    fn sum(a: &LocalVariable, b: &LocalVariable) -> Result<LocalVariable, String> {
        match (a, b) {
            (LocalVariable::Int(num_a), LocalVariable::Int(num_b)) => {
                Ok(LocalVariable::Int(num_a + num_b))
            }
            (LocalVariable::Short(num_a), LocalVariable::Short(num_b)) => {
                Ok(LocalVariable::Short(num_a + num_b))
            }
            (LocalVariable::Float(num_a), LocalVariable::Float(num_b)) => {
                Ok(LocalVariable::Float(num_a + num_b))
            }
            (LocalVariable::Long(num_a), LocalVariable::Long(num_b)) => {
                Ok(LocalVariable::Long(num_a + num_b))
            }
            (LocalVariable::Double(num_a), LocalVariable::Double(num_b)) => {
                Ok(LocalVariable::Double(num_a + num_b))
            }
            _ => { Err("Mismatched types".to_string()) }
        }
    }

    fn sub(a: &LocalVariable, b: &LocalVariable) -> Result<LocalVariable, String> {
        match (a, b) {
            (LocalVariable::Int(num_a), LocalVariable::Int(num_b)) => {
                Ok(LocalVariable::Int(num_a - num_b))
            }
            (LocalVariable::Short(num_a), LocalVariable::Short(num_b)) => {
                Ok(LocalVariable::Short(num_a - num_b))
            }
            (LocalVariable::Float(num_a), LocalVariable::Float(num_b)) => {
                Ok(LocalVariable::Float(num_a - num_b))
            }
            (LocalVariable::Long(num_a), LocalVariable::Long(num_b)) => {
                Ok(LocalVariable::Long(num_a - num_b))
            }
            (LocalVariable::Double(num_a), LocalVariable::Double(num_b)) => {
                Ok(LocalVariable::Double(num_a - num_b))
            }
            _ => { Err("Mismatched types".to_string()) }
        }
    }
}