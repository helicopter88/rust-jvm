use std::fmt;
use std::fmt::{Debug, Formatter};
use anyhow::anyhow;

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
    Package(u16),
}

#[derive(Debug, Clone)]
pub(crate) struct Field {
    pub(crate) flags: u16,
    pub(crate) name: String,
    pub(crate) descriptor: String,
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
    fn sum(a: &T, b: &T) -> Result<T, anyhow::Error>;
    fn sub(a: &T, b: &T) -> Result<T, anyhow::Error>;
    fn and(a: &T, b: &T) -> Result<T, anyhow::Error>;
    fn shl(a: &T, b: &T) -> Result<T, anyhow::Error>;
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone)]
pub enum ReferenceKind {
    ObjectReference(usize),
    ArrayReference(usize),
    ClassReference(String),
    Null(),
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
    Reference(ReferenceKind),
    ReturnAddress(u16),
    Long(i64),
    Double(f64),
}

impl LocalVariable
{
    pub(crate) fn to_int(&self) -> anyhow::Result<i32>
    {
        match self.to_lv_int()
        {
            LocalVariable::Int(i) => { return Ok(i); }
            def => { Err(anyhow!("Wut, got {:?}", def)) }
        }
    }
    pub(crate) fn to_lv_int(&self) -> LocalVariable
    {
        match &self {
            LocalVariable::Boolean(true) => { LocalVariable::Int(1) }
            LocalVariable::Boolean(false) => { LocalVariable::Int(0) }
            LocalVariable::Byte(b) => { LocalVariable::Int(*b as i32) }
            LocalVariable::Char(c) => { LocalVariable::Int(*c as i32) }
            LocalVariable::Short(s) => { LocalVariable::Int(*s as i32) }
            _ => { self.clone() }
        }
    }
}
/*impl PartialEq for LocalVariable {
fn eq(&self, other: &Self) -> bool {
    match self {
        LocalVariable::Void() => { false } ;
        LocalVariable::Boolean(x) => { x == other.clone() }
        LocalVariable::Byte(_) => {}
        LocalVariable::Char(_) => {}
        LocalVariable::Short(_) => {}
        LocalVariable::Int(_) => {}
        LocalVariable::Float(_) => {}
        LocalVariable::Reference(_) => {}
        LocalVariable::ReturnAddress(_) => {}
        LocalVariable::Long(_) => {}
        LocalVariable::Double(_) => {}
    }
}
}*/
impl LocalVariable {
    pub(crate) fn from_constant_pool(cp: ConstantPool) -> LocalVariable {
        match cp {
            ConstantPool::Integer(num) => {
                LocalVariable::Int(num)
            }
            ConstantPool::Empty() => { LocalVariable::Void() }
            ConstantPool::Float(num) => { LocalVariable::Float(num) }
            ConstantPool::Long(num) => { LocalVariable::Long(num) }
            ConstantPool::Double(num) => { LocalVariable::Double(num) }
            def => {
                println!("Found {:?} in the constant pool", def);
                LocalVariable::Reference(ReferenceKind::Null())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BootstrapMethod {
    pub(crate) method_ref: u16,
    num_args: u16,
    arguments: Vec<u16>,
}

impl BootstrapMethod {
    pub(crate) fn new(method_ref: u16, num_args: u16, arguments: Vec<u16>) -> Self {
        Self {
            method_ref,
            num_args,
            arguments,
        }
    }
}

impl Arithmetic<LocalVariable> for LocalVariable {
    fn sum(a: &LocalVariable, b: &LocalVariable) -> Result<LocalVariable, anyhow::Error> {
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
            _ => {
                let (int_a, int_b) = (a.to_lv_int(), b.to_lv_int());
                match (&int_a, &int_b)
                {
                    (LocalVariable::Int(_), LocalVariable::Int(_)) => { LocalVariable::sum(&int_a, &int_b) }
                    _ => { Err(anyhow!("SUM Mismatched types, {:?} {:?}", a, b)) }
                }
            }
        }
    }

    fn sub(a: &LocalVariable, b: &LocalVariable) -> Result<LocalVariable, anyhow::Error> {
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
            _ => {
                let (int_a, int_b) = (a.to_lv_int(), b.to_lv_int());
                match (&int_a, &int_b)
                {
                    (LocalVariable::Int(_), LocalVariable::Int(_)) => { LocalVariable::sub(&int_a, &int_b) }
                    _ => { Err(anyhow!("SUM Mismatched types, {:?} {:?}", a, b)) }
                }
            }
        }
    }
    fn and(a: &LocalVariable, b: &LocalVariable) -> Result<LocalVariable, anyhow::Error> {
        match (a, b) {
            (LocalVariable::Int(num_a), LocalVariable::Int(num_b)) => {
                Ok(LocalVariable::Int(num_a & num_b))
            }
            _ => {
                let (int_a, int_b) = (a.to_lv_int(), b.to_lv_int());
                match (&int_a, &int_b)
                {
                    (LocalVariable::Int(_), LocalVariable::Int(_)) => { LocalVariable::and(&int_a, &int_b) }
                    _ => { Err(anyhow!("SUM Mismatched types, {:?} {:?}", a, b)) }
                }
            }
        }
    }

    fn shl(a: &LocalVariable, b: &LocalVariable) -> Result<LocalVariable, anyhow::Error> {
        match (a, b) {
            (LocalVariable::Long(num_a), LocalVariable::Int(num_b)) => {
                Ok(LocalVariable::Long(num_a << num_b))
            }
            _ => {
                let (int_a, int_b) = (a.to_lv_int(), b.to_lv_int());
                match (&int_a, &int_b)
                {
                    (LocalVariable::Int(_), LocalVariable::Int(_)) => { LocalVariable::and(&int_a, &int_b) }
                    _ => { Err(anyhow!("SHL Mismatched types, {:?} {:?}", a, b)) }
                }
            }
        }
    }
}
pub const FLAG_PUBLIC: u16 = 1;
pub const FLAG_STATIC: u16 = 0x0008;
pub const FLAG_FINAL: u16 = 0x0010;
pub const FLAG_SUPER: u16 = 0x0020;
pub const FLAG_INTERFACE: u16 = 0x0200;
pub const FLAG_ABSTRACT: u16 = 0x0400;
pub const FLAG_SYNTHETIC: u16 = 0x1000;
pub const FLAG_ENUM: u16 = 0x4000;
pub const FLAG_NATIVE: u16 = 0x0100;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Flags {
    Public,
    Final,
    Super,
    Interface,
    Abstract,
    Native,
}
