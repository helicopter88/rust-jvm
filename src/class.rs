use std::rc::Rc;

use crate::enums::{Attribute, ConstantPool, Field, LocalVariable, Flags};
use std::cell::{RefCell};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use core::fmt;

#[derive(Debug, Clone)]
pub struct Class
{
    pub(crate) constant_pool: Vec<ConstantPool>,
    pub(crate) name: String,
    pub(crate) super_class: String,
    pub(crate) flags: Vec<Flags>,
    interfaces: Vec<String>,
    pub(crate) fields: Vec<Field>,
    pub(crate) methods: Vec<Field>,
    pub(crate) attributes: Vec<Attribute>,
    pub(crate) static_fields: Box<HashMap<String, LocalVariable>>
}


pub struct Object
{
    pub(crate) class: Rc<Class>,
    pub(crate) super_instance: Box<Option<usize>>,
    pub(crate) fields: Box<HashMap<String, LocalVariable>>,
}

impl Object {
    pub(crate) fn put_field(&mut self, name: &str, local_variable: LocalVariable)
    {
        dbg!("PUT FIELD", name, &local_variable);
        self.fields.insert(name.to_string(), local_variable);
        dbg!("PUT FIELD", &self.fields);
    }

    pub(crate) fn put_super_instance(&self, super_idx: usize)
    {
        *self.super_instance = Some(super_idx);
    }

    pub(crate) fn get_super_instance(&self) -> Option<usize>
    {
        *self.super_instance
    }

}
impl Debug for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} (super: {:?})", &self.class.name, &self.super_instance)
    }
}
impl Class {
    pub(crate) fn new(constant_pool: Vec<ConstantPool>,
                      name: String,
                      super_class: String,
                      flags: Vec<Flags>,
                      interfaces: Vec<String>,
                      fields: Vec<Field>,
                      methods: Vec<Field>,
                      attributes: Vec<Attribute>) -> Self {
        let static_fields = Box::new(HashMap::from([("$assertionsDisabled".to_string(), LocalVariable::Boolean(true))]));
        Class {
            constant_pool,
            name,
            super_class,
            flags,
            interfaces,
            fields,
            methods,
            attributes,
            static_fields
        }
    }
}