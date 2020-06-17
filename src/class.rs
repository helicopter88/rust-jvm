use std::rc::Rc;

use crate::enums::{Attribute, ConstantPool, Field, LocalVariable};
use std::cell::RefCell;
use std::collections::HashMap;
use std::borrow::Borrow;

#[derive(Debug, Clone)]
pub struct Class
{
    pub(crate) constant_pool: Vec<ConstantPool>,
    pub(crate) name: String,
    super_class: String,
    flags: u16,
    interfaces: Vec<String>,
    pub(crate) fields: Vec<Field>,
    pub(crate) methods: Vec<Field>,
    pub(crate) attributes: Vec<Attribute>,
}

pub struct Object
{
    pub(crate) class: Rc<Class>,
    pub(crate) super_instance: Option<Rc<Object>>,
    pub(crate) fields: RefCell<HashMap<String, LocalVariable>>,
}

impl Object {
    pub(crate) fn put_field(&self, name: &str, local_variable: LocalVariable)
    {
        dbg!(name, &local_variable);
        self.fields.borrow_mut().insert(name.to_string(), local_variable);
    }

}
impl Class {
    pub(crate) fn new(constant_pool: Vec<ConstantPool>,
                      name: String,
                      super_class: String,
                      flags: u16,
                      interfaces: Vec<String>,
                      fields: Vec<Field>,
                      methods: Vec<Field>,
                      attributes: Vec<Attribute>) -> Self {
        Class {
            constant_pool,
            name,
            super_class,
            flags,
            interfaces,
            fields,
            methods,
            attributes,
        }
    }
}