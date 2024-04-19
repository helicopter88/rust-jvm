use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use core::fmt;
use anyhow::anyhow;
use crate::rustvm::enums::{Attribute, ConstantPool, Field, LocalVariable, Flags};

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
    pub(crate) static_fields: Box<HashMap<String, LocalVariable>>,
}

pub(crate) type ClassRef = Box<Class>;

pub struct Object
{
    pub(crate) class: ClassRef,
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

    pub(crate) fn put_super_instance(&mut self, super_idx: usize) -> Result<(), anyhow::Error>
    {
        if self.super_instance.is_some()
        {
            return Err(anyhow!("Multiple inheritance hasn't been implemented"));
        }
        self.super_instance = Box::from(Some(super_idx));
        Ok(())
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
            static_fields,
        }
    }
    pub(crate) fn resolve_string(&self, idx: usize) -> String
    {
        return match &self.constant_pool[idx] {
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


    pub(crate) fn resolve_method_handle_ref(&self, idx: u16) -> Result<(String, (String, String)), anyhow::Error>
    {
        let cp = &self.constant_pool;

        let ref_ = &cp[idx as usize];
        match ref_ {
            ConstantPool::MethodHandle(kind, reference_) => {
                let reference = *reference_ as usize;
                match kind {
                    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 => {
                        match cp.get(reference).ok_or(anyhow!("Pointed to non existing thing: {}", reference))? {
                            ConstantPool::FieldRef(class_idx, nat) | ConstantPool::MethodRef(class_idx, nat) | ConstantPool::InterfaceMethodRef(class_idx, nat) => {
                                Ok((self.resolve_string(*class_idx as usize), self.find_name_and_type(*nat)?))
                            }
                            d => { Err(anyhow!("Unexpected field {:?} for kind: {}", d, kind)) }
                        }
                    }
                    _ => { Err(anyhow!("Unexpected kind: {}", kind)) }
                }
            }
            def => { Err(anyhow!("Expected MethodHandle, got {:#?}", def)) }
        }
    }

    pub(crate) fn find_method_or_field(&self, idx: u16) -> Result<(String, (String, String)), anyhow::Error>
    {
        let cp = &self.constant_pool;

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
            def => Err(anyhow!("Found {:#?} which is not a method or field", def))
        }
    }

    fn find_name_and_type(&self, idx: u16) -> Result<(String, String), anyhow::Error>
    {
        let cp = &self.constant_pool;

        let ref_ = &cp[idx as usize];
        match ref_ {
            ConstantPool::NameAndTypeIndex(name_idx, type_idx) => {
                Ok((self.resolve_string(*name_idx as usize),
                    self.resolve_string(*type_idx as usize)))
            }
            def => Err(anyhow!("Found {:#?} which is not a NaT", def))
        }
    }
    pub(crate) fn find_invoke_dynamic(&self, idx: u16) -> Result<(u16, (String, String)), anyhow::Error>
    {
        let cp = &self.constant_pool;

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
            def => Err(anyhow!("Found {:#?} which is not a dynamic or invoke dynamic", def))
        }
    }

    pub(crate) fn get_static_field(&self, idx: u16) -> Result<LocalVariable, anyhow::Error>
    {
        let (_, (field_name, _)) = self.find_method_or_field(idx)?;
        let error_message = anyhow!("Non-existing field, field={}, all_fields={:#?}", field_name, &self.static_fields.keys());
        self.static_fields.get(&field_name).ok_or(error_message).cloned()
    }

    pub(crate) fn put_static_field(&mut self, idx: u16, field_val: LocalVariable) -> Result<(), anyhow::Error> {
        let (class_name, (method_name, _method_type)) = self.find_method_or_field(idx)?;
        dbg!("Putting field={} into={},val={}", &method_name, class_name, &field_val);
        self.static_fields.insert(method_name, field_val);
        Ok(())
    }
}
