use std::borrow::{BorrowMut, Borrow};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::{File, DirEntry, ReadDir};
use std::io::{Read, Seek, SeekFrom};
use std::rc::Rc;

use crate::class::{Class, Object};
use crate::enums::{Attribute, ConstantPool, Field, LocalVariable};
use crate::frame::Frame;
use std::fs;
use std::path::{Path, PathBuf};

pub struct VM
{
    classpath: PathBuf,
    classes: RefCell<HashMap<String, Rc<Class>>>,
    pub(crate) objects: RefCell<Vec<Rc<Object>>>,
    pub(crate) native_methods: HashMap<String, Rc<dyn Fn(&Class, &Frame, &[LocalVariable]) -> LocalVariable>>,
}

pub(crate) struct FileReader
{
    file: File,
    constant_pool: Vec<ConstantPool>,
}

impl FileReader {
    pub fn read_class_from_file(file_name: &str) -> Result<Rc<Class>, String>
    {
        let file = File::open(file_name);
        if file.is_err() {
            panic!("Couldn't open file: {:?}", file.err())
        }
        let mut ret = Self { file: file.unwrap(), constant_pool: vec![] };

        return Ok(ret.read_class());
    }

    fn resolve_constant_pool(&mut self) -> Vec<ConstantPool>
    {
        let pool_size = self.read_u16() as usize;
        let mut constant_pool: Vec<ConstantPool> = vec![ConstantPool::Empty(); pool_size];
        for item in 1..pool_size
        {
            let tag = self.read_u8();
            let c: ConstantPool = match tag {
                0x01 => {
                    let size = self.read_u16() as usize;
                    ConstantPool::JvmString(self.read_variable_as_str(size))
                }
                0x03 => { ConstantPool::Integer(self.read_i32()) }
                0x04 => { ConstantPool::Float(self.read_i32() as f32) }
                0x05 => { ConstantPool::Long(self.read_u64() as i64) }
                0x06 => { ConstantPool::Double(self.read_u64() as f64) }
                0x07 => { ConstantPool::ClassIndex(self.read_u16()) }
                0x08 => { ConstantPool::StringIndex(self.read_u16()) }
                0x09 => { ConstantPool::FieldRef(self.read_u16(), self.read_u16()) }
                0x0a => { ConstantPool::MethodRef(self.read_u16(), self.read_u16()) }
                0x0b => { ConstantPool::InterfaceMethodRef(self.read_u16(), self.read_u16()) }
                0x0c => { ConstantPool::NameAndTypeIndex(self.read_u16(), self.read_u16()) }
                0x0f => { ConstantPool::MethodHandle(self.read_u8(), self.read_u16()) }
                0x10 => { ConstantPool::MethodType(self.read_u16()) }
                0x11 => { ConstantPool::Dynamic(self.read_u16(), self.read_u16()) }
                0x12 => { ConstantPool::InvokeDynamic(self.read_u16(), self.read_u16()) }
                0x13 => { ConstantPool::Module(self.read_u16()) }
                0x14 => { ConstantPool::Package(self.read_u16()) }
                num => {
                    println!("Unknown constant pool: {}, with size {}/{:?}", num, pool_size - 1, constant_pool);
                    self.file.seek(SeekFrom::Current(-1));
                    continue;
                }
            };
            constant_pool[item] = c;
        }
        constant_pool
    }

    pub(crate) fn resolve_field_and_type(&self, idx: usize) -> (String, String)
    {
        return match &self.constant_pool[idx] {
            ConstantPool::ClassIndex(recursive_idx) => {
                self.resolve_field_and_type((*recursive_idx) as usize)
            }
            ConstantPool::FieldRef(class_name, attrib_name) => {
                (self.resolve_string((*class_name) as usize), self.resolve_string(*attrib_name as usize))
            }
            ConstantPool::MethodRef(class_name, method_name) => {
                (self.resolve_string((*class_name) as usize), self.resolve_string(*method_name as usize))
            }
            ConstantPool::NameAndTypeIndex(nameIdx, type_idx) => {
                (self.resolve_string((*nameIdx) as usize), self.resolve_string(*type_idx as usize))
            }
            def => {
                panic!("What is this {:#?}", def)
            }
        };
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

    pub fn resolve_interfaces(&mut self) -> Vec<String>
    {
        let mut interfaces: Vec<String> = vec![];
        let interface_count = self.read_u16();
        for _ in 0..interface_count
        {
            let str_idx = self.read_u16() as usize;
            let str = self.resolve_string(str_idx);
            interfaces.push(str)
        }
        interfaces
    }

    fn resolve_fields(&mut self) -> Vec<Field>
    {
        let mut fields: Vec<Field> = vec![];
        let field_count = self.read_u16();
        fields.reserve((field_count) as usize);
        for _ in 0..field_count
        {
            let flags = self.read_u16();
            let name_idx = self.read_u16() as usize;
            let name = self.resolve_string(name_idx);
            let descriptor_idx = self.read_u16() as usize;
            let descriptor = self.resolve_string(descriptor_idx);
            fields.push(Field::new(
                flags,
                name,
                descriptor,
                self.resolve_attributes(),
            ))
        }
        fields
    }

    fn resolve_attributes(&mut self) -> Vec<Attribute>
    {
        let mut attributes: Vec<Attribute> = vec![];
        let attributes_count = self.read_u16();
        for _ in 0..attributes_count
        {
            let name_idx = self.read_u16() as usize;
            let name = self.resolve_string(name_idx);
            let data_len = self.read_u32() as usize;
            let data = self.read_variable(data_len);
            attributes.push(Attribute { name, data })
        }
        attributes
    }

    fn read_class(&mut self) -> Rc<Class>
    {
        let begin = self.read_u32();
        let minor = self.read_u16();

        let major = self.read_u16();
        println!("Reading java file, version: {:X}, {}.{}",begin, major, minor);
        self.constant_pool = self.resolve_constant_pool();
        let constant_pool = self.constant_pool.clone();

        let flags = self.read_u16();
        let name_idx = self.read_u16() as usize;
        let name = self.resolve_string(name_idx);
        let super_idx = self.read_u16() as usize;
        let super_class = self.resolve_string(super_idx);
        let interfaces = self.resolve_interfaces();
        let fields = self.resolve_fields();
        let methods = self.resolve_fields();
        let attributes = self.resolve_attributes();
        Rc::new(Class::new(
            constant_pool.to_vec(),
            name,
            super_class,
            flags,
            interfaces,
            fields,
            methods,
            attributes,
        ))
    }


    fn read_u8(&mut self) -> u8
    {
        let mut buf = [0; 1];
        assert!(self.file.read_exact(&mut buf).is_ok());
        u8::from_be_bytes(buf)
    }

    fn read_u16(&mut self) -> u16
    {
        let mut buf = [0; 2];
        assert!(self.file.read_exact(&mut buf).is_ok());
        u16::from_be_bytes(buf)
    }

    fn read_i32(&mut self) -> i32
    {
        let mut buf = [0; 4];
        assert!(self.file.read_exact(&mut buf).is_ok());
        i32::from_be_bytes(buf)
    }

    fn read_u32(&mut self) -> u32
    {
        let mut buf = [0; 4];
        assert!(self.file.read_exact(&mut buf).is_ok());
        u32::from_be_bytes(buf)
    }

    fn read_u64(&mut self) -> u64
    {
        let mut buf = [0; 8];
        assert!(self.file.read_exact(&mut buf).is_ok());
        u64::from_be_bytes(buf)
    }

    fn read_variable(&mut self, size: usize) -> Vec<u8>
    {
        let mut buf: Vec<u8> = vec![0; size];
        assert!(self.file.read_exact(buf.as_mut_slice()).is_ok());
        buf
    }

    fn read_variable_as_str(&mut self, size: usize) -> String
    {
        let buf = self.read_variable(size);
        String::from_utf8(buf.clone()).unwrap_or("Wtf".to_string())
    }
}

impl VM
{
    pub fn new(file_name: &str, class_path: &str) -> Result<Rc<VM>, String>
    {
        let classpath = Path::new(class_path);

        let main_class = FileReader::read_class_from_file(file_name)?;

        let mut ret = Self { classes: RefCell::new(HashMap::new()), objects: RefCell::new(vec![]), native_methods: (HashMap::new()), classpath: classpath.to_path_buf() };

        ret.classes.borrow_mut().insert(main_class.name.clone(), main_class.clone());
        ret.add_native_methods();
        Ok(Rc::new(ret))
    }

    pub(crate) fn make_native_method_name(class_name: &String, method_name: &String) -> String
    {
        format!("{}.{}", class_name.clone(), method_name).to_string()
    }

    fn add_native_methods(&mut self) {
        let object_init_method = Field::new(0, "<init>".to_string(), "()V".to_string(), vec![]);
        let object_class = Rc::new(Class::new(
            vec![],
            "java/lang/Object".to_string(),
            "".to_string(),
            0,
            vec![],
            vec![],
            vec![object_init_method],
            vec![],
        ));

        let println_method = Field::new(0, "println".to_string(), "()V".to_string(), vec![]);
        let out_field = Field::new(0, "out".to_string(), "Ljava/io/PrintStream".to_string(), vec![]);
        let system_class = Rc::new(Class::new(
            vec![],
            "java/lang/System".to_string(),
            "java/lang/Object".to_string(),
            0,
            vec![],
            vec![out_field],
            vec![],
            vec![],
        ));
        let printstream_class = Rc::new(Class::new(
            vec![],
            "java/io/PrintStream".to_string(),
            "java/lang/Object".to_string(),
            0,
            vec![],
            vec![],
            vec![println_method],
            vec![],
        ));
        self.native_methods.borrow_mut().insert(Self::make_native_method_name(&object_class.name, &"<init>".to_string()), Rc::new(|_, _, _| { LocalVariable::Void() }));
        self.native_methods.borrow_mut().insert(Self::make_native_method_name(&printstream_class.name, &"println".to_string()), Rc::new(|class, frame, args| {
            for item in args {
                match item {
                    LocalVariable::Void() => {}
                    LocalVariable::Boolean(b) => { print!("{}", b) }
                    LocalVariable::Byte(b) => { print!("{}", b) }
                    LocalVariable::Char(b) => { print!("{}", b) }
                    LocalVariable::Short(b) => { print!("{}", b) }
                    LocalVariable::Int(b) => { print!("{}", b) }
                    LocalVariable::Float(b) => { print!("{}", b) }
                    LocalVariable::Reference(re) => {
                        match &class.constant_pool.get(*re as usize) {
                            None => {}
                            Some(cp) => {
                                match cp {
                                    ConstantPool::StringIndex(idx) => { print!("{}", &frame.resolve_string(*idx as usize)) }
                                    ConstantPool::JvmString(string) => { print!("{}", string); }
                                    _ => {}
                                }
                            }
                        }
                    }
                    LocalVariable::ReturnAddress(_) => {}
                    LocalVariable::Long(b) => { print!("{}", b) }
                    LocalVariable::Double(b) => { print!("{}", b) }
                }
            }
            print!("\n");

            LocalVariable::Void()
        }));


        self.classes.borrow_mut().insert(object_class.name.clone(), object_class);
        self.classes.borrow_mut().insert(system_class.name.clone(), system_class);
        self.classes.borrow_mut().insert(printstream_class.name.clone(), printstream_class);
    }

    pub(crate) fn new_object(&self, class_name: String) -> usize
    {
        dbg!("Trying to find", &class_name);
        let class = &self.get_class(&class_name).unwrap();

        self.objects.borrow_mut().push(Rc::new(Object {
            class: class.clone(),
            super_instance: None,
            fields: RefCell::new(HashMap::new()),
        }));
        self.objects.borrow().len() - 1
    }

    pub(crate) fn get_class(&self, class_name: &str) -> Option<Rc<Class>>
    {
        if self.classes.clone().borrow().get(class_name).is_some() {
            return Some(self.classes.clone().borrow().get(class_name).unwrap().clone());
        }

        // Let's look in the classpath
        println!("Class {}", class_name);
        let full_class_name = format!("{}.class", class_name);
        let full_class_path = Path::new(full_class_name.as_str());
        let file = self.classpath.join(full_class_path);
        if file.is_file() {
            let new_class = FileReader::read_class_from_file(file.as_os_str().to_str()?);
            if new_class.is_ok() {
                let ok_class = new_class.unwrap().clone();
                self.classes.clone().borrow_mut().insert(class_name.to_string(), ok_class.clone());
                return Some(ok_class.clone());
            }
        }
        None
    }


    pub fn start(vm: Rc<VM>, main_class: String)
    {
        let class = &vm.classes.borrow()[&main_class];
        let frame = Frame::new(&vm, &class, "main".to_string(), vec![]);

        match frame.unwrap().exec() {
            Ok(res) => { println!("Result of {}: {:#?}", &class.name, res); }
            Err(msg) => { panic!(msg); }
        }
    }
}