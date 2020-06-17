use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use crate::class::{Class, Object};
use crate::enums::{Attribute, ConstantPool, Field, LocalVariable};
use crate::frame::Frame;

pub struct VM
{
    file: File,
    pub(crate) classes: RefCell<HashMap<String, Rc<Class>>>,
    pub(crate) objects: RefCell<Vec<Rc<Object>>>,
    constant_pool: Vec<ConstantPool>,
    pub(crate) native_methods: HashMap<String, Rc<dyn Fn(&VM, &[LocalVariable]) -> LocalVariable>>,
}

impl VM
{
    pub(crate) fn make_native_method_name(class_name: &String, method_name: &String) -> String
    {
        format!("{}.{}", class_name.clone(), method_name).to_string()
    }
    pub fn new(file_name: &str) -> Result<Rc<VM>, String>
    {
        let file = File::open(file_name);
        if file.is_err()
        {
            panic!(file.err())
        }
        let mut ret = Self { file: file.unwrap(), classes: RefCell::new(HashMap::new()), objects: RefCell::new(vec![]), constant_pool: (vec![]), native_methods: (HashMap::new()) };
        let begin = ret.read_u32();
        if begin != 0xcafe_babe
        {
            return Err(format!("Unexpected start of file: {:x}", begin));
        }
        let minor = ret.read_u16();

        let major = ret.read_u16();
        println!("Reading java file, version: {}.{}", major, minor);
        let main_class = ret.read_class();
        ret.classes.borrow_mut().insert(main_class.name.clone(), main_class);
        ret.add_native_methods();
        Ok(Rc::new(ret))
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
        self.native_methods.borrow_mut().insert(Self::make_native_method_name(&object_class.name, &"<init>".to_string()), Rc::new(|_, _| { LocalVariable::Void() }));
        self.native_methods.borrow_mut().insert(Self::make_native_method_name(&printstream_class.name, &"println".to_string()), Rc::new(|vm, args| {
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
                        match &vm.constant_pool.get(*re as usize) {
                            None => {}
                            Some(cp) => {
                                match cp {
                                    ConstantPool::StringIndex(idx) => { print!("{}", &vm.resolve_string(*idx as usize)) }
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
    fn resolve_constant_pool(&mut self) -> Vec<ConstantPool>
    {
        let pool_size = self.read_u16() as usize;
        let mut constant_pool: Vec<ConstantPool> = vec![ConstantPool::Empty(); pool_size];
        constant_pool.reserve(pool_size);
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
                num => panic!("Wtf pool size={} number={}", pool_size, num)
            };
            constant_pool.insert(item, c);
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
    /*
    func (l *loader) interfaces(cp ConstPool) (interfaces []string) {
	interfaceCount := l.u2()
	for i := uint16(0); i < interfaceCount; i++ {
		interfaces = append(interfaces, cp.Resolve(l.u2()))
	}
	return interfaces
    }

     */
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

    /*
    func (l *loader) fields(cp ConstPool) (fields []Field) {
	fieldsCount := l.u2()
	for i := uint16(0); i < fieldsCount; i++ {
		fields = append(fields, Field{
			Flags:      l.u2(),
			Name:       cp.Resolve(l.u2()),
			Descriptor: cp.Resolve(l.u2()),
			Attributes: l.attrs(cp),
		})
	}
	return fields
    }
     */
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
    /*
    func (l *loader) attrs(cp ConstPool) (attrs []Attribute) {
	attributesCount := l.u2()
	for i := uint16(0); i < attributesCount; i++ {
		attrs = append(attrs, Attribute{
			Name: cp.Resolve(l.u2()),
			Data: l.bytes(int(l.u4())),
		})
	}
	return attrs
}
     */
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

    pub(crate) fn new_object(&self, class_name: String) -> usize
    {
        let class = &self.classes.borrow()[&class_name];
        self.objects.borrow_mut().push(Rc::new(Object {
            class: class.clone(),
            super_instance: None,
            fields: RefCell::new(HashMap::new()),
        }));
        self.objects.borrow().len() - 1
    }

    pub fn start(vm: Rc<VM>, main_class: String)
    {
        let class = &vm.classes.borrow()[&main_class];
        let frame = Frame::new(&vm, &class, "main".to_string(), vec![]);

        match  frame.unwrap().exec() {
            Ok(res) => { println!("Result of {}: {:#?}", &class.name, res); }
            Err(msg) => { panic!(msg); }
        }

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