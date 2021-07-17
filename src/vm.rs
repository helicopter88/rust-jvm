use std::cell::RefCell;
use std::collections::{HashMap, VecDeque, HashSet};
use std::fs::File;
use std::io::{Read};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::class::{Class, Object};
use crate::enums::{Attribute, ConstantPool, Field, FLAG_ABSTRACT, FLAG_ENUM, FLAG_FINAL, FLAG_PUBLIC, FLAG_STATIC, FLAG_SUPER, Flags, LocalVariable, ReferenceKind, FLAG_SYNTHETIC, FLAG_NATIVE};
use crate::enums::ConstantPool::Empty;
use crate::frame::Frame;
use crate::enums::ReferenceKind::{ObjectReference, ClassReference, Null, ArrayReference};
use std::borrow::Borrow;
use std::thread;
use std::future::Future;
use futures::executor::block_on;
use std::sync::{Arc, Mutex};
use futures::TryFutureExt;

pub struct VM
{
    classpath: PathBuf,
    classes: Rc<RefCell<HashMap<String, Rc<Class>>>>,
    pub(crate) objects: RefCell<Vec<Rc<Object>>>,
    pub(crate) native_methods: HashMap<String, Rc<dyn Fn(&Class, &Frame, &[LocalVariable]) -> Result<LocalVariable, String>>>,
    pub(crate) native_fields: HashMap<String, LocalVariable>,
    pub(crate) arrays: RefCell<Vec<RefCell<Vec<LocalVariable>>>>,
    cl: DeferredClassLoader,
}

pub(crate) struct FileReader
{
    file: File,
    constant_pool: Vec<ConstantPool>,
}

struct DeferredClassLoader
{
    classpath: PathBuf,
    classes: Arc<Mutex<HashMap<String, Rc<Class>>>>,
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
        let mut constant_pool: Vec<ConstantPool> = vec![ConstantPool::Empty(); pool_size + 1];
        let mut count = 1;
        loop
        {
            if count == pool_size
            {
                break;
            }
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
                    panic!("Unknown constant pool: {} {}, with size {}/{:?}", count, num, pool_size, constant_pool);
                }
            };
            match c {
                ConstantPool::Double(_) | ConstantPool::Long(_) => {
                    constant_pool[count] = c;
                    count += 1;
                    constant_pool[count] = Empty();
                }
                _ => {
                    constant_pool[count] = c;
                }
            }
            count += 1;
        }
        constant_pool
    }

    pub(crate) fn resolve_string(&self, idx: usize) -> String
    {
        return match &self.constant_pool[idx] {
            ConstantPool::JvmString(str) => { str.to_string() }
            ConstantPool::ClassIndex(recursive_idx) => {
                self.resolve_string((*recursive_idx) as usize)
            }
            def => {
                //panic!("Unexpected index, {} which is a {:#?}", idx, def);
                panic!("Wtf Requested index: {} which is instead a {:#?}", idx, def);
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
        fields.reserve(field_count as usize);
        println!("Found this many fields={}", field_count);
        for _ in 0..field_count
        {
            let flags = self.read_u16();
            let name_idx = self.read_u16() as usize;
            let name = self.resolve_string(name_idx);
            println!("Found this field '{}'", name);
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
        attributes.reserve((attributes_count.into()));
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

    fn parse_flags(from: u16) -> Vec<Flags>
    {
        let mut ret = vec![];
        if from & FLAG_PUBLIC != 0
        {
            ret.push(Flags::Public)
        }
        if from & FLAG_ABSTRACT != 0
        {
            ret.push(Flags::Abstract)
        }
        if from & FLAG_SUPER != 0
        {
            ret.push(Flags::Super)
        }
        if from & FLAG_FINAL != 0
        {
            ret.push(Flags::Final)
        }
        if from & FLAG_NATIVE != 0
        {
            ret.push(Flags::Native)
        }
        ret
    }
    fn read_class(&mut self) -> Rc<Class>
    {
        let begin = self.read_u32();
        assert_eq!(begin, 0xCAFEBABE);
        let minor = self.read_u16();

        let major = self.read_u16();
        println!("Reading java file, version: {:X}, {}.{}", begin, major, minor);
        self.constant_pool = self.resolve_constant_pool();
        let constant_pool = self.constant_pool.clone();

        let flags = Self::parse_flags(self.read_u16()).clone();
        let name_idx = self.read_u16() as usize;
        let name = self.resolve_string(name_idx);
        let super_idx = self.read_u16() as usize;
        let mut super_class = "".to_string();
        if super_idx != 0
        {
            super_class = self.resolve_string(super_idx);
        }
        println!("Loading class={} super={}", &name, &super_class);
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

        let mut ret = Self {
            classes: Rc::new(RefCell::new(HashMap::new())),
            objects: RefCell::new(vec![]),
            native_methods: (HashMap::new()),
            classpath: classpath.to_path_buf(),
            arrays: RefCell::new(vec![]),
            cl: DeferredClassLoader::new(classpath.to_path_buf()),
            native_fields: Default::default(),
        };

        ret.cl.insert_class(&main_class.name, main_class.clone());
        ret.native_methods.insert(VM::make_native_method_name("java/lang/Object", "getClass"), Rc::new(|class, frame, variables| {
            if let LocalVariable::Reference(ObjectReference(idx)) = variables[0]
            {
                let objects = frame.vm.objects.clone();
                let obj = objects.borrow().get(idx).clone().unwrap().class.name.to_string();

                return Ok(LocalVariable::Reference(ReferenceKind::ClassReference(obj)));
            } else {
                panic!("Wtf")
            }
        }));
        ret.native_methods.insert(VM::make_native_method_name("java/lang/Object", "hashCode"), Rc::new(|class, frame, variables| {
            if let LocalVariable::Reference(ObjectReference(idx)) = variables[0] {
                return Ok(LocalVariable::Int(idx as i32 * 3));
            } else {
                panic!("Wtf")
            }
        }));
        ret.native_methods.insert(VM::make_native_method_name("java/lang/Class", "getName"), Rc::new(|class, frame, variables| {
            if let LocalVariable::Reference(ClassReference(idx)) = &variables[0]
            {
                let obj = frame.vm.new_object("java/lang/String");
                let string_class = frame.vm.get_class("java/lang/String").unwrap();
                let array = frame.vm.new_string(&idx);
                let res = Frame::new(&frame.vm, &string_class, "<init>",
                                     [LocalVariable::Reference(ReferenceKind::ObjectReference(obj)), LocalVariable::Reference(ReferenceKind::ArrayReference(array))].to_vec()
                                     , "([C)V")?.exec();
                println!("getName returned {:?}", res);
                if res.is_err()
                {
                    panic!("Error occurred when running getName, err={}", res.err().unwrap());
                }
                return Ok(LocalVariable::Reference(ObjectReference(obj)));
            }
            return Err("Unexpected argument".into());
        }));
        ret.native_methods.insert(VM::make_native_method_name("java/lang/System", "arraycopy"), Rc::new(|class, frame, variables| {
            let src = variables[0].clone();
            let start = variables[1].clone().to_int() as usize;
            let dst = variables[2].clone();
            let end = variables[3].clone().to_int() as usize;
            let len = variables[4].clone().to_int() as usize;
            if let LocalVariable::Reference(ArrayReference(src_ref)) = src
            {
                if let LocalVariable::Reference(ArrayReference(dst_ref)) = dst
                {
                    let borrowed_arrays = frame.vm.arrays.borrow().clone();
                    let src_arr = borrowed_arrays.get(src_ref).ok_or("Source did not exist")?.clone();
                    let dst_arr = borrowed_arrays.get(dst_ref).ok_or("Destination did not exist")?.clone();

                    let mut count = end as usize;
                    for item in src_arr.into_inner()
                    {
                        if count == len - 1
                        {
                            break;
                        }
                        dst_arr.borrow_mut()[end + count] = item;
                        count += 1;
                    }
                } else {
                    return Err(format!("Dest was not an array {:?} in {:?}", dst, variables));
                }
            } else {
                return Err(format!("Source was not an array {:?} in {:?}", src, variables));
            }

            return Ok(LocalVariable::Void());
        }));
        ret.native_methods.insert(VM::make_native_method_name("jdk/internal/util/SystemProps$Raw", "platformProperties"), Rc::new(|class, frame, variables|
            {
                println!("Called system props");
                let arr_idx = frame.vm.new_array(12, 38);
                Ok(LocalVariable::Reference(ArrayReference(arr_idx)))
            }));
        ret.native_methods.insert(VM::make_native_method_name("jdk/internal/util/SystemProps$Raw", "vmProperties"), Rc::new(|class, frame, variables|
            {
                let arr_idx = frame.vm.new_array(12, 0);
                Ok(LocalVariable::Reference(ArrayReference(arr_idx)))
            }));
        Ok(Rc::new(ret))
    }

    pub(crate) fn make_native_method_name(class_name: &str, method_name: &str) -> String
    {
        format!("{}.{}", class_name.clone(), method_name).to_string()
    }


    fn type_to_class(t: &str) -> &str
    {
        if t.chars().nth(0) != Some('L') {
            return t;
        }
        return &t[1..t.len() - 1];
    }

    fn default_initialiser(&self, descriptor: &str, is_final: bool) -> LocalVariable
    {
        match descriptor.chars().nth(0).unwrap() {
            'B' => {
                LocalVariable::Byte(0)
            }
            'C' => {
                LocalVariable::Char('\0')
            }
            'F' => {
                LocalVariable::Float(0.0)
            }
            'D' => {
                LocalVariable::Double(0.0)
            }
            'I' => {
                LocalVariable::Int(0)
            }
            'J' => {
                LocalVariable::Long(0)
            }
            'L' => {
                if is_final {
                    LocalVariable::Reference(ReferenceKind::ObjectReference(self.new_object(VM::type_to_class(descriptor))))
                } else {
                    LocalVariable::Reference(Null())
                }
            }
            'Z' => {
                LocalVariable::Boolean(false)
            }
            '[' =>
                {
                    LocalVariable::Reference(ReferenceKind::Null())
                }
            _ => {
                panic!("descriptor {}", descriptor);
            }
        }
    }

    pub(crate) fn new_object(&self, class_name: &str) -> usize
    {
        let class = &self.get_class(&class_name).unwrap();
        let mut fields = HashMap::new();
        for field in &class.fields {
            if field.flags & (FLAG_SYNTHETIC | FLAG_FINAL | FLAG_STATIC) == (FLAG_SYNTHETIC | FLAG_FINAL | FLAG_STATIC) {
                if field.name == "$assertionsDisabled"
                {
                    fields.insert(field.name.clone(), LocalVariable::Boolean(true));
                }
                continue;
            }
            if field.flags & FLAG_FINAL | FLAG_STATIC == FLAG_FINAL | FLAG_STATIC && VM::type_to_class(&field.descriptor) != class_name
            {
                for attrib in &field.attributes {
                    if attrib.name == "ConstantValue" {
                        let mut buf = [0 as u8; 2];
                        for idx in 0..=1 {
                            buf[idx] = attrib.data[0 + idx]
                        }
                        let const_idx: u16 = u16::from_be_bytes(buf);
                        if let Some(cp) = class.constant_pool.get(const_idx as usize) {
                            fields.insert(field.name.clone(), LocalVariable::from_constant_pool(cp.clone()));
                        }
                        continue;
                    }
                }
                fields.insert(field.name.clone(), self.default_initialiser(&field.descriptor, true));
            } else {
                fields.insert(field.name.clone(), self.default_initialiser(&field.descriptor, false));
            }
        }
        self.objects.borrow_mut().push(Rc::new(Object {
            class: class.clone(),
            super_instance: RefCell::new(None),
            fields: RefCell::new(fields),
        }));
        self.objects.borrow().len() - 1
    }

    pub(crate) fn set_array_element(&self, array: usize, idx: usize, elem: LocalVariable) {
        println!("The array is {:?}", self.arrays.borrow()[array]);
        self.arrays.borrow_mut()[array].borrow_mut()[idx] = elem;
    }

    pub(crate) fn new_string(&self, string: &str) -> usize {
        let array = self.new_array(5, string.len());
        println!("Creating new string array, idx={} for str={}", array, string);
        for (idx, char) in string.chars().enumerate()
        {
            self.arrays.borrow_mut()[array].borrow_mut()[idx] = LocalVariable::Char(char)
        }
        array
    }
    pub(crate) fn new_array(&self, arr_type: u8, count: usize) -> usize {
        let arr_initaliser = match arr_type {
            4 => {
                LocalVariable::Boolean(false)
            }
            5 => {
                LocalVariable::Char('\0')
            }
            6 => {
                LocalVariable::Float(0.0)
            }
            7 => {
                LocalVariable::Double(0.0)
            }
            8 => {
                LocalVariable::Byte(0)
            }
            9 => {
                LocalVariable::Short(0)
            }
            10 => {
                LocalVariable::Int(0)
            }
            11 => {
                LocalVariable::Long(0)
            }
            12 => {
                LocalVariable::Reference(Null())
            }
            _ => { panic!("no"); }
        };
        let arr = vec![arr_initaliser; count + 1];
        println!("Initialising array, idx={}, type={}, count={}", self.arrays.borrow().len(), arr_type, &arr.len());
        self.arrays.borrow_mut().push(RefCell::new(arr));
        return self.arrays.borrow().len() - 1;
    }

    pub(crate) fn get_class_impl(&self, class_name: &str) -> Option<Rc<Class>>
    {
        if let Some(class) = (*self.classes).borrow().get(class_name) {
            return Some(class.clone());
        }
        if class_name.contains("Error") || class_name.contains("Exception")
        {
            panic!("{} occurred", class_name);
        }
        let full_class_name = format!("{}.class", class_name);
        let full_class_path = Path::new(full_class_name.as_str());
        let file = self.classpath.join(full_class_path);
        if file.is_file() {
            let new_class = FileReader::read_class_from_file(file.as_os_str().to_str()?);
            if new_class.is_ok() {
                let ok_class = new_class.unwrap().clone();
                return Some(ok_class.clone());
            }
        }
        None
    }

    pub(crate) fn find_field(self: &Self, obj_ref: &Rc<Object>, method_name: &str, method_type: &str) -> Result<LocalVariable, String>
    {
        let field_ref = obj_ref.fields.borrow();
        let field_val = field_ref.get(method_name).cloned();
        if field_val.is_some()
        {
            return Ok(field_val.unwrap().clone());
        }
        if let Some(t) = self.native_fields.get(&*VM::make_native_method_name(&obj_ref.class.name, method_name))
        {
            return Ok(t.clone());
        }
        let super_instance = obj_ref.super_instance.borrow().clone();
        if super_instance.is_none()
        {
            return Err(format!("Field {}({}) not found in {:?}", method_name, method_type, obj_ref));
        }
        let obj_map_ref = self.objects.borrow();
        let super_instance_ref = obj_map_ref.get(super_instance.unwrap()).ok_or(format!("Null pointer exception with superclass {}", super_instance.unwrap()))?;
        return dbg!(self.find_field(super_instance_ref, method_name, method_type));
    }
    pub(crate) fn get_class(&self, class_name: &str) -> Option<Rc<Class>>
    {
        if class_name.is_empty()
        {
            return None;
        }
        dbg!("Loading class: ", class_name);
        let maybe_class = block_on(self.cl.defer_load_class(class_name));
        if maybe_class.is_none()
        {
            return None;
        }
        let class = maybe_class.unwrap();
        let mut maybe_super_class = None;

        maybe_super_class = block_on(self.cl.defer_load_class(&class.super_class));

        // TODO: static initialisers and class initialisers
        Some(class)
    }


    pub fn start(vm: Rc<VM>, main_class: String)
    {
        let class = block_on(vm.cl.defer_load_class(&main_class)).unwrap();
        let init = Frame::new(&vm, &vm.get_class("java/lang/System").unwrap(), "initPhase1", vec![], "()V").unwrap().exec();
        if let Err(msg) = init {
            panic!(msg);
        }

        let frame = Frame::new(&vm, &class, "main", vec![], "([Ljava/lang/String;)V");

        match frame.unwrap().exec() {
            Ok(res) => { println!("Result of {}: {:#?}", &class.name, res); }
            Err(msg) => { panic!(msg); }
        }
    }
}

impl DeferredClassLoader {
    pub(crate) fn new(classpath: PathBuf) -> Self {
        Self { classpath, classes: Default::default() }
    }

    pub(crate) fn get_class_impl(classpath: &PathBuf, class_name: &str) -> Option<Rc<Class>>
    {
        println!("Loading {}", class_name);

        if class_name.contains("Error") || class_name.contains("Exception")
        {
            panic!("{} occurred", class_name);
        }
        let full_class_name = format!("{}.class", class_name);
        let full_class_path = Path::new(full_class_name.as_str());
        let file = classpath.join(full_class_path);
        println!("Loading {} from {:?}", class_name, file);
        if file.is_file() {
            println!("Loaded {} from {:?}", class_name, file);
            let new_class = FileReader::read_class_from_file(file.as_os_str().to_str()?);
            if new_class.is_ok() {
                let ok_class = new_class.unwrap().clone();
                return Some(ok_class.clone());
            }
        }
        None
    }

    pub(crate) async fn defer_load_class(&self, class_name: &str) -> Option<Rc<Class>>
    {
        if let Some(class) = self.classes.lock().unwrap().get(class_name)
        {
            println!("Class was loaded {}", class_name);
            return Some(class.clone());
        }
        let loaded_class = DeferredClassLoader::get_class_impl(&self.classpath, class_name).clone();
        if let Some(class) = loaded_class {
            {
                self.classes.lock().unwrap().insert(class_name.into(), class.clone());
            }
            let map = self.classes.clone().clone();
            let super_class = class.clone().super_class.clone();
            let super_loaded = DeferredClassLoader::get_class_impl(&self.classpath, super_class.as_str());
            if let Some(s) = super_loaded {
                map.lock().unwrap().insert(super_class, s.clone());
            }
            ;
            return Some(class);
        }
        None
    }

    pub(crate) fn insert_class(&self, class_name: &str, class: Rc<Class>)
    {
        self.classes.lock().unwrap().insert(class_name.into(), class.clone());
    }
}
