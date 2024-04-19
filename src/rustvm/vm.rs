use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use anyhow::anyhow;

use crate::rustvm::class::{Class, ClassRef, Object};
use crate::rustvm::enums::{Attribute, ConstantPool, Field, FLAG_ABSTRACT, FLAG_FINAL, FLAG_NATIVE, FLAG_PUBLIC, FLAG_STATIC, FLAG_SUPER, FLAG_SYNTHETIC, Flags, LocalVariable};
use crate::rustvm::enums::ConstantPool::Empty;
use crate::rustvm::enums::ReferenceKind::{ArrayReference, ClassReference, Null, ObjectReference};
use crate::rustvm::frame::{ExecutionResult, Frame};

pub struct VM
{
    classpath: PathBuf,
    classes: Box<HashMap<String, ClassRef>>,
    pub(crate) objects: Box<Vec<Object>>,
    pub(crate) native_methods: NativeMethods,
    pub(crate) native_fields: HashMap<String, LocalVariable>,
    pub(crate) arrays: Box<Vec<Box<Vec<LocalVariable>>>>,
    cl: DeferredClassLoader,
}

pub(crate) type NativeMethods = HashMap<String, Rc<dyn Fn(ClassRef, &mut Frame, &[LocalVariable], &mut VM) -> Result<LocalVariable, anyhow::Error>>>;

pub(crate) struct FileReader
{
    file: File,
    constant_pool: Vec<ConstantPool>,
}

struct DeferredClassLoader
{
    classpath: PathBuf,
    classes: Box<HashMap<String, ClassRef>>,
}

impl FileReader {
    pub fn read_class_from_file(file_name: &str) -> Result<ClassRef, anyhow::Error>
    {
        let file = File::open(file_name);
        if file.is_err() {
            return Err(anyhow!("Couldn't open file: {:?}", file.err()));
        }
        let mut ret = Self { file: file.unwrap(), constant_pool: vec![] };

        return Ok(ret.read_class());
    }

    fn resolve_constant_pool(&mut self) -> Vec<ConstantPool>
    {
        let pool_size = self.read_u16() as usize;
        let mut constant_pool: Vec<ConstantPool> = vec![Empty(); pool_size + 1];
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
            //println!("Found this field '{}'", name);
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
        attributes.reserve(attributes_count as _);
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
    fn read_class(&mut self) -> ClassRef
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
        Box::new(Class::new(
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
    pub fn new(file_name: &str, class_path: &str) -> Result<VM, anyhow::Error>
    {
        let classpath = Path::new(class_path);


        let mut ret = Self {
            classes: Box::new(HashMap::new()),
            objects: Box::new(vec![]),
            native_methods: (HashMap::new()),
            classpath: classpath.to_path_buf(),
            arrays: Box::new(vec![]),
            cl: DeferredClassLoader::new(classpath.to_path_buf()),
            native_fields: Default::default(),
        };

        let main_class = FileReader::read_class_from_file(file_name)?;
        ret.cl.insert_class(&main_class.name.clone(), main_class);

        ret.native_methods.insert(VM::make_native_method_name("java/lang/Object", "getClass"), Rc::new(|_class, _frame, variables, vm| {
            if let LocalVariable::Reference(ObjectReference(idx)) = variables[0]
            {
                let objects = &vm.objects;
                let obj = objects.get(idx).unwrap().class.name.clone();

                return Ok(LocalVariable::Reference(ClassReference(obj)));
            } else {
                panic!("Wtf")
            }
        }));
        ret.native_methods.insert(VM::make_native_method_name("java/lang/Object", "hashCode"), Rc::new(|_class, _frame, variables, _| {
            if let LocalVariable::Reference(ObjectReference(idx)) = variables[0] {
                return Ok(LocalVariable::Int(idx as i32 * 3));
            } else {
                panic!("Wtf")
            }
        }));
        ret.native_methods.insert(VM::make_native_method_name("java/lang/Class", "getName"), Rc::new(|_class, _frame, variables, vm| {
            if let LocalVariable::Reference(ClassReference(idx)) = &variables[0]
            {
                let obj = vm.new_object("java/lang/String");
                let string_class = vm.get_class("java/lang/String").unwrap();
                let array = vm.new_string(&idx);
                let res = Frame::new(string_class, "<init>",
                                     [LocalVariable::Reference(ObjectReference(obj)), LocalVariable::Reference(ArrayReference(array))].to_vec()
                                     , "([C)V", vm)?.exec(vm);
                println!("getName returned {:?}", res);
                if res.is_err()
                {
                    panic!("Error occurred when running getName, err={}", res.err().unwrap());
                }
                return Ok(LocalVariable::Reference(ObjectReference(obj)));
            }
            return Err(anyhow::Error::msg("Unexpected argument"));
        }));
        ret.native_methods.insert(VM::make_native_method_name("java/lang/System", "arraycopy"), Rc::new(|_class, _frame, variables, vm| {
            let src = variables[0].clone();
            let _start = variables[1].clone().to_int() as usize;
            let dst = variables[2].clone();
            let end = variables[3].clone().to_int() as usize;
            let len = variables[4].clone().to_int() as usize;
            if let LocalVariable::Reference(ArrayReference(src_ref)) = src
            {
                if let LocalVariable::Reference(ArrayReference(dst_ref)) = dst
                {
                    let borrowed_arrays = &mut vm.arrays;
                    let src_arr = borrowed_arrays.get(src_ref).ok_or(anyhow!("Source did not exist"))?.clone();
                    let mut dst_arr = borrowed_arrays.get_mut(dst_ref).ok_or(anyhow!("Destination did not exist"))?.clone();

                    let mut count = end as usize;
                    for item in *src_arr
                    {
                        if count == len - 1
                        {
                            break;
                        }
                        dst_arr[end + count] = item;
                        count += 1;
                    }
                } else {
                    return Err(anyhow!("Dest was not an array {:?} in {:?}", dst, variables));
                }
            } else {
                return Err(anyhow!("Source was not an array {:?} in {:?}", src, variables));
            }

            return Ok(LocalVariable::Void());
        }));
        ret.native_methods.insert(VM::make_native_method_name("jdk/internal/util/SystemProps$Raw", "platformProperties"), Rc::new(|_class, _frame, _variables, vm|
            {
                println!("Called system props");
                let arr_idx = vm.new_array(12, 38);
                Ok(LocalVariable::Reference(ArrayReference(arr_idx)))
            }));
        ret.native_methods.insert(VM::make_native_method_name("jdk/internal/util/SystemProps$Raw", "vmProperties"), Rc::new(|_class, _frame, _variables, vm|
            {
                println!("Called VM props");
                let arr_idx = vm.new_array(12, 10);
                Ok(LocalVariable::Reference(ArrayReference(arr_idx)))
            }));
        Ok(ret)
    }

    pub(crate) fn make_native_method_name(class_name: &str, method_name: &str) -> String
    {
        format!("{}.{}", class_name, method_name).to_string()
    }


    fn type_to_class(t: &str) -> &str
    {
        if t.chars().nth(0) != Some('L') {
            return t;
        }
        return &t[1..t.len() - 1];
    }

    fn default_initialiser(&mut self, descriptor: &str, is_final: bool) -> LocalVariable
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
                    LocalVariable::Reference(ObjectReference(self.new_object(VM::type_to_class(descriptor))))
                } else {
                    LocalVariable::Reference(Null())
                }
            }
            'Z' => {
                LocalVariable::Boolean(false)
            }
            '[' =>
                {
                    LocalVariable::Reference(Null())
                }
            _ => {
                panic!("descriptor {}", descriptor);
            }
        }
    }

    pub(crate) fn new_object(&mut self, class_name: &str) -> usize
    {
        let mut class = self.get_class(&class_name).unwrap();
        let mut fields = HashMap::new();
        for field in &class.fields {
            if field.flags & (FLAG_SYNTHETIC | FLAG_FINAL | FLAG_STATIC) == (FLAG_SYNTHETIC | FLAG_FINAL | FLAG_STATIC) || field.flags & FLAG_NATIVE | FLAG_FINAL | FLAG_STATIC == (FLAG_NATIVE | FLAG_FINAL | FLAG_STATIC) {
                if field.name == "$assertionsDisabled"
                {
                    class.static_fields.insert(field.name.clone(), LocalVariable::Boolean(true));
                } else {
                    class.static_fields.insert(field.name.clone(), self.default_initialiser(field.descriptor.as_str(), true));
                }
            }
            if field.flags & FLAG_FINAL | FLAG_STATIC == FLAG_FINAL | FLAG_STATIC && VM::type_to_class(&field.descriptor) != class_name
            {
                for attrib in &field.attributes {
                    if attrib.name == "ConstantValue" {
                        let mut buf = [0u8; 2];
                        for idx in 0..=1 {
                            buf[idx] = attrib.data[0 + idx]
                        }
                        let const_idx: u16 = u16::from_be_bytes(buf);
                        if let Some(cp) = class.constant_pool.get(const_idx as usize) {
                            class.static_fields.insert(field.name.clone(), LocalVariable::from_constant_pool(cp.clone()));
                        }
                        continue;
                    }
                }
                class.static_fields.insert(field.name.clone(), self.default_initialiser(&field.descriptor, true));
            } else {
                fields.insert(field.name.clone(), self.default_initialiser(&field.descriptor, false));
            }
        }
        self.objects.push(Object {
            class: class.clone(),
            super_instance: Box::new(None),
            fields: Box::new(fields),
        });
        self.objects.len() - 1
    }

    pub(crate) fn set_array_element(&mut self, array: usize, idx: usize, elem: LocalVariable) {
        println!("The array is {:?}", self.arrays[array]);
        self.arrays[array][idx] = elem;
    }

    pub(crate) fn new_string(&mut self, string: &str) -> usize {
        let array = self.new_array(5, string.len());
        println!("Creating new string array, idx={} for str={}", array, string);
        for (idx, char) in string.chars().enumerate()
        {
            self.arrays[array][idx] = LocalVariable::Char(char)
        }
        array
    }
    pub(crate) fn new_array(&mut self, arr_type: u8, count: usize) -> usize {
        let arr_initializer = match arr_type {
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
        let arr = vec![arr_initializer; count + 1];
        println!("Initialising array, idx={}, type={}, count={}", self.arrays.len(), arr_type, &arr.len());
        self.arrays.push(Box::new(arr));
        return self.arrays.len() - 1;
    }

    pub(crate) fn find_field(&self, obj_ref: usize, method_name: &str, method_type: &str) -> Result<LocalVariable, anyhow::Error>
    {
        let object = self.objects.get(obj_ref).ok_or(anyhow!("Object did not exist"))?;
        let field_ref = &object.fields;
        let field_val = &field_ref.get(method_name);
        if field_val.is_some()
        {
            return Ok(field_val.unwrap().clone());
        }
        if let Some(t) = self.native_fields.get(&*VM::make_native_method_name(&object.class.name, method_name))
        {
            return Ok(t.clone());
        }
        let super_instance = object.super_instance.clone();
        if super_instance.is_none()
        {
            return Err(anyhow!("Field {}({}) not found in {:?}", method_name, method_type, obj_ref));
        }
        return dbg!(self.find_field(super_instance.unwrap(), method_name, method_type));
    }
    pub(crate) fn get_class(&mut self, class_name: &str) -> Result<ClassRef, anyhow::Error>
    {
        if class_name.is_empty()
        {
            return Err(anyhow!("Empty class name!"));
        }
        dbg!("Loading class: ", class_name);
        let class = self.cl.load_class(class_name)?;
        if !class.super_class.is_empty()
        {
            self.cl.load_class(&class.super_class)?;
        }
        // TODO: static initialisers and class initialisers
        Ok(class)
    }

    pub(crate) fn create_superclass(&mut self, this_idx: usize, class_name_ref: &str) -> Result<LocalVariable, anyhow::Error>
    {
        let super_instance_idx: usize = self.new_object(class_name_ref);

        let this_instance = self.objects.get_mut(this_idx).unwrap();
        this_instance.put_super_instance(super_instance_idx)?;
        return Ok(LocalVariable::Reference(ObjectReference(this_instance.get_super_instance().unwrap())));
    }

    pub fn start(&mut self, main_class: String) -> Result<Option<LocalVariable>, anyhow::Error>
    {
        let mut frame_stack: VecDeque<Frame> = VecDeque::new();
        let init_class = self.get_class("java/lang/System").unwrap();
        frame_stack.push_front(Frame::new(init_class, "initPhase1", vec![], "()V", self).unwrap());
        let class = self.cl.load_class(&main_class).unwrap();
        frame_stack.push_back(Frame::new(class, "main", vec![], "([Ljava/lang/String;)V", self).unwrap());
        let mut maybe_result: Option<LocalVariable> = None;
        while !frame_stack.is_empty()
        {
            let frame = frame_stack.front_mut().unwrap();
            if let Some(to_be_put) = &maybe_result {
                match to_be_put {
                    LocalVariable::Void() => {}
                    it => { frame.stack.push_front(it.clone()) }
                }
                maybe_result = None;
            }
            let result = match frame.will_execute_native_method {
                false => {
                    frame.exec(self)
                }
                true => {
                    frame.exec_native(self)
                }
            };

            match result? {
                ExecutionResult::FunctionCallResult(ret) => {
                    frame_stack.pop_front();
                    maybe_result = Some(ret);
                }
                ExecutionResult::Invoke(f) => {
                    frame_stack.push_front(f);
                }
            }
        }
        return Ok(maybe_result);
    }
}

impl DeferredClassLoader {
    pub(crate) fn new(classpath: PathBuf) -> Self {
        Self { classpath, classes: Default::default() }
    }

    pub(crate) fn get_class_impl(classpath: &PathBuf, class_name: &str) -> Result<ClassRef, anyhow::Error>
    {
        println!("Loading {}", class_name);

        if class_name.contains("Error") || class_name.contains("Exception")
        {
            return Err(anyhow!("Error or exception occurred, exception={}", class_name));
        }
        let full_class_name = format!("{}.class", class_name);
        let full_class_path = Path::new(full_class_name.as_str());
        let file = classpath.join(full_class_path);
        println!("Loading {} from {:?}", class_name, file);
        if file.is_file() {
            println!("Loaded {} from {:?}", class_name, file);
            return FileReader::read_class_from_file(file.as_os_str().to_str().ok_or(anyhow!("Couldn't convert file into string, file={}", file.display()))?);
        }
        Err(anyhow!("Class {} not found", class_name))
    }

    pub(crate) fn load_class(&mut self, class_name: &str) -> Result<ClassRef, anyhow::Error>
    {
        if let Some(class) = self.classes.get(class_name)
        {
            println!("Class was loaded {}", class_name);
            return Ok(class.clone());
        }
        let loaded_class = DeferredClassLoader::get_class_impl(&self.classpath, class_name)?;
        self.classes.insert(class_name.into(), loaded_class.clone());
        if !loaded_class.super_class.is_empty()
        {
            let super_class = loaded_class.super_class.clone();
            let super_loaded = DeferredClassLoader::get_class_impl(&self.classpath, super_class.as_str())?;
            self.classes.insert(super_class, super_loaded.clone());
        }
        Ok(loaded_class)
    }

    pub(crate) fn insert_class(&mut self, class_name: &str, class: ClassRef)
    {
        self.classes.insert(class_name.into(), class.clone());
    }
}
