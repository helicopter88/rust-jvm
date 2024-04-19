mod rustvm;

use rustvm::vm::VM;


use std::env;

fn main() -> Result<(), anyhow::Error> {
    let args: Vec<_> = env::args().collect();
    if args.len() != 3 { panic!("Wrong number of arguments, expected file and classpath"); }
    let myfile = &args[1];
    let mycp = &args[2];
    let mut vm = VM::new(myfile, mycp).unwrap();
    let res = VM::start(&mut vm, "HelloWorld".to_string());
    if res.is_ok()
    {
        println!("{:#?}", res);
        Ok(())
    } else {
        Err(res.err().unwrap())
    }
}
