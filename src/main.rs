mod rustvm;

use rustvm::vm::VM;


use std::env;

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 3 { panic!("Wrong number of arguments, expected file and classpath"); }
    let myfile = &args[1];
    let mycp = &args[2];
    let mut vm = VM::new(myfile, mycp).unwrap();
    let result = VM::start(&mut vm, "HelloWorld".to_string());
    match result {
        Ok(ret) => {
            println!("Result from computation is: {:#?}", ret.ok_or("Empty"))
        }
        Err(msg) => { panic!("{}", msg) }
    }
}
