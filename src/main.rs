use std::rc::Rc;
use quicli::prelude::*;
use structopt::StructOpt;

use crate::vm::VM;

mod frame;
mod class;
mod enums;
mod vm;

#[derive(Debug, StructOpt)]
struct Cli {
    /// Input file to read
    file: String,
    classpath: String
}



fn main() -> CliResult {
    let matches = Cli::from_args();
    let myfile = &matches.file;
    let mycp = &matches.classpath;
    let mut vm = VM::new(myfile, mycp).unwrap();
    VM::start(&mut vm, "HelloWorld".to_string());
    Ok(())
}
