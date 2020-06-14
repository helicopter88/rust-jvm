
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
}



fn main() -> CliResult {
    let matches = Cli::from_args();
    let myfile = &matches.file;
    let vm = VM::new(myfile);
    VM::start(vm.unwrap(), "Add".to_string());
    Ok(())
}
