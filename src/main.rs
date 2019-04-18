extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate clap;

#[macro_use]
mod llvm_utils;

mod codegen;
mod parser;

use clap::{App, Arg};
use parser::*;

fn main() {
    let matches = App::new("Plumber")
        .author("YangKeao <keao.yang@yahoo.com>")
        .arg(Arg::with_name("INPUT").required(true).index(1))
        .arg(Arg::with_name("target").short("t").long("target"))
        .get_matches();

    let file_name = matches.value_of("INPUT").unwrap();
    let input_file = std::fs::read_to_string(file_name).unwrap();
    let target = matches.value_of("target").unwrap_or("x86_64-pc-linux-gnu");

    Plumber::compile(&input_file, target, file_name);
}
