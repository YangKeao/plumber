extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate clap;

use pest::Parser;
use clap::{App, Arg};

#[derive(Parser)]
#[grammar = "../grammar/plumber.pest"]
struct PlumberParser;

fn main() {
    let matches = App::new("Plumber")
        .author("YangKeao <keao.yang@yahoo.com>")
        .arg(Arg::with_name("INPUT")
            .required(true)
            .index(1))
        .get_matches();

    let input_file = std::fs::read_to_string(matches.value_of("INPUT").unwrap()).unwrap();
    let ast = PlumberParser::parse(Rule::program, &input_file).unwrap_or_else(|e| panic!("{}", e));
    println!("{:?}", ast);
}
