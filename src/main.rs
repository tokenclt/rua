extern crate clap;
#[cfg(test)]
extern crate tempdir;

use clap::{App, Arg};
use std::io::prelude::*;
use std::fs::File;

use compiler::Compiler;

mod lexer;
mod parser;
mod ir_generator;
mod bytecode_generator;
mod assembler;
mod compiler;

fn main() {
    let matches = App::new("rua")
        .version("0.1")
        .author("Mingyu Zhou <zhoumy46@gmail.com>")
        .about("A toy Lua compiler in Rust")
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("FILE")
                .help("Sets where to write bytecode")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input source file")
                .required(true)
                .index(1),
        )
        .get_matches();

    let input_file = matches.value_of("INPUT").unwrap();
    let output_file = matches.value_of("output").unwrap_or("a.out");
    let bytecode = Compiler::from_file(input_file);

    let mut file = File::create(output_file).expect("Failed to open file to write");
    file.write_all(&bytecode).expect("Failed to write bytecode");
    file.sync_all().expect("Failed to flush file");
}
