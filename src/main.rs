mod lexer;
mod parser;
// mod semantic_analyzer;
mod interpreter;
mod ir_generator;
mod assembler;

use assembler::tests;
fn main() {
    tests::empty()
}