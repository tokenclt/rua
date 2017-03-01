mod lexer;
mod parser;
// mod semantic_analyzer;
mod interpreter;
mod compiler;

use compiler::tests;
fn main() {
    tests::set_get_table();
}
