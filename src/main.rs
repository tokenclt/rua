mod lexer;
mod parser;
mod interpreter;
mod arith_playground;

use std::io;

fn evaluator() {
loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Fail to read");
        let mut ps = parser::Parser::new(&input);
        let mut itpr = interpreter::Interpreter {};
        match ps.parse() {
            Ok(ref ast) => {
                let result = itpr.interpret(ast);
                println!("Result is: {0}", result);
            }
            Err(_) => println!("Parse failed"),
        }

    }
}

fn rpn_printer() {
    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Fail to read");
        let mut converter = arith_playground::RpnConverter{};
        converter.print_rpn(&input);
    }
}

fn main() {
    evaluator();
}
