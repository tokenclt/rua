mod lexer;
mod parser;
mod interpreter;

use std::io;

fn main() {
    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Fail to read");
        let mut ps = parser::Parser::new(&input);
        let mut itpr = interpreter::Interpreter{};
        match ps.parse() {
            Ok(ref ex) => {
                let result = itpr.interpret(ex);
                println!("Result is: {0}", result);
            },
            Err(_) => println!("Parse failed"),
        }

    }
}
