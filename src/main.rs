mod lexer;
mod interpreter;

use interpreter::Interpreter;
use std::io;

fn main() {   
    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Fail to read");
        let mut inter = Interpreter::new(&input);
        println!("Result is: {0}", inter.expr().expect("Evaluate failed"));
    }
    
}
