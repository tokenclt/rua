use lexer::{TokenType, Tokenizer};
use interpreter::{Interpreter, Visitor};
use parser;

pub struct RpnConverter{
}

impl Visitor<()> for RpnConverter {
    fn visit_expr(&mut self, e: &parser::Node) {
        match *e{
            parser::Node::Num(v) => print!("{0} ", v),
            parser::Node::BinOp(t, ref left, ref right) =>{
                match t{
                    TokenType::Plus => {
                        self.visit_expr(left);
                        self.visit_expr(right);
                        print!("+ ");
                    },
                    TokenType::Minus => {
                        self.visit_expr(left);
                        self.visit_expr(right);
                        print!("- ");
                    },
                    TokenType::Mul => {
                        self.visit_expr(left);
                        self.visit_expr(right);
                        print!("* ");
                    },
                    TokenType::Div => {
                        self.visit_expr(left);
                        self.visit_expr(right);
                        print!("/ ");
                    },
                    _ => panic!("Unexpected token type"),
                }
            }
            _ => panic!("Unexpected token type"),
        }
    }
}

impl RpnConverter {
    pub fn print_rpn(&mut self, text: &String) {
        let mut ps = parser::Parser::new(text);
        match ps.parse() {
            Ok(ref ast) => {
                self.visit_expr(ast);
                println!("");
            },
            Err(_) => println!("Parse Error"),
        }
    }
}