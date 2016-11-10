use lexer::{Tokenizer, TokenType, Token, TokenIterator};
use parser;

pub enum InterpretError {
    Error,
}

pub trait Visitor<T> {
    fn visit_expr(&mut self, e: &parser::Expr) ->T;
}

pub struct Interpreter{
}

impl Interpreter {
    pub fn interpret(&mut self, e: &parser::Expr) -> f64 {
        self.visit_expr(e)
    }
}

impl Visitor<f64> for Interpreter {
    fn visit_expr(&mut self, e: &parser::Expr) -> f64 {
        match *e{
            parser::Expr::Num(v) => v,
            parser::Expr::BinOp(t, ref left, ref right) => {
                match t{
                    TokenType::Plus => self.visit_expr(left) + self.visit_expr(right),
                    TokenType::Minus => self.visit_expr(left) - self.visit_expr(right),
                    TokenType::Mul => self.visit_expr(left) * self.visit_expr(right),
                    TokenType::Div => self.visit_expr(left) / self.visit_expr(right),
                    _ => panic!("Unexpected token type"),
                }
            }
        }
    }
}