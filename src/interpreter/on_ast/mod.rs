use interpreter::{Variable, InterpretError};
use parser::types::{Expr, Node, Stat, Var, Block};
use lexer::tokens::FlagType;
use compiler::symbol_table::ScopedSymbolTableBuilder;

use std::collections::HashMap;

pub struct Interpreter {
    global_variables: HashMap<String, f64>,
}

// Public interface
#[allow(dead_code)]
impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { global_variables: HashMap::new() }
    }

    pub fn interpret(&mut self, n: &Node) -> Result<(), InterpretError> {
        self.visit_block(n)
    }

    pub fn print_symble_table(&self) {
        println!("{0:?}", self.global_variables);
    }
}

//  Visit method
#[allow(dead_code)]
impl Interpreter {
    fn visit_expr(&mut self, e: &Expr) -> Result<f64, InterpretError> {
        match *e {
            Expr::Num(v) => Ok(v),
            Expr::BinOp(t, ref left, ref right) => {
                match t {
                    FlagType::Plus => {
                        Ok(try!(self.visit_expr(left)) + try!(self.visit_expr(right)))
                    }

                    FlagType::Minus => {
                        Ok(try!(self.visit_expr(left)) - try!(self.visit_expr(right)))
                    }

                    FlagType::Mul => Ok(try!(self.visit_expr(left)) * try!(self.visit_expr(right))),

                    FlagType::Div => Ok(try!(self.visit_expr(left)) / try!(self.visit_expr(right))),

                    _ => Err(InterpretError::Error),
                }
            }
            Expr::UnaryOp(t, ref child) => {
                match t {
                    FlagType::Minus => Ok(-try!(self.visit_expr(child))),
                    FlagType::Plus => Ok(try!(self.visit_expr(child))),
                    _ => Err(InterpretError::Error),
                }
            }
            Expr::Var(ref var) => self.visit_var(var),
            _ => unimplemented!(),

        }
    }

    fn visit_var(&mut self, v: &Var) -> Result<f64, InterpretError> {
        match *v {
            Var::Name(ref id) => {
                match self.global_variables.get(id) {
                    Some(&v) => Ok(v),
                    None => Err(InterpretError::UndefinedVariable),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn visit_block(&mut self, n: &Node) -> Result<(), InterpretError> {
        match *n {
            Node::Block(Block { ref stats, .. }) => {
                for stat in stats {
                    try!(self.visit_stat(stat));
                }
                Ok(())
            }
            _ => Err(InterpretError::Error),
        }
    }

    fn visit_stat(&mut self, stat: &Stat) -> Result<(), InterpretError> {
        match *stat {
            Stat::Assign(ref varlist, ref exprlist) => {
                for (var, expr) in varlist.into_iter().zip(exprlist.into_iter()) {
                    if let &Var::Name(ref name) = var {
                        let result = try!(self.visit_expr(expr));
                        let v = self.global_variables.entry(name.clone()).or_insert(result);
                        *v = result;
                    } else {
                        panic!("Do not support non Name as Var");
                    }
                }
                Ok(())
            }
            Stat::Empty => Ok(()),
            _ => unimplemented!(),
        }
    }

    fn visit_if_else(&mut self, condition: &Expr, then_block: &Node) -> Result<(), InterpretError> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;
    use parser::Parser;
    use lexer::Lexer;

    #[test]
    fn test() {
        let text = String::from("\
    --This a comment 
    a, number = 0, 1
    _b = number * 10 - 2*(3+4)
    a = a + _b

    ");
        let lexer = Lexer::new();
        let mut itpt = Interpreter::new();
        let out = itpt.interpret(&Parser::parse(lexer.tokenize(text.chars())).unwrap());
        out.unwrap();
        assert_eq!(itpt.global_variables.get("a"), Some(&-4f64));
        assert_eq!(itpt.global_variables.get("_b"), Some(&-4f64));
        assert_eq!(itpt.global_variables.get("number"), Some(&1f64));
    }
}