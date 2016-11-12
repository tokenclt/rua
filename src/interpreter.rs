use lexer::{Tokenizer, TokenType, Token, TokenIterator};
use parser::{Node, Expr, Var, Stat};
use std::collections::HashMap;

#[derive(Debug)]
pub enum InterpretError {
    UndefinedVariable,
    Error,
}

pub struct Interpreter {
    symbol_table: HashMap<String, f64>,
}

// Public interface
impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { symbol_table: HashMap::new() }
    }
    pub fn interpret(&mut self, n: &Node) {
        self.visit_block(n);
    }
    pub fn print_symble_table(&self) {
        println!("{0:?}", self.symbol_table);
    }
}

//  Visit method
impl Interpreter {
    fn visit_expr(&mut self, e: &Expr) -> Result<f64, InterpretError> {
        match *e {
            Expr::Num(v) => Ok(v),
            Expr::BinOp(t, ref left, ref right) => {
                match t {
                    TokenType::Plus => {
                        Ok(try!(self.visit_expr(left)) + try!(self.visit_expr(right)))
                    }

                    TokenType::Minus => {
                        Ok(try!(self.visit_expr(left)) - try!(self.visit_expr(right)))
                    }

                    TokenType::Mul => {
                        Ok(try!(self.visit_expr(left)) * try!(self.visit_expr(right)))
                    }

                    TokenType::Div => {
                        Ok(try!(self.visit_expr(left)) / try!(self.visit_expr(right)))
                    }

                    _ => Err(InterpretError::Error),
                }
            }
            Expr::UnaryOp(t, ref child) => {
                match t {
                    TokenType::Minus => Ok(-try!(self.visit_expr(child))),
                    TokenType::Plus => Ok(try!(self.visit_expr(child))),
                    _ => Err(InterpretError::Error),
                }
            }
            Expr::Var(ref var) => self.visit_var(var),
        }
    }

    fn visit_var(&mut self, v: &Var) -> Result<f64, InterpretError> {
        match *v {
            Var::Name(ref id) => {
                match self.symbol_table.get(id) {
                    Some(&v) => Ok(v),
                    None => Err(InterpretError::UndefinedVariable),
                }
            }
        }
    }

    fn visit_block(&mut self, n: &Node) -> Result<(), InterpretError> {
        match *n {
            Node::Block(ref stats) => {
                for stat in stats {
                    self.visit_stat(stat);
                }
                Ok(())
            }
            _ => Err(InterpretError::Error),
        }
    }

    fn visit_stat(&mut self, stat: &Stat) -> Result<(), InterpretError> {
        match *stat {
            Stat::Assign(ref varlist, ref exprlist) => {
                for (&Var::Name(ref var), expr) in varlist.into_iter().zip(exprlist.into_iter()) {
                    let result = try!(self.visit_expr(expr));
                    let v = self.symbol_table.entry(var.clone()).or_insert(result);
                    *v = result;
                }
                Ok(())
            }
            Stat::Empty => Ok(()),
        }
    }
}

#[cfg(test)]
mod test {
    use interpreter::Interpreter;
    use parser::Parser;

    #[test]
    fn test() {
        let text = String::from("\
    a, number = 0, 1
    b = number * 10 - 2*(3+4)
    a = a + b

    ");
        let mut ast = Parser::new(&text);
        let mut itpt = Interpreter::new();
        itpt.interpret(&ast.parse().unwrap());
        assert_eq!(itpt.symbol_table.get("a"), Some(&-4f64));
        assert_eq!(itpt.symbol_table.get("b"), Some(&-4f64));
        assert_eq!(itpt.symbol_table.get("number"), Some(&1f64));
    }
}