use std::collections::HashMap;
use parser::{Node, Expr, Var, Stat};
use lexer::tokens::FlagType;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SymbolType {
    Real,
    Str,
    Boole,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolError {
    UndefinedSymbol(String),
    TypeMismatched,
    Error,
}

#[derive(Debug)]
pub struct SymbolTable {
    symbols: HashMap<String, SymbolType>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable { symbols: HashMap::new() }
    }

    pub fn define(&mut self, name: &str, t: SymbolType) {
        // insert or update
        let slot = self.symbols.entry(name.to_string()).or_insert(t);
        *slot = t;
    }

    pub fn lookup(&self, name: &str) -> Option<SymbolType> {
        self.symbols.get(name).cloned()
    }
}

#[allow(dead_code)]
pub struct SymbolTableBuiler {
    symbol_table: SymbolTable,
}

#[allow(dead_code)]
impl SymbolTableBuiler {
    pub fn new() -> SymbolTableBuiler {
        SymbolTableBuiler { symbol_table: SymbolTable::new() }
    }

    pub fn visit(&mut self, n: &Node) -> Result<(), SymbolError> {
        self.visit_block(n)
    }

    pub fn lookup(&self, name: &str) -> Option<SymbolType> {
        self.symbol_table.lookup(name)
    }
}

#[allow(dead_code)]
impl SymbolTableBuiler {
    fn visit_block(&mut self, n: &Node) -> Result<(), SymbolError> {
        match *n {
            Node::Block(ref stats) => {
                for stat in stats {
                    try!(self.visit_stat(stat));
                }
                Ok(())
            }
            _ => Err(SymbolError::Error),
        }
    }

    fn visit_stat(&mut self, stat: &Stat) -> Result<(), SymbolError> {
        match *stat {
            Stat::Assign(ref varlist, ref exprlist) => {
                for (&Var::Name(ref var), expr) in varlist.into_iter().zip(exprlist.into_iter()) {
                    let result_type = try!(self.visit_expr(expr));
                    self.symbol_table.define(var, result_type);
                }
                Ok(())
            }
            Stat::IfElse(..) => unimplemented!(),
            Stat::Empty => Ok(()),
            _ => unimplemented!(),
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> Result<SymbolType, SymbolError> {
        match *e {
            Expr::Num(_) => Ok(SymbolType::Real),
            Expr::BinOp(op_type, ref left, ref right) => {
                // TODO : check method exist
                let left_type = try!(self.visit_expr(left));
                let right_type = try!(self.visit_expr(right));
                if left_type == right_type {
                    match op_type {
                        FlagType::AND | FlagType::OR | FlagType::EQ | FlagType::NEQ => {
                            Ok(SymbolType::Boole)
                        }
                        _ => Ok(left_type),
                    }
                } else {
                    Err(SymbolError::TypeMismatched)
                }
            }
            Expr::UnaryOp(_, ref child) => self.visit_expr(child),
            Expr::Var(ref var) => self.visit_var(var),
            Expr::Str(_) => Ok(SymbolType::Str),
            Expr::Boole(_) => Ok(SymbolType::Boole),
        }
    }

    fn visit_var(&mut self, v: &Var) -> Result<SymbolType, SymbolError> {
        match *v {
            Var::Name(ref id) => {
                self.symbol_table.lookup(id).ok_or(SymbolError::UndefinedSymbol(id.clone()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use parser::Parser;
    use lexer::Lexer;
    use interpreter::symbols::*;

    #[test]
    fn test_type_check() {
        let text = String::from("\
        a, str = 1, 'abcabc' 
        b = a + str
        ");

        let lexer = Lexer::new();
        let mut sym1 = SymbolTableBuiler::new();
        let ast1 = lexer.tokenize(text.chars());
        let mut result = sym1.visit(&Parser::parse(ast1).unwrap());
        println!("{:?}", result);
        assert_eq!(result, Err(SymbolError::TypeMismatched));

        let text2 = String::from("\
        boole_a = (1+2) == 3
        boole_b = \"abc\" ~= \"str\" and boole_a
        b =  boole_a + 2
        ");
        let mut sym2 = SymbolTableBuiler::new();
        let ast2 = lexer.tokenize(text2.chars());
        result = sym2.visit(&Parser::parse(ast2).unwrap());
        assert_eq!(Some(SymbolType::Boole), sym2.lookup("boole_a"));
        assert_eq!(Some(SymbolType::Boole), sym2.lookup("boole_b"));       
        assert_eq!(result, Err(SymbolError::TypeMismatched));
    }
}