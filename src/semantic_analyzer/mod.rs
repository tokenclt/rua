use parser::types::{Node, Expr, Var, Stat, Block};
use lexer::tokens::FlagType;
use compiler::symbol_table::*;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolError {
    UndefinedSymbol(String),
    TypeMismatched,
    Error,
}

#[allow(dead_code)]
pub struct SemanticAnalyzer {
    symbol_table: ScopedSymbolTableBuilder,
}

#[allow(dead_code)]
impl SemanticAnalyzer {
    pub fn new() -> SemanticAnalyzer {
        SemanticAnalyzer { symbol_table: ScopedSymbolTableBuilder::new() }
    }

    pub fn visit(&mut self, n: &Node) -> Result<(), SymbolError> {
        self.visit_block(n).and(Ok(())) // clear Ok
    }

    pub fn lookup(&self, name: &str) -> Option<SymbolType> {
        self.symbol_table.lookup(name)
    }

    fn type_consistent<'a>(lhs: &'a SymbolType, rhs: &'a SymbolType) -> (bool, &'a SymbolType) {
        if *lhs == SymbolType::UnKnow {
            (true, lhs)
        } else if *rhs == SymbolType::UnKnow {
            (true, rhs)
        } else if *lhs == *rhs {
            (true, lhs)
        } else {
            (false, lhs)
        }
    }
}

#[allow(dead_code)]
impl SemanticAnalyzer {
    fn visit_block(&mut self, n: &Node) -> Result<SymbolType, SymbolError> {
        self.symbol_table.initialize_scope();
        let result = match *n {
            Node::Block(Block{ref stats, ref ret}) => {
                for stat in stats {
                    try!(self.visit_stat(stat));
                }
                // return null tuple and no return stat are Nil
                if let &Some(ref exprlist) = ret {
                    Ok(if exprlist.len() > 0 {
                        let mut types = Vec::with_capacity(exprlist.len());
                        for expr in exprlist {
                            types.push(try!(self.visit_expr(expr)));
                        }
                        SymbolType::Tuple(types)
                    } else {
                        SymbolType::Nil
                    })
                } else {
                    Ok(SymbolType::Nil)
                }
            }
            _ => Err(SymbolError::Error),
        };
        self.symbol_table.finalize_scope();
        result
    }

    fn visit_stat(&mut self, stat: &Stat) -> Result<(), SymbolError> {
        match *stat {
            Stat::Assign(ref varlist, ref exprlist) => {
                for (&Var::Name(ref var), expr) in varlist.into_iter().zip(exprlist.into_iter()) {
                    let result_type = try!(self.visit_expr(expr));
                    self.symbol_table.define_global(var, result_type);
                }
                Ok(())
            }
            Stat::AssignLocal(ref namelist, ref exprlist) => {
                unimplemented!()
            }
            Stat::IfElse(ref condition, ref then_node, ref else_node) => {
                try!(self.visit_expr(condition).and_then(|t| if t == SymbolType::Boole {
                    Ok(())
                } else {
                    Err(SymbolError::TypeMismatched)
                }));
                try!(self.visit_block(then_node));
                try!(self.visit_block(then_node));
                if let &Some(ref block) = else_node {
                    try!(self.visit_block(block));
                }
                Ok(())

            }
            Stat::Empty => Ok(()),
            Stat::Break => Ok(()),
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
                match op_type {
                    FlagType::AND | FlagType::OR => Ok(SymbolType::UnKnow),
                    FlagType::EQ | FlagType::NEQ => {
                        if let (true, _) = Self::type_consistent(&left_type, &right_type) {
                            Ok(SymbolType::Boole)
                        } else {
                            Err(SymbolError::TypeMismatched)
                        }
                    }
                    _ => {
                        if let (true, t) = Self::type_consistent(&left_type, &right_type) {
                            Ok(t.clone())
                        } else {
                            Err(SymbolError::TypeMismatched)
                        }
                    }

                }
            }
            Expr::UnaryOp(_, ref child) => self.visit_expr(child),
            Expr::Var(ref var) => self.visit_var(var),
            Expr::Str(_) => Ok(SymbolType::Str),
            Expr::Boole(_) => Ok(SymbolType::Boole),
            Expr::FunctionDef(..) => unimplemented!(),
        }
    }

    fn visit_var(&mut self, v: &Var) -> Result<SymbolType, SymbolError> {
        match *v {
            Var::Name(ref id) => {
                self.symbol_table.lookup(id).ok_or(SymbolError::UndefinedSymbol(id.clone()))
            }
        }
    }

    // if expr, then then_block else else_block
    fn visit_if_else(&mut self,
                     condition: &Expr,
                     then_block: &Node,
                     else_block: &Option<Node>)
                     -> Result<(), SymbolError> {
        let expr_type = try!(self.visit_expr(condition));
        if expr_type != SymbolType::Boole {
            return Err(SymbolError::TypeMismatched);
        }

        try!(self.visit_block(then_block));
        if let &Some(ref e) = else_block {
            try!(self.visit_block(e));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use parser::Parser;
    use lexer::Lexer;
    use semantic_analyzer::*;
    use compiler::symbol_table::*;

    #[test]
    fn test_type_check() {
        let text = String::from("\
        a, str = 1, 'abcabc' 
        b = a + str
        ");

        let lexer = Lexer::new();
        let mut sym1 = SemanticAnalyzer::new();
        let ast1 = lexer.tokenize(text.chars());
        let mut result = sym1.visit(&Parser::parse(ast1).unwrap());
        println!("{:?}", result);
        assert_eq!(result, Err(SymbolError::TypeMismatched));

        let text2 = String::from("\
        boole_a = (1+2) == 3
        unknow = \"abc\" ~= \"str\" and boole_a
        b =  boole_a + 2
        ");
        let mut sym2 = SemanticAnalyzer::new();
        let ast2 = lexer.tokenize(text2.chars());
        result = sym2.visit(&Parser::parse(ast2).unwrap());
        assert_eq!(Some(SymbolType::Boole), sym2.lookup("boole_a"));
        assert_eq!(Some(SymbolType::UnKnow), sym2.lookup("unknow"));
        assert_eq!(result, Err(SymbolError::TypeMismatched));
    }

    #[test]
    fn test_if_else() {}
}