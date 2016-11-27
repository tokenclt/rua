use lexer::tokens::{Token, FlagType};
use lexer::{Lexer, TokenIterator};
use self::types::*;
use std::iter;

pub mod types;

pub struct Parser<'a, Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    /// peekable tokenizer
    token_iter: iter::Peekable<TokenIterator<'a, Tit>>,
}

/// Common method
#[allow(dead_code)]
impl<'a, Tit> Parser<'a, Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    pub fn parse(text: TokenIterator<'a, Tit>) -> Result<Box<Node>, ParserError> {
        let mut obj = Parser { token_iter: text.peekable() };
        obj.program()
    }

    pub fn peek_clone(&mut self) -> Option<Token> {
        self.token_iter.peek().map(|t| t.clone())
    }


    /// compare the current token with the passed token
    /// if they match, advance tokenizer
    /// otherwise raise an error
    fn eat(&mut self, ft: FlagType) -> Result<(), ParserError> {
        if let Some(true) = self.token_iter.peek().map(|token| {
            // map: end reference scope
            match *token {
                Token::Num(_) => ft == FlagType::Integer,
                Token::Name(_) => ft == FlagType::Name,
                Token::Str(_) => ft == FlagType::Str,
                Token::Flag(t) => ft == t,
            }
        }) {
            self.token_iter.next();
            Ok(())
        } else {
            Err(ParserError::ExpectationUnmeet)
        }
    }
}

/// Expression parser
#[allow(dead_code)]
impl<'a, Tit> Parser<'a, Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    /// rule: factor: (Plus | Minus) factor | Integer | LParen expr RParen | Var
    fn factor(&mut self) -> Result<Box<Expr>, ParserError> {
        if let Some(token) = self.peek_clone() {
            match token {
                Token::Num(n) => {
                    self.eat(FlagType::Integer).unwrap();
                    Ok(Box::new(Expr::Num(n)))
                }
                Token::Flag(t) if t == FlagType::Plus || t == FlagType::Minus => {
                    self.eat(t).unwrap();
                    let node = try!(self.factor());
                    Ok(Box::new(Expr::UnaryOp(t, node)))
                }
                Token::Flag(FlagType::LParen) => {
                    self.eat(FlagType::LParen).unwrap();
                    let node = try!(self.expr());
                    try!(self.eat(FlagType::RParen));
                    Ok(node)
                }
                Token::Name(id) => {
                    self.eat(FlagType::Name).unwrap();
                    Ok(Box::new(Expr::Var(Var::Name(id))))
                }
                Token::Str(s) => {
                    self.eat(FlagType::Str).unwrap();
                    Ok(Box::new(Expr::Str(s)))
                }
                Token::Flag(FlagType::True) => {
                    self.eat(FlagType::True).unwrap();
                    Ok(Box::new(Expr::Boole(true)))
                }
                Token::Flag(FlagType::False) => {
                    self.eat(FlagType::False);
                    Ok(Box::new(Expr::Boole(false)))
                }
                _ => Err(ParserError::SyntaxError),
            }
        } else {
            Err(ParserError::SyntaxError) // e.g. ended early
        }

    }

    /// rule: term : factor((Mul | Div) factor)*
    fn term(&mut self) -> Result<Box<Expr>, ParserError> {
        let mut node = try!(self.factor());
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Mul) => {
                    self.eat(FlagType::Mul).unwrap(); // must succeed
                    node = Box::new(Expr::BinOp(FlagType::Mul, node, try!(self.factor())));
                }
                Token::Flag(FlagType::Div) => {
                    self.eat(FlagType::Div).unwrap(); // must secceed
                    node = Box::new(Expr::BinOp(FlagType::Div, node, try!(self.factor())));
                }
                _ => break, 
            }
        }
        Ok(node)
    }

    /// rule: LogicalTerm: term {(Plus | Minus) term}
    fn logical_term(&mut self) -> Result<Box<Expr>, ParserError> {
        let mut node = try!(self.term());
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Plus) => {
                    self.eat(FlagType::Plus).unwrap();
                    node = Box::new(Expr::BinOp(FlagType::Plus, node, try!(self.term())));
                }
                Token::Flag(FlagType::Minus) => {
                    self.eat(FlagType::Minus).unwrap();
                    node = Box::new(Expr::BinOp(FlagType::Minus, node, try!(self.term())));
                }
                _ => break,
            };
        }
        Ok(node)
    }

    /// rule: cmp: Disj [ ( EQ | NEQ ) Disj]
    fn cmp(&mut self) -> Result<Box<Expr>, ParserError> {
        let mut node = try!(self.logical_term());
        if let Some(Token::Flag(flag)) = self.peek_clone() {
            match flag {
                FlagType::EQ | FlagType::NEQ => {
                    self.eat(flag).unwrap();
                    node = Box::new(Expr::BinOp(flag, node, try!(self.logical_term())));
                }
                _ => {}
            }
        }
        Ok(node)
    }

    /// rule: conj : LogicalTerm AND LogicalTerm
    fn conj(&mut self) -> Result<Box<Expr>, ParserError> {
        let mut node = try!(self.cmp());
        while let Some(Token::Flag(FlagType::AND)) = self.peek_clone() {
            self.eat(FlagType::AND).unwrap();
            node = Box::new(Expr::BinOp(FlagType::AND, node, try!(self.cmp())));
        }
        Ok(node)
    }

    /// rule: disj: Conj OR Conj
    fn disj(&mut self) -> Result<Box<Expr>, ParserError> {
        let mut node = try!(self.conj());
        while let Some(Token::Flag(FlagType::OR)) = self.peek_clone() {
            self.eat(FlagType::OR).unwrap();
            node = Box::new(Expr::BinOp(FlagType::OR, node, try!(self.conj())));
        }
        Ok(node)
    }

    fn expr(&mut self) -> Result<Box<Expr>, ParserError> {
        match self.peek_clone() {
            Some(Token::Flag(FlagType::Function)) => self.function_def().map(|e| Box::new(e)),
            _ => self.disj(),
        }
    }

    fn error() {
        panic!("Unexpected token");
    }
}

/// Statement parser
impl<'a, Tit> Parser<'a, Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    /// rule: program: Block [newline]
    fn program(&mut self) -> Result<Box<Node>, ParserError> {
        self.block()
        // TODO: handle newline
    }

    /// rule: Block: {Stat} [Retstat]
    fn block(&mut self) -> Result<Box<Node>, ParserError> {
        let mut stats: Vec<Box<Stat>> = vec![];
        while let Ok(stat) = self.stat() {
            stats.push(stat);
        }
        let ret = self.retstat().ok();
        Ok(Box::new(Node::Block(Block::new(stats, ret))))
        // TODO: Retstat
    }

    /// rule: Stat: Colons | (Varlist Assign ExprList)
    fn stat(&mut self) -> Result<Box<Stat>, ParserError> {
        if let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(flag) if flag == FlagType::Colons || flag == FlagType::EOF => {
                    self.eat(flag).unwrap();
                    Ok(Box::new(Stat::Empty))
                }
                Token::Flag(FlagType::Local) => self.assign_local(),
                Token::Name(_) => {
                    let it_backup = self.token_iter.clone();
                    match self.assign() {
                        Ok(stat) => Ok(stat),
                        Err(ParserError::ExpectationUnmeet) => unimplemented!(),
                        e @ _ => e,
                    }
                }
                Token::Flag(FlagType::If) => self.if_else_clause(),                
                Token::Flag(FlagType::Break) => {
                    self.eat(FlagType::Break).unwrap();
                    Ok(Box::new(Stat::Break))
                }
                _ => unimplemented!(),
            }
        } else {
            Err(ParserError::SyntaxError)
        }
    }

    /// rule: assign: Varlist = Exprlist
    fn assign(&mut self) -> Result<Box<Stat>, ParserError> {
        let mut varlist = try!(self.varlist());
        if let Ok(_) = self.eat(FlagType::Assign) {
            let mut exprlist = try!(self.exprlist());
            Ok(Box::new(Stat::Assign(varlist, exprlist)))
        } else {
            Err(ParserError::ExpectationUnmeet)
        }
    }

    /// rule : assign_local : Local Namelist = Exprlist
    fn assign_local(&mut self) -> Result<Box<Stat>, ParserError> {
        self.eat(FlagType::Local).unwrap();
        let namelist = try!(self.namelist());
        if let Ok(_) = self.eat(FlagType::Assign) {
            let exprlist = try!(self.exprlist());
            Ok(Box::new(Stat::AssignLocal(namelist, exprlist)))
        } else {
            Err(ParserError::SyntaxError)
        }
    }

    /// rule: retstat:
    fn retstat(&mut self) -> Result<Vec<Box<Expr>>, ParserError> {
        try!(self.eat(FlagType::Return));
        self.exprlist().or(Ok(vec![])).map(|r| {
            self.eat(FlagType::Colons).ok(); // ignore
            r
        })
    }

    /// rule: var: Name | PrefixExpr |
    fn var(&mut self) -> Result<Var, ParserError> {
        match self.peek_clone() {
            Some(Token::Name(_)) => self.name().map(|id| Var::Name(id)),
            _ => unimplemented!(),
        }
    }
    /// rule: varlist Name { Comma Name}
    fn varlist(&mut self) -> Result<Vec<Var>, ParserError> {
        let mut var = try!(self.var());
        let mut list = vec![var];
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Comma) => {
                    self.eat(FlagType::Comma).unwrap();
                    var = try!(self.var());
                    list.push(var);
                }
                _ => break,
            }
        }
        Ok(list)
    }

    fn name(&mut self) -> Result<Name, ParserError> {
        if let Some(Token::Name(id)) = self.peek_clone() {
            self.eat(FlagType::Name).unwrap();
            Ok(id)
        } else {
            Err(ParserError::SyntaxError)
        }
    }

    fn namelist(&mut self) -> Result<Vec<Name>, ParserError> {
        let mut list = vec![try!(self.name())];
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Comma) => {
                    self.eat(FlagType::Comma).unwrap();
                    list.push(try!(self.name()));
                }
                _ => break,
            }
        }
        Ok(list)
    }

    /// rule: exprlist: Expr { Comma Expr}
    fn exprlist(&mut self) -> Result<Vec<Box<Expr>>, ParserError> {
        let mut expr = try!(self.expr());
        let mut list = vec![expr];
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Comma) => {
                    self.eat(FlagType::Comma).unwrap();
                    expr = try!(self.expr());
                    list.push(expr);
                }
                _ => break,
            }
        }
        Ok(list)
    }

    /// ifelse: if Expr then Block { elseif Expr then Block} [else Block] end
    fn if_else_clause(&mut self) -> Result<Box<Stat>, ParserError> {
        try!(self.eat(FlagType::If));
        let expr = try!(self.expr());
        try!(self.eat(FlagType::Then));
        let then_node = try!(self.block());
        let mut result = Box::new(Stat::IfElse(expr, then_node, None));
        let mut bottom_clause = result.as_mut() as *mut Stat;
        // {elseif exp then exp}
        unsafe {
            // bypass borrow checker
            while let Some(Token::Flag(FlagType::Elseif)) = self.peek_clone() {
                self.eat(FlagType::Elseif).unwrap();
                let expr = try!(self.expr());
                try!(self.eat(FlagType::Then).or(Err(ParserError::SyntaxError)));
                let then_node = try!(self.block());
                // create new if-else node and walk down
                let sub_clause = Box::into_raw(Box::new(Stat::IfElse(expr, then_node, None)));
                if let Stat::IfElse(_, _, ref mut e) = *bottom_clause {
                    *e = Some(Box::new(Node::Block(Block::new(vec![Box::from_raw(sub_clause)],
                                                              None))));
                } else {
                    panic!("Should not be refute");
                }
                bottom_clause = sub_clause;
            }
            // [else block]
            if let Some(Token::Flag(FlagType::Else)) = self.peek_clone() {
                self.eat(FlagType::Else).unwrap();
                let block = try!(self.block());
                if let Stat::IfElse(_, _, ref mut e) = *bottom_clause {
                    *e = Some(block);
                } else {
                    panic!("Should not be refute");
                }
            }
            if let Err(_) = self.eat(FlagType::End) {
                Err(ParserError::SyntaxError)
            } else {
                Ok(Box::from_raw(bottom_clause))
            }
        }
    }

    /// WhileDo: while Expr do Block end
    fn while_do(&mut self) -> Result<Box<Stat>, ParserError> {
        self.eat(FlagType::While).unwrap();
        let expr = try!(self.expr());
        try!(self.eat(FlagType::Do).or(Err(ParserError::SyntaxError)));
        let block = try!(self.block());
        try!(self.eat(FlagType::End).or(Err(ParserError::SyntaxError)));
        Ok(Box::new(Stat::While(expr, block)))
    }

    /// RangedFor: for Namelist in Exprlist do block end
    fn ranged_for(&mut self) -> Result<Box<Stat>, ParserError> {
        self.eat(FlagType::For).unwrap();
        let namelist = try!(self.namelist());
        try!(self.eat(FlagType::In).or(Err(ParserError::SyntaxError)));
        let exprlist = try!(self.exprlist());
        try!(self.eat(FlagType::Do).or(Err(ParserError::SyntaxError)));
        let block = try!(self.block());
        Ok(Box::new(Stat::ForRange(namelist, exprlist, block)))
    }
}

/// function def and call
impl<'a, Tit> Parser<'a, Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    /// rule : function FunctionBody
    fn function_def(&mut self) -> Result<Expr, ParserError> {
        self.eat(FlagType::Function).unwrap();
        let (paras, content) = try!(self.function_body());
        Ok(Expr::FunctionDef(paras, content))
    }
    /// rule: Namelist [ , ...]
    fn parlist(&mut self) -> Result<(Vec<Name>, bool), ParserError> {
        // can not use namelist,
        // three dot exist
        let mut list = vec![try!(self.name())];
        let mut multiret = false;
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Comma) => {
                    self.eat(FlagType::Comma).unwrap();
                    match self.name() {
                        Ok(name) => list.push(name),
                        Err(_) => {
                            if let Some(Token::Flag(FlagType::ThreeDot)) = self.peek_clone() {
                                self.eat(FlagType::ThreeDot).unwrap();
                                multiret = true;
                            } else {
                                return Err(ParserError::SyntaxError);
                            }
                        }
                    }
                }
                _ => break,
            }
        }
        Ok((list, multiret))
    }

    /// rule ( [parlist] ) Block end
    fn function_body(&mut self) -> Result<((Vec<Name>, bool), Box<Block>), ParserError> {
        try!(self.eat(FlagType::LParen));
        // if parlist parse failed
        // it means no paras, use a empty list
        let paras = self.parlist().unwrap_or((vec![], false));
        try!(self.eat(FlagType::RParen));
        if let Some(content) = Block::from_node_enum(*try!(self.block())) {
            return Ok((paras, Box::new(content)));
        } else {
            panic!("Block should alway return a Node::Block");
        }

    }
}
