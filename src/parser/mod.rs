use lexer::tokens::{Token, FlagType};
use lexer::{Lexer, TokenIterator};
use self::types::*;
use std::iter;
use std::ops::Deref;

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
    pub fn parse(text: TokenIterator<'a, Tit>) -> Result<Node, ParserError> {
        let mut obj = Parser { token_iter: text.peekable() };
        obj.program()
    }

    pub fn ast_from_text(text: &String) -> Result<Node, ParserError> {
        let lex = Lexer::new();
        let token_it = lex.tokenize(text.chars());
        Parser::parse(token_it)
    }

    fn peek_clone(&mut self) -> Option<Token> {
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
    /// rule: factor: (Plus | Minus | Not) factor | Integer | String| Boolean | LParen expr RParen | Var
    fn factor(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.peek_clone() {
            match token {
                Token::Num(n) => {
                    self.eat(FlagType::Integer).unwrap();
                    Ok(Expr::Num(n))
                }
                Token::Flag(t) if t == FlagType::Plus || t == FlagType::Minus ||
                                  t == FlagType::Not => {
                    self.eat(t).unwrap();
                    let node = Box::new(self.factor()?);
                    Ok(Expr::UnaryOp(t, node))
                }
                // TODO: use prefixexr() to parse parentheses
                Token::Flag(FlagType::LParen) |
                Token::Name(_) => self.prefixexp().map(|r| r.0), // dispose type info
                Token::Str(s) => {
                    self.eat(FlagType::Str).unwrap();
                    Ok(Expr::Str(s))
                }
                Token::Flag(FlagType::True) => {
                    self.eat(FlagType::True).unwrap();
                    Ok(Expr::Boole(true))
                }
                Token::Flag(FlagType::False) => {
                    self.eat(FlagType::False);
                    Ok(Expr::Boole(false))
                }
                _ => Err(ParserError::SyntaxError),
            }
        } else {
            Err(ParserError::SyntaxError) // e.g. ended early
        }

    }

    /// rule: term : factor((Mul | Div) factor)*
    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut node = self.factor()?;
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Mul) => {
                    self.eat(FlagType::Mul).unwrap(); // must succeed
                    node = Expr::BinOp(FlagType::Mul, Box::new(node), Box::new(self.factor()?));
                }
                Token::Flag(FlagType::Div) => {
                    self.eat(FlagType::Div).unwrap(); // must secceed
                    node = Expr::BinOp(FlagType::Div, Box::new(node), Box::new(self.factor()?));
                }
                _ => break, 
            }
        }
        Ok(node)
    }

    /// rule: LogicalTerm: term {(Plus | Minus) term}
    fn logical_term(&mut self) -> Result<Expr, ParserError> {
        let mut node = self.term()?;
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Plus) => {
                    self.eat(FlagType::Plus).unwrap();
                    node = Expr::BinOp(FlagType::Plus, Box::new(node), Box::new(self.term()?));
                }
                Token::Flag(FlagType::Minus) => {
                    self.eat(FlagType::Minus).unwrap();
                    node = Expr::BinOp(FlagType::Minus, Box::new(node), Box::new(self.term()?));
                }
                _ => break,
            };
        }
        Ok(node)
    }

    /// rule: cmp: Disj [ ( EQ | NEQ ) Disj]
    fn cmp(&mut self) -> Result<Expr, ParserError> {
        let mut node = self.logical_term()?;
        while let Some(Token::Flag(flag)) = self.peek_clone() {
            match flag {
                FlagType::LESS | FlagType::LEQ | FlagType::GREATER | FlagType::GEQ |
                FlagType::EQ | FlagType::NEQ => {
                    self.eat(flag).unwrap();
                    node = Expr::BinOp(flag, Box::new(node), Box::new(self.logical_term()?));
                }
                _ => break,
            }
        }
        Ok(node)
    }

    /// rule: conj : LogicalTerm AND LogicalTerm
    fn conj(&mut self) -> Result<Expr, ParserError> {
        let mut node = self.cmp()?;
        while let Some(Token::Flag(FlagType::AND)) = self.peek_clone() {
            self.eat(FlagType::AND).unwrap();
            node = Expr::BinOp(FlagType::AND, Box::new(node), Box::new(self.cmp()?));
        }
        Ok(node)
    }

    /// rule: disj: Conj OR Conj
    fn disj(&mut self) -> Result<Expr, ParserError> {
        let mut node = self.conj()?;
        while let Some(Token::Flag(FlagType::OR)) = self.peek_clone() {
            self.eat(FlagType::OR).unwrap();
            node = Expr::BinOp(FlagType::OR, Box::new(node), Box::new(self.conj()?));
        }
        Ok(node)
    }


    /// rule: prefixexp ::= var | functioncall | '(' expr ')'
    /// ret: (prefixexp, FuncCall or Var)
    fn prefixexp(&mut self) -> Result<(Expr, PrefixExp), ParserError> {
        // look forward (1)
        let prefix = match self.peek_clone().unwrap() {
            // '(' expr ')'
            Token::Flag(FlagType::LParen) => {
                self.eat(FlagType::LParen).unwrap();
                let node = self.expr()?;
                self.eat(FlagType::RParen)?;
                (node, PrefixExp::Other)
            }
            // could be name or name + modifier
            Token::Name(name) => {
                self.eat(FlagType::Name).unwrap();
                let node = Expr::Var(Var::Name(name));
                self.name_complement(node)?
            }
            _ => panic!("Token won't be used by prefixexp"),
        };
        self.prefixexp_expand(prefix)
    }

    /// expend prefixexp with ':' '[' '.'
    /// if can not expand
    /// original prefix is returned (epsilon)
    fn prefixexp_expand(&mut self,
                        prefix: (Expr, PrefixExp))
                        -> Result<(Expr, PrefixExp), ParserError> {
        if let Some(token) = self.peek_clone() {
            match token {
                // recurse
                Token::Flag(FlagType::LCrotchet) => {
                    let (table_name, _) = prefix;
                    let table_ref = self.table_ref_comp(table_name)?;
                    self.prefixexp_expand((table_ref, PrefixExp::Var))
                }
                Token::Flag(FlagType::Semi) => unimplemented!(),
                Token::Flag(FlagType::Dot) => unimplemented!(),
                _ => Ok(prefix),
            }
        } else {
            Ok(prefix)
        }
    }

    /// try parse complement form (e.g. ':' , '[')
    /// and combine complement form with prefix
    /// if no complement is found, original_prefix is returned
    fn name_complement(&mut self, prefix: Expr) -> Result<(Expr, PrefixExp), ParserError> {
        if let Some(token) = self.peek_clone() {
            match token {
                // Name args
                Token::Flag(FlagType::LParen) => {
                    self.eat(FlagType::LParen).unwrap();
                    let (args, is_vararg) = self.arglist()?;
                    try!(self.eat(FlagType::RParen));
                    let node = Expr::FunctionCall(Box::new(prefix), args, is_vararg);
                    Ok((node, PrefixExp::FuncCall))
                }
                // Name '[' exp ']'
                Token::Flag(FlagType::LCrotchet) => {
                    Ok((self.table_ref_comp(prefix)?, PrefixExp::Var))
                }
                // Name ':' Name args
                Token::Flag(FlagType::Semi) => unimplemented!(),
                Token::Flag(FlagType::Dot) => unimplemented!(),
                // no expansion performed
                _ => Ok((prefix, PrefixExp::Name)),
            }
        } else {
            Ok((prefix, PrefixExp::Name))
        }
    }

    fn expr(&mut self) -> Result<Expr, ParserError> {
        match self.peek_clone() {
            // function def
            Some(Token::Flag(FlagType::Function)) => self.function_def(),
            // table constructor
            Some(Token::Flag(FlagType::LBrace)) => self.table_constructor(),
            _ => self.disj(),
        }
    }
}

/// Statement parser
impl<'a, Tit> Parser<'a, Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    /// rule: program: Block [newline]
    fn program(&mut self) -> Result<Node, ParserError> {
        self.block().map(|block| Node::Block(block))
        // TODO: handle newline
    }

    /// rule: Block: {Stat} [Retstat]
    fn block(&mut self) -> Result<Block, ParserError> {
        let mut stats: Vec<Stat> = vec![];
        loop {
            let stat = self.stat();
            // println!("{:?}", stat);
            match stat {
                Ok(s) => stats.push(s),
                Err(ParserError::ExpectationUnmeet) => break,
                Err(err) => return Err(err),
            }
        }
        let ret = self.retstat().ok();
        Ok(Block::new(stats, ret))
        // TODO: Retstat
    }

    /// rule: Stat: Semi | (Varlist Assign ExprList)
    fn stat(&mut self) -> Result<Stat, ParserError> {
        loop {
            let attempt = if let Some(token) = self.peek_clone() {
                match token {
                    Token::Flag(FlagType::Semi) => {
                        self.eat(FlagType::Semi).unwrap();
                        Ok(Stat::Empty)
                    }
                    Token::Flag(FlagType::Local) => self.assign_local(),
                    Token::Flag(FlagType::If) => self.if_else_clause(),
                    // do not handle
                    Token::Flag(FlagType::Else) |
                    Token::Flag(FlagType::Elseif) => Err(ParserError::ExpectationUnmeet), 
                    Token::Flag(FlagType::While) => self.while_do(),
                    Token::Flag(FlagType::For) => self.for_clause(),
                    Token::Flag(FlagType::Break) => {
                        self.eat(FlagType::Break).unwrap();
                        Ok(Stat::Break)
                    }
                    // do not handle retstat, leave it to block
                    Token::Flag(FlagType::Return) => Err(ParserError::ExpectationUnmeet), 
                    //  return an error , this will stop parsing block
                    Token::Flag(FlagType::End) => Err(ParserError::ExpectationUnmeet),
                    Token::Flag(FlagType::EOF) => Err(ParserError::ExpectationUnmeet),
                    _ => self.assign(),
                }
            } else {
                Err(ParserError::SyntaxError)
            };
            if let Ok(stat) = attempt {
                // if stat is empty, drop it and keep parsing
                if stat != Stat::Empty {
                    // ownership, attempt is moved
                    return Ok(stat);
                }
                // Err
            } else {
                return attempt;
            }
        }

    }

    /// rule: assign: Varlist = Exprlist
    fn assign(&mut self) -> Result<Stat, ParserError> {
        let mut varlist = try!(self.varlist());
        if let Ok(_) = self.eat(FlagType::Assign) {
            let mut exprlist = try!(self.exprlist());
            Ok(Stat::Assign(varlist, exprlist))
        } else {
            Err(ParserError::ExpectationUnmeet)
        }
    }

    /// rule : assign_local : Local Namelist = Exprlist
    fn assign_local(&mut self) -> Result<Stat, ParserError> {
        self.eat(FlagType::Local).unwrap();
        let namelist = try!(self.namelist());
        if let Ok(_) = self.eat(FlagType::Assign) {
            let exprlist = try!(self.exprlist());
            Ok(Stat::AssignLocal(namelist, exprlist))
        } else {
            // just declaration
            Ok(Stat::AssignLocal(namelist, vec![]))
        }
    }

    /// rule: retstat:
    fn retstat(&mut self) -> Result<Vec<Expr>, ParserError> {
        try!(self.eat(FlagType::Return));
        self.exprlist().or(Ok(vec![])).map(|r| {
            self.eat(FlagType::Semi).ok(); // ignore
            r
        })
    }

    /// rule: var: Name | PrefixExpr |
    fn var(&mut self) -> Result<Var, ParserError> {
        self.prefixexp().and_then(|(expr, cat)| {
            match cat {
                PrefixExp::Name => {
                    if let Expr::Var(Var::Name(name)) = expr {
                        Ok(Var::Name(name))
                    } else {
                        panic!("Auxiliary type info is inconsistent");
                    }
                }
                PrefixExp::Var => Ok(Var::PrefixExp(Box::new(expr))),
                _ => Err(ParserError::SyntaxError),
            }
        })
    }

    /// rule: varlist Name { Comma Name}
    fn varlist(&mut self) -> Result<Vec<Var>, ParserError> {
        let mut var = self.var()?;
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

    /// args for a function call
    /// could end with TripleDot
    fn arglist(&mut self) -> Result<(Vec<Expr>, bool), ParserError> {
        let (mut list, mut var_arg) = match self.expr() {
            Ok(expr) => (vec![expr], false),
            Err(_) => {
                if let Some(Token::Flag(FlagType::TripleDot)) = self.peek_clone() {
                    self.eat(FlagType::TripleDot).unwrap();
                    (vec![], true)
                } else {
                    (vec![], false)
                }
            }
        };
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Comma) => {
                    self.eat(FlagType::Comma).unwrap();
                    match self.expr() {
                        Ok(expr) => list.push(expr),
                        Err(_) => {
                            if let Some(Token::Flag(FlagType::TripleDot)) = self.peek_clone() {
                                self.eat(FlagType::TripleDot).unwrap();
                                var_arg = true;
                            } else {
                                return Err(ParserError::SyntaxError);
                            }
                        }
                    }
                }
                _ => break,
            }
        }
        Ok((list, var_arg))
    }

    /// rule: exprlist:  Expr { Comma Expr}
    fn exprlist(&mut self) -> Result<Vec<Expr>, ParserError> {
        let expr = self.expr()?;
        let mut list = vec![expr];
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Comma) => {
                    self.eat(FlagType::Comma).unwrap();
                    let expr = self.expr()?;
                    list.push(expr);
                }
                _ => break,
            }
        }
        Ok(list)
    }

    /// ifelse: if Expr then Block { elseif Expr then Block} [else Block] end
    fn if_else_clause(&mut self) -> Result<Stat, ParserError> {
        self.eat(FlagType::If)?;
        let expr = Box::new(self.expr()?);
        self.eat(FlagType::Then)?;
        let then_node = Box::new(self.block()?);
        let mut result = Stat::IfElse(expr, then_node, None);
        let mut bottom_clause = &mut result as *mut Stat;
        let root_clause = bottom_clause;
        // {elseif exp then exp}
        unsafe {
            // bypass borrow checker
            while let Some(Token::Flag(FlagType::Elseif)) = self.peek_clone() {
                self.eat(FlagType::Elseif).unwrap();
                let expr = Box::new(self.expr()?);
                self.eat(FlagType::Then).or(Err(ParserError::SyntaxError))?;
                let then_node = Box::new(self.block()?);
                // create new if-else node and walk down
                let sub_clause =
                    Box::into_raw(Box::new(Block::new(vec![Stat::IfElse(expr, then_node, None)],
                                                      None)));
                if let Stat::IfElse(_, _, ref mut e) = *bottom_clause {
                    *e = Some(Box::from_raw(sub_clause));
                } else {
                    panic!("Should not refute");
                }
                bottom_clause = &mut (*sub_clause).stats[0] as *mut Stat;
            }
            // [else block]
            if let Some(Token::Flag(FlagType::Else)) = self.peek_clone() {
                self.eat(FlagType::Else).unwrap();
                let block = Box::new(self.block()?);
                if let Stat::IfElse(_, _, ref mut e) = *bottom_clause {
                    *e = Some(block);
                } else {
                    panic!("Should not refute");
                }
            }
            if let Err(_) = self.eat(FlagType::End) {
                Err(ParserError::SyntaxError)
            } else {
                Ok(result)
            }
        }
    }

    /// WhileDo: while Expr do Block end
    fn while_do(&mut self) -> Result<Stat, ParserError> {
        self.eat(FlagType::While).unwrap();
        let expr = Box::new(self.expr()?);
        self.eat(FlagType::Do).or(Err(ParserError::SyntaxError))?;
        let block = Box::new(self.block()?);
        self.eat(FlagType::End).or(Err(ParserError::SyntaxError))?;
        Ok(Stat::While(expr, block))
    }

    fn for_clause(&mut self) -> Result<Stat, ParserError> {
        self.eat(FlagType::For).unwrap();
        let namelist = self.namelist()?;
        match self.peek_clone() {
            Some(Token::Flag(FlagType::Assign)) => {
                if namelist.len() == 1 {
                    self.numeric_for(namelist[0].clone())
                } else {
                    Err(ParserError::SyntaxError)
                }
            }
            Some(Token::Flag(FlagType::In)) => self.ranged_for(namelist),
            _ => Err(ParserError::SyntaxError),
        }
    }

    /// for Name = expr, expr [, expr] do Block end
    fn numeric_for(&mut self, name: Name) -> Result<Stat, ParserError> {
        self.eat(FlagType::Assign).unwrap();
        let start = Box::new(self.expr()?);
        self.eat(FlagType::Comma).or(Err(ParserError::SyntaxError))?;
        let end = Box::new(self.expr()?);
        // step : default 1
        let step = if let Ok(_) = self.eat(FlagType::Comma) {
            self.expr()?
        } else {
            Expr::Num(1_f64)
        };
        self.eat(FlagType::Do).or(Err(ParserError::SyntaxError))?;
        let block = Box::new(self.block()?);
        self.eat(FlagType::End)?;
        Ok(Stat::ForNumeric(name, start, end, Box::new(step), block))
    }

    /// RangedFor: for Namelist in Exprlist do block end
    fn ranged_for(&mut self, namelist: Vec<Name>) -> Result<Stat, ParserError> {
        self.eat(FlagType::In).unwrap();
        let exprlist = self.exprlist()?;
        self.eat(FlagType::Do).or(Err(ParserError::SyntaxError))?;
        let block = Box::new(self.block()?);
        self.eat(FlagType::End)?;
        Ok(Stat::ForRange(namelist, exprlist, block))
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
        try!(self.eat(FlagType::End));
        Ok(Expr::FunctionDef(paras, content))
    }
    /// rule: Namelist [ , ...]
    fn parlist(&mut self) -> Result<(Vec<Name>, bool), ParserError> {
        // can not use namelist, can not parse three dot
        let (mut list, mut multiret) = match self.name() {
            Ok(name) => (vec![name], false),
            // (...)
            Err(_) => {
                if let Some(Token::Flag(FlagType::TripleDot)) = self.peek_clone() {
                    self.eat(FlagType::TripleDot).unwrap();
                    (vec![], true)
                } else {
                    (vec![], false)
                }
            }
        };
        while let Some(token) = self.peek_clone() {
            match token {
                Token::Flag(FlagType::Comma) => {
                    self.eat(FlagType::Comma).unwrap();
                    match self.name() {
                        Ok(name) => list.push(name),
                        Err(_) => {
                            if let Some(Token::Flag(FlagType::TripleDot)) = self.peek_clone() {
                                self.eat(FlagType::TripleDot).unwrap();
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
        let body = Box::new(self.block()?);
        Ok((paras, body))
    }
}

// table operations
impl<'a, Tit> Parser<'a, Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    /// table constructor ::= '{' [fieldlist] '}'
    fn table_constructor(&mut self) -> Result<Expr, ParserError> {
        self.eat(FlagType::LBrace).unwrap();
        let result = match self.field_list() {
            Ok(list) => Ok(Expr::TableCtor(list)),
            // allow empty field
            Err(ParserError::ExpectationUnmeet) => Ok(Expr::TableCtor(vec![])),
            Err(err) => Err(err), // TODO: why err @ Err(_) => err not work
        };
        self.eat(FlagType::RBrace).and(result)
    }

    /// fieldlist ::= field {(, | ;) field} [(, | ;)]
    fn field_list(&mut self) -> Result<Vec<TableEntry>, ParserError> {
        let mut list = vec![self.field().map_err(|e| ParserError::ExpectationUnmeet)?];
        while let Some(Token::Flag(flag)) = self.peek_clone() {
            if flag == FlagType::Comma || flag == FlagType::Semi {
                self.eat(flag).unwrap();
                match self.field() {
                    Ok(field) => list.push(field),
                    Err(_) => break,
                }
            } else {
                break;
            }
        }
        Ok(list)
    }

    /// field ::= '[' exp ']' '=' exp | Name '=' exp | exp
    fn field(&mut self) -> Result<TableEntry, ParserError> {
        let entry = match self.peek_clone() {
            // '[' exp ']' '=' exp
            Some(Token::Flag(FlagType::LCrotchet)) => {
                self.eat(FlagType::LCrotchet).unwrap();
                let key = self.expr()?;
                self.eat(FlagType::RCrotchet)?;
                self.eat(FlagType::Assign)?;
                let value = self.expr()?;
                (Some(key), value)
            }
            // Name '=' exp
            Some(Token::Name(name)) => {
                self.eat(FlagType::Name).unwrap();
                let key = Expr::Var(Var::Name(name));
                self.eat(FlagType::Assign)?;
                let value = self.expr()?;
                (Some(key), value)
            }
            // exp
            _ => {
                let value = self.expr()?;
                (None, value)
            }
        };
        Ok(entry)
    }

    // build table reference syntax from provided table Exp
    // table_ref ::= Exp '[ Expr ']'
    fn table_ref_comp(&mut self, expr: Expr) -> Result<Expr, ParserError> {
        self.eat(FlagType::LCrotchet).unwrap();
        let refer_field = self.expr()?;
        self.eat(FlagType::RCrotchet)?;
        Ok(Expr::TableRef(Box::new(expr), Box::new(refer_field)))
    }
}
