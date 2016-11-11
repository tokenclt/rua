use lexer::{Tokenizer, TokenType, Token, TokenIterator};
use std::iter;

pub enum Expr {
    Num(f64),
    BinOp(TokenType, Box<Expr>, Box<Expr>),
    UnaryOp(TokenType, Box<Expr>),
}

#[derive(Debug)]
pub enum ParserError {
    SyntaxError,
}

pub struct Parser<'a, TRaw>
    where TRaw: Tokenizer<'a>
{
    /// peekable tokenizer
    token_iter: iter::Peekable<TokenIterator<TRaw::Iter>>,
}

impl<'a, TRaw> Parser<'a, TRaw>
    where TRaw: Tokenizer<'a>
{
    pub fn new(text: &'a TRaw) -> Parser<'a, TRaw> {
        Parser { token_iter: text.tokenize().peekable() }
    }

    pub fn parse(&mut self) -> Result<Box<Expr>, ParserError> {
        self.expr()
    }

    /// compare the current token with the passed token
    /// if they match, advance tokenizer
    /// otherwise raise an error
    fn eat(&mut self, token_type: TokenType) -> Result<(), ParserError> {
        if let Some(&token) = self.token_iter.peek() {
            if token.token_type == token_type {
                self.token_iter.next();
                return Ok(());
            }
        }
        Err(ParserError::SyntaxError)
    }

    /// rule: factor: (Plus | Minus) factor | Integer | LParen expr RParen
    fn factor(&mut self) -> Result<Box<Expr>, ParserError> {
        if let Some(&Token { token_type, token_value }) = self.token_iter.peek() {
            match token_type {
                TokenType::Plus | TokenType::Minus => {
                    self.eat(token_type); // must succeed
                    let node = try!(self.factor());
                    Ok(Box::new(Expr::UnaryOp(token_type,node)))
                }
                TokenType::Integer => {
                    self.eat(TokenType::Integer);
                    Ok(Box::new(Expr::Num(token_value.unwrap())))
                }
                TokenType::LParen => {
                    self.eat(TokenType::LParen);
                    let node = try!(self.expr());
                    try!(self.eat(TokenType::RParen));
                    Ok(node)
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
        while let Some(&Token { token_type, .. }) = self.token_iter.peek() {
            match token_type {
                TokenType::Mul => {
                    self.eat(TokenType::Mul); // must succeed
                    node = Box::new(Expr::BinOp(token_type, node, try!(self.factor())));
                }
                TokenType::Div => {
                    self.eat(TokenType::Div); // must secceed
                    node = Box::new(Expr::BinOp(token_type, node, try!(self.factor())));
                }
                _ => break, 
            }
        }
        Ok(node)
    }

    /// rule: expr : term((Plus | Minus) term)*
    fn expr(&mut self) -> Result<Box<Expr>, ParserError> {
        let mut node = try!(self.term());
        while let Some(&Token { token_type, .. }) = self.token_iter.peek() {
            match token_type {
                TokenType::Plus => {
                    self.eat(TokenType::Plus);
                    node = Box::new(Expr::BinOp(token_type, node, try!(self.term())));
                }
                TokenType::Minus => {
                    self.eat(TokenType::Minus);
                    node = Box::new(Expr::BinOp(token_type, node, try!(self.term())));
                }
                _ => break,
            };
        }
        Ok(node)
    }

    fn error() {
        panic!("Unexpected token");
    }
}