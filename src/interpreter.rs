use lexer::{Tokenizer, TokenType, Token, TokenIterator};
use std::iter;

pub struct Interpreter<'a, TRaw>
    where TRaw: Tokenizer<'a>
{
    /// peekable tokenizer
    token_iter: iter::Peekable<TokenIterator<TRaw::Iter>>,
}

#[derive(Debug)]
pub enum InterpreterError {
    SyntaxError,
}

impl<'a, TRaw> Interpreter<'a, TRaw>
    where TRaw: Tokenizer<'a>
{
    /// compare the current token with the passed token
    /// if they match, advance tokenizer
    /// otherwise raise an error
    fn eat(&mut self, token_type: TokenType) -> Result<(), InterpreterError> {
        if let Some(&token) = self.token_iter.peek() {
            if token.token_type == token_type {
                self.token_iter.next();
                return Ok(());
            }
        }
        Err(InterpreterError::SyntaxError)
    }

    /// rule: factor: Integer
    fn factor(&mut self) -> Result<f64, InterpreterError> {
        if let Some(&token) = self.token_iter.peek() {
            try!(self.eat(TokenType::Integer));
            Ok(token.token_value.unwrap())
        } else {
            Err(InterpreterError::SyntaxError) // e.g. ended early
        }

    }

    /// rule: term : factor((Mul | Div) factor)*
    fn term(&mut self) -> Result<f64, InterpreterError> {
        let mut result = try!(self.factor());
        while let Some(&Token { token_type, .. }) = self.token_iter.peek() {
            match token_type {
                TokenType::Mul => {
                    self.eat(TokenType::Mul); // must succeed
                    result = result * try!(self.factor());
                }
                TokenType::Div => {
                    self.eat(TokenType::Div); // must secceed
                    result = result / try!(self.factor());
                }
                _ => break, 
            }
        }
        Ok(result)
    }

    /// rule: expr : term((Plus | Minus) term)*
    pub fn expr(&mut self) -> Result<f64, InterpreterError> {
        let mut result = try!(self.term());
        while let Some(&Token { token_type, .. }) = self.token_iter.peek() {
            match token_type {
                TokenType::Plus => {
                    self.eat(TokenType::Plus);
                    result = result + try!(self.term());
                }
                TokenType::Minus => {
                    self.eat(TokenType::Minus);
                    result = result - try!(self.term());
                }
                _ => break,
            };
        }
        Ok(result)
    }

    pub fn new(text: &'a TRaw) -> Interpreter<'a, TRaw> {
        Interpreter { token_iter: text.tokenize().peekable() }
    }

    fn error() {
        panic!("Unexpected token");
    }
}