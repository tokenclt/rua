use std::iter;
use std::str;

#[derive(Debug, PartialEq, Copy, Clone)]
///  Token
pub enum TokenType {
    Integer,
    Plus,
    Minus,
    Mul,
    Div,
    EOF,
}

#[derive(Debug, Copy, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub token_value: Option<f64>,
}

impl Token {
    fn new(tp: TokenType, val: Option<f64>) -> Token {
        Token {
            token_type: tp,
            token_value: val,
        }
    }
}

pub struct TokenIterator<Tit>
    where Tit: iter::Iterator<Item = char>
{
    /// A simple tokenizer
    /// used as an iterator
    text_iter: iter::Peekable<Tit>,
    is_ended: bool,
}

impl<Tit> TokenIterator<Tit>
    where Tit: iter::Iterator<Item = char>
{
    fn new(it: Tit) -> Self {
        TokenIterator {
            text_iter: it.peekable(),
            is_ended: false,
        }
    }

    fn consume_while<F>(&mut self, predicate: F) -> Vec<char>
        where F: Fn(char) -> bool
    {
        let mut v: Vec<char> = vec![];
        while let Some(&ch) = self.text_iter.peek() {
            if predicate(ch) {
                self.text_iter.next();
                v.push(ch);
            } else {
                break;
            }
        }
        v
    }
}

impl<Tit> iter::Iterator for TokenIterator<Tit>
    where Tit: iter::Iterator<Item = char>
{
    type Item = Token;

    /// Main logic for lexical analysis
    fn next(&mut self) -> Option<Token> {
        if self.is_ended {
            return None;
        }

        loop {
            match self.text_iter.peek() {
                Some(&ch) => {
                    match ch {
                        '0'...'9' | '.' => {
                            let num: String = self.consume_while(|c| c.is_numeric() || c == '.')
                                .into_iter()
                                .collect();
                            return Some(Token::new(TokenType::Integer,
                                                   Some(num.parse::<f64>().unwrap())));
                        }
                        '+' => {
                            self.text_iter.next();
                            return Some(Token::new(TokenType::Plus, None));
                        }
                        '-' => {
                            self.text_iter.next();
                            return Some(Token::new(TokenType::Minus, None));
                        }
                        '*' => {
                            self.text_iter.next();
                            return Some(Token::new(TokenType::Mul, None));
                        }
                        '/' => {
                            self.text_iter.next();
                            return Some(Token::new(TokenType::Div, None));
                        }
                        ' ' => {
                            self.text_iter.next();
                            continue;
                        }
                        _ => {
                            self.text_iter.next();
                            return None;
                        }
                    }
                } 
                None => {
                    self.is_ended = true; // EOF should only return for once
                    return Some(Token::new(TokenType::EOF, None));
                }
            }
        }
    }
}


pub trait Tokenizer<'a>
    where <Self as Tokenizer<'a>>::Iter: iter::Iterator<Item = char>
{
    type Iter;
    fn tokenize(&'a self) -> TokenIterator<Self::Iter>;
}

impl<'a> Tokenizer<'a> for String {
    type Iter = str::Chars<'a>;
    fn tokenize(&'a self) -> TokenIterator<Self::Iter> {
        TokenIterator::new(self.chars())
    }
}
