use std::iter;
use std::str;
use std::collections;

#[derive(Debug, PartialEq, Copy, Clone)]
///  Token
pub enum TokenType {
    Integer,
    LParen,
    RParen,
    Plus,
    Minus,
    Mul,
    Div,
    EOF,
    Semi, // ';'
    Assign, // '='
    Comma, // ','
    Name, // keyworks and variable identifier
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Flag(TokenType),
    Num(f64),
    Name(String),
}

pub struct TokenIterator<Tit>
    where Tit: iter::Iterator<Item = char>
{
    /// A simple tokenizer
    /// used as an iterator
    text_iter: iter::Peekable<Tit>,
    is_ended: bool,
    keyword_table: collections::HashMap<String, TokenType>,
}

impl<Tit> TokenIterator<Tit>
    where Tit: iter::Iterator<Item = char>
{
    fn new(it: Tit) -> Self {
        TokenIterator {
            text_iter: it.peekable(),
            is_ended: false,
            keyword_table: collections::HashMap::new(),
        }
    }

    fn consume_while<F>(&mut self, predicate: F) -> Vec<char>
        where F: Fn(char) -> bool
    {
        let mut v: Vec<char> = vec![];
        while let Some((true, c)) = self.text_iter.peek().map(|&c| (predicate(c), c)) {
            self.text_iter.next();
            v.push(c);
        }
        v
    }

    /// find id in text, return Name or reserved keywords
    fn handle_identifier(&mut self) -> Token {
        let id: String = self.consume_while(|c| c.is_alphabetic() || c == '_')
            .into_iter()
            .collect();
        // TODO add keyword table
        Token::Name(id)
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
                            return Some(Token::Num(num.parse::<f64>().unwrap()));
                        }
                        '(' => {
                            self.text_iter.next();
                            return Some(Token::Flag(TokenType::LParen));
                        }
                        ')' => {
                            self.text_iter.next();
                            return Some(Token::Flag(TokenType::RParen));
                        }
                        '+' => {
                            self.text_iter.next();
                            return Some(Token::Flag(TokenType::Plus));
                        }
                        '-' => {
                            self.text_iter.next();
                            return Some(Token::Flag(TokenType::Minus));
                        }
                        '*' => {
                            self.text_iter.next();
                            return Some(Token::Flag(TokenType::Mul));
                        }
                        '/' => {
                            self.text_iter.next();
                            return Some(Token::Flag(TokenType::Div));
                        }
                        ' ' => {
                            self.text_iter.next();
                            continue;
                        }
                        ';' => {
                            self.text_iter.next();
                            return Some(Token::Flag(TokenType::Semi));
                        }
                        '=' => {
                            self.text_iter.next();
                            return Some(Token::Flag(TokenType::Assign));
                        }
                        ',' => {
                            self.text_iter.next();
                            return Some(Token::Flag(TokenType::Comma));
                        }
                        c if c.is_alphabetic() || c == '_' => {
                            return Some(self.handle_identifier());
                        }
                        _ => {
                            self.text_iter.next();
                            return None;
                        }
                    }
                } 
                None => {
                    self.is_ended = true; // EOF should only return for once
                    return Some(Token::Flag(TokenType::EOF));
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

#[cfg(test)]
mod tests {
    use lexer::*;

    #[test]
    fn expression_tokenize() {
        let text = "2 * 3 - 2 * ( 3 / 22 + 1 - - 2.22)".to_string();
        let mut token_it = text.tokenize();
        assert_eq!(token_it.next(), Some(Token::Num(2f64)));
        assert_eq!(token_it.next(), Some(Token::Flag(TokenType::Mul)));
        assert_eq!(token_it.next(), Some(Token::Num(3f64)));
        assert_eq!(token_it.next(), Some(Token::Flag(TokenType::Minus)));
        assert_eq!(token_it.next(), Some(Token::Num(2f64)));

        assert_eq!(token_it.next(), Some(Token::Flag(TokenType::Mul)));
        assert_eq!(token_it.next(), Some(Token::Flag(TokenType::LParen)));
        assert_eq!(token_it.next(), Some(Token::Num(3f64)));
        assert_eq!(token_it.next(), Some(Token::Flag(TokenType::Div)));
        assert_eq!(token_it.next(), Some(Token::Num(22f64)));

        assert_eq!(token_it.next(), Some(Token::Flag(TokenType::Plus)));
        assert_eq!(token_it.next(), Some(Token::Num(1f64)));
        assert_eq!(token_it.next(), Some(Token::Flag(TokenType::Minus)));
        assert_eq!(token_it.next(), Some(Token::Flag(TokenType::Minus)));
        assert_eq!(token_it.next(), Some(Token::Num(2.22f64)));

        assert_eq!(token_it.next(), Some(Token::Flag(TokenType::RParen)));
        assert_eq!(token_it.next(), Some(Token::Flag(TokenType::EOF)));
        assert_eq!(token_it.next(), None);

    }

}
