pub mod tokens;
pub mod buffer;

use std::iter;
use std::str;
use std::collections::HashMap;
use std::clone::Clone;
use std::cmp::max;
use self::tokens::*;
use self::buffer::Buffer;

#[derive(Debug)]
pub enum TokenizeError {
    Invalidoperator,
    InvalidNumber,
    Error,
}

pub struct Lexer {
    keyword_table: HashMap<String, FlagType>,
    operator_table: HashMap<String, FlagType>,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            keyword_table: get_keyword_table(),
            operator_table: get_operator_table(),
        }
    }

    pub fn tokenize<'a, Tit>(&'a self, text_it: Tit) -> TokenIterator<'a, Tit>
        where Tit: iter::Iterator<Item = char> + Clone
    {
        TokenIterator::new(text_it, &self.keyword_table, &self.operator_table)
    }
}

#[derive(Clone)]
pub struct TokenIterator<'a, Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    /// A simple ToIterator
    /// used as an iterator
    buffer: Buffer<Tit>,
    is_ended: bool,
    keywords: &'a HashMap<String, FlagType>,
    operators: &'a HashMap<String, FlagType>,
}

impl<'a, Tit> TokenIterator<'a, Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    fn new(it: Tit,
           kt: &'a HashMap<String, FlagType>,
           st: &'a HashMap<String, FlagType>)
           -> TokenIterator<'a, Tit> {
        TokenIterator {
            buffer: Buffer::<Tit>::new(it),
            is_ended: false,
            keywords: kt,
            operators: st,
        }
    }

    fn consume_while<F>(&mut self, predicate: F) -> String
        where F: Fn(char) -> bool
    {
        let mut v = String::new();
        while let Some((true, c)) = self.buffer.peek().map(|c| (predicate(c), c)) {
            self.buffer.next();
            v.push(c);
        }
        v
    }

    /// find id in text, return Name or reserved keywords
    fn handle_identifier(&mut self) -> Token {
        let id: String = self.consume_while(|c| c.is_alphabetic() || c == '_' || c.is_numeric());
        match self.keywords.get(&id) {
            Some(keyword) => Token::Flag(keyword.clone()),
            _ => Token::Name(id),
        }
    }

    fn handle_operator(&mut self) -> Result<Token, TokenizeError> {
        // Look for longer operator first
        let max_operator_length = 3;
        self.buffer.mark();
        let n_char_sym: String = self.buffer.by_ref().take(max_operator_length).collect();
        for len in (1..(n_char_sym.len() + 1)).rev() {
            if let Some(&sym) = self.operators.get(&n_char_sym[0..len]) {
                // advance original iterator
                self.buffer.rewind();
                let _ = self.buffer.by_ref().take(len).count();
                return Ok(Token::Flag(sym));
            }
        }
        return Err(TokenizeError::Invalidoperator);
    }

    fn handle_string(&mut self) -> Result<Token, TokenizeError> {
        // skip starting quote and save for match
        let start = self.buffer.next().expect("This should never failed");
        let string: String = self.consume_while(|c| c != start && c != '\n');
        //  skip ending
        if let Some(start) = self.buffer.next() {
            Ok(Token::Str(string))
        } else {
            Err(TokenizeError::Error)
        }
        // TODO multiline string
    }

    fn handle_number(&mut self) -> Result<Token, TokenizeError> {
        let raw: String = self.consume_while(|c| c.is_numeric() || c == '.');
        match raw.parse::<f64>() {
            Ok(num) => Ok(Token::Num(num)),
            _ => Err(TokenizeError::InvalidNumber),
        }
    }

    fn eat(&mut self, ch: char) -> Result<(), TokenizeError> {
        if let Some(ch) = self.buffer.peek() {
            self.buffer.next();
            Ok(())
        } else {
            Err(TokenizeError::Error)
        }
    }

    fn skip_comment(&mut self) {
        while let Some(true) = self.buffer.peek().map(|c| c != '\n') {
            self.buffer.next();
        }
        self.eat('\n').expect("Tokenizing failed");
    }
}

impl<'a, Tit> iter::Iterator for TokenIterator<'a, Tit>
    where Tit: iter::Iterator<Item = char> + Clone
{
    type Item = Token;

    /// Main logic for lexical analysis
    fn next(&mut self) -> Option<Token> {
        if self.is_ended {
            return None;
        }

        loop {
            match self.buffer.peek() {
                Some(ch) if ch == ' ' || ch == '\n' => {
                    self.buffer.next();
                    continue;
                }
                Some(ch) if ch.is_alphabetic() || ch == '_' => {
                    return Some(self.handle_identifier())
                }
                Some(ch) if ch.is_numeric() => {
                    return if let Ok(token) = self.handle_number() {
                        Some(token)
                    } else {
                        None
                    }
                }
                Some(ch) if ch == '.' => {
                    self.buffer.mark();
                    if let Some(next_ch) = self.buffer.by_ref().skip(1).next() {
                        if !next_ch.is_numeric() {
                            self.buffer.rewind();
                            return self.handle_operator().ok();
                        } else {
                            self.buffer.rewind();
                            return self.handle_number().ok();
                        }
                    }
                }
                Some(ch) if ch == '-' => {
                    self.buffer.mark();
                    if let Some('-') = self.buffer.by_ref().skip(1).next() {
                        self.buffer.rewind();
                        self.skip_comment();
                    } else {
                        self.buffer.rewind();
                        self.buffer.next();
                        return Some(Token::Flag(FlagType::Minus));
                    }
                }
                Some(ch) if ch == '\'' || ch == '\"' => {
                    return if let Ok(string) = self.handle_string() {
                        Some(string)
                    } else {
                        None
                    }
                }
                Some(_) => {
                    return if let Ok(token) = self.handle_operator() {
                        Some(token)
                    } else {
                        None
                    }
                }
                None => {
                    self.is_ended = true; // EOF should only return for once
                    return Some(Token::Flag(FlagType::EOF));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use lexer::*;
    use lexer::tokens::*;

    #[test]
    fn expression_tokenize() {
        let mut text_1 = "2 * 3 - 2 * ( 3 / 22 + 1 - - 2.22)".to_string();
        let lexer = Lexer::new();
        let mut token_it_1 = lexer.tokenize(text_1.chars());
        assert_eq!(token_it_1.next(), Some(Token::Num(2f64)));
        assert_eq!(token_it_1.next(), Some(Token::Flag(FlagType::Mul)));
        assert_eq!(token_it_1.next(), Some(Token::Num(3f64)));
        assert_eq!(token_it_1.next(), Some(Token::Flag(FlagType::Minus)));
        assert_eq!(token_it_1.next(), Some(Token::Num(2f64)));

        assert_eq!(token_it_1.next(), Some(Token::Flag(FlagType::Mul)));
        assert_eq!(token_it_1.next(), Some(Token::Flag(FlagType::LParen)));
        assert_eq!(token_it_1.next(), Some(Token::Num(3f64)));
        assert_eq!(token_it_1.next(), Some(Token::Flag(FlagType::Div)));
        assert_eq!(token_it_1.next(), Some(Token::Num(22f64)));

        assert_eq!(token_it_1.next(), Some(Token::Flag(FlagType::Plus)));
        assert_eq!(token_it_1.next(), Some(Token::Num(1f64)));
        assert_eq!(token_it_1.next(), Some(Token::Flag(FlagType::Minus)));
        assert_eq!(token_it_1.next(), Some(Token::Flag(FlagType::Minus)));
        assert_eq!(token_it_1.next(), Some(Token::Num(2.22f64)));

        assert_eq!(token_it_1.next(), Some(Token::Flag(FlagType::RParen)));
        assert_eq!(token_it_1.next(), Some(Token::Flag(FlagType::EOF)));
        assert_eq!(token_it_1.next(), None);

        let text_2 = "a, number = 0, 1
            str = \"this is a string\"
            b, c = true, false
        "
            .to_string();
        let mut token_it_2 = lexer.tokenize(text_2.chars());

        assert_eq!(token_it_2.next(), Some(Token::Name("a".to_string())));
        assert_eq!(token_it_2.next(), Some(Token::Flag(FlagType::Comma)));
        assert_eq!(token_it_2.next(), Some(Token::Name("number".to_string())));
        assert_eq!(token_it_2.next(), Some(Token::Flag(FlagType::Assign)));
        assert_eq!(token_it_2.next(), Some(Token::Num(0f64)));

        assert_eq!(token_it_2.next(), Some(Token::Flag(FlagType::Comma)));
        assert_eq!(token_it_2.next(), Some(Token::Num(1f64)));
        assert_eq!(token_it_2.next(), Some(Token::Name("str".to_string())));
        assert_eq!(token_it_2.next(), Some(Token::Flag(FlagType::Assign)));
        assert_eq!(token_it_2.next(),
                   Some(Token::Str("this is a string".to_string())));

        assert_eq!(token_it_2.next(), Some(Token::Name("b".to_string())));
        assert_eq!(token_it_2.next(), Some(Token::Flag(FlagType::Comma)));
        assert_eq!(token_it_2.next(), Some(Token::Name("c".to_string())));
        assert_eq!(token_it_2.next(), Some(Token::Flag(FlagType::Assign)));
        assert_eq!(token_it_2.next(), Some(Token::Flag(FlagType::True)));

        assert_eq!(token_it_2.next(), Some(Token::Flag(FlagType::Comma)));
        assert_eq!(token_it_2.next(), Some(Token::Flag(FlagType::False)));
        assert_eq!(token_it_2.next(), Some(Token::Flag(FlagType::EOF)));
        assert_eq!(token_it_2.next(), None);

    }

    #[test]
    fn blank_line() {
        let text = "\
            a = 1

            b = 2
        "
            .to_string();

        let lexer = Lexer::new();
        let mut it = lexer.tokenize(text.chars());

        assert_eq!(it.next(), Some(Token::Name("a".to_string())));
        assert_eq!(it.next(), Some(Token::Flag(FlagType::Assign)));
        assert_eq!(it.next(), Some(Token::Num(1f64)));

        assert_eq!(it.next(), Some(Token::Name("b".to_string())));
        assert_eq!(it.next(), Some(Token::Flag(FlagType::Assign)));
        assert_eq!(it.next(), Some(Token::Num(2f64)));
    }
}
