use std::collections::HashMap;

macro_rules! map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = HashMap::new();
            $(
                m.insert($key.to_string(), $value);
            )+
            m
        }
     };
);

///  Token
#[derive(Debug, PartialEq, Copy, Clone)]
#[allow(dead_code)]
pub enum FlagType {
    Integer,
    Name, // keyworks and variable identifier
    Str,
    True,
    False,
    Local,

    Colons, // ';'
    Assign, // '='
    Comma, // ','
    LParen,
    RParen,

    Plus,
    Minus,
    Mul,
    Div,
    EOF,

    
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Flag(FlagType),
    Num(f64),
    Name(String),
    Str(String),
}

pub fn get_keyword_table() -> HashMap<String, FlagType> {
    map!{
        "true" => FlagType::True,
        "false" => FlagType::False,
        "local" => FlagType::Local
    }
}

pub fn get_symbol_table() -> HashMap<String, FlagType> {
    map!{
        ";" => FlagType::Colons,
        "=" => FlagType::Assign,
        "," => FlagType::Comma,
        "(" => FlagType::LParen,
        ")" => FlagType::RParen,
        "+" => FlagType::Plus,
        "-" => FlagType::Minus,
        "*" => FlagType::Mul,
        "/" => FlagType::Div
    }
}

