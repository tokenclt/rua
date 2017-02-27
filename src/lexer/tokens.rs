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
#[derive(Debug, PartialEq, Copy, Clone, Eq, Hash)]
#[allow(dead_code)]
pub enum FlagType {
    // variable type
    Integer,
    Name,
    Str,
    // keywords
    True,
    False,
    Nil,
    Local,
    Goto,
    While,
    Do,
    End,
    Break,
    Repeat,
    Until,
    If,
    Then,
    Elseif,
    Else,
    For,
    In,
    Function,
    Return,

    Semi, // ';'
    Assign, // '='
    Comma, // ','
    LParen,
    RParen,
    LCrotchet,
    RCrotchet,
    LBrace,
    RBrace,
    // operators
    Plus,
    Minus,
    Mul,
    Div,
    EQ,
    NEQ,
    LEQ,
    GEQ,
    LESS,
    GREATER,
    AND,
    OR,
    Not,

    Dot,
    DoubleDot,
    TripleDot,

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
        "true"     =>  FlagType::True,
        "false"    =>  FlagType::False,
        "local"    =>  FlagType::Local,
        "nil"      =>  FlagType::Nil,
        "goto"     =>  FlagType::Goto,
        "while"    =>  FlagType::While,
        "break"    =>  FlagType::Break,
        "do"       =>  FlagType::Do,
        "end"      =>  FlagType::End,
        "repeat"   =>  FlagType::Repeat,
        "if"       =>  FlagType::If,
        "and"      =>  FlagType::AND,
        "or"       =>  FlagType::OR,
        "not"      =>  FlagType::Not,
        "then"     =>  FlagType::Then,
        "else"     =>  FlagType::Else,
        "elseif"   =>  FlagType::Elseif,
        "for"      =>  FlagType::For,
        "in"       =>  FlagType::In,
        "function" =>  FlagType::Function,
        "return"   =>  FlagType::Return
    }
}

pub fn get_operator_table() -> HashMap<String, FlagType> {
    map!{
        ";"   =>  FlagType::Semi,
        "="   =>  FlagType::Assign,
        ","   =>  FlagType::Comma,
        "("   =>  FlagType::LParen,
        ")"   =>  FlagType::RParen,
        "["   =>  FlagType::LCrotchet,
        "]"   =>  FlagType::RCrotchet,
        "{"   =>  FlagType::LBrace,
        "}"   =>  FlagType::RBrace,
        "+"   =>  FlagType::Plus,
        "-"   =>  FlagType::Minus,
        "*"   =>  FlagType::Mul,
        "/"   =>  FlagType::Div,
        "=="  =>  FlagType::EQ,
        "~="  =>  FlagType::NEQ,
        "<="  =>  FlagType::LEQ,
        ">="  =>  FlagType::GEQ,
        "<"   =>  FlagType::LESS,
        ">"   =>  FlagType::GREATER,
        "."   =>  FlagType::Dot,
        "..." =>  FlagType::TripleDot
        
    }
}
