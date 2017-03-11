use std::ops::{DerefMut, Deref};
use lexer::tokens::{Token, FlagType};

pub type Name = String;
pub type TableEntry = (Option<Expr>, Expr);

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Num(f64),
    Boole(bool),
    Var(Var),
    Str(String),
    Nil,
    BinOp(FlagType, Box<Expr>, Box<Expr>),
    UnaryOp(FlagType, Box<Expr>),
    // named paras and has_unnamed
    FunctionDef((Vec<Name>, bool), Box<Block>),
    // evaluating the expr yields the name of function, args, is_vararg
    GeneralCall(Box<Expr>, Vec<Expr>, bool),
    ColonCall(Box<Expr>, Name, Vec<Expr>, bool),
    // vector of expr '=' expr
    TableCtor(Vec<TableEntry>),
    // Exp[Exp]
    TableRef(Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub stats: Vec<Stat>,
    pub ret: Option<Vec<Expr>>,
}

impl Block {
    pub fn new(body: Vec<Stat>, ret: Option<Vec<Expr>>) -> Block {
        Block {
            stats: body,
            ret: ret,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Var {
    /// serve as recursion base
    Name(Name),
    PrefixExp(Box<Expr>),
    /// pass register position directly
    Reg(u32),
}

// if a prefixexp is ended by '(' ... ')', GeneralCall
// if ended by '[' Name ']', '.' Name, Name, Var
// else other
#[derive(Debug, PartialEq)]
pub enum PrefixExp {
    GeneralCall,
    ColonCall,
    Var,
    Other,
    Name,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stat {
    Empty,
    Break,
    // is_local
    Assign(Vec<Var>, Vec<Expr>),
    AssignLocal(Vec<Name>, Vec<Expr>),
    IfElse(Box<Expr>, Box<Block>, Option<Box<Block>>),
    While(Box<Expr>, Box<Block>),
    ForRange(Vec<Name>, Vec<Expr>, Box<Block>),
    ForNumeric(Name, Box<Expr>, Box<Expr>, Box<Expr>, Box<Block>),
    GeneralCall(Box<Expr>, Vec<Expr>, bool),
    ColonCall(Box<Expr>, Name, Vec<Expr>, bool),
    Ret(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Expr(Expr),
    Block(Block),
}

#[derive(Debug)]
pub enum ParserError {
    SyntaxError,
    ExpectationUnmeet,
    ParseFailed,
}