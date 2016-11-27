use std::ops::{DerefMut, Deref};
use lexer::tokens::{Token, FlagType};

#[derive(Clone, Debug)]
pub enum Expr {
    Num(f64),
    Boole(bool),
    Var(Var),
    Str(String),
    BinOp(FlagType, Box<Expr>, Box<Expr>),
    UnaryOp(FlagType, Box<Expr>),
    // named paras and has_unnamed
    FunctionDef((Vec<Name>, bool), Box<Block>),
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stats: Vec<Box<Stat>>,
    pub ret: Option<Vec<Box<Expr>>>,
}

impl Block {
    pub fn new(body: Vec<Box<Stat>>, ret: Option<Vec<Box<Expr>>>) -> Block {
        Block {
            stats: body,
            ret: ret,
        }
    }

    pub fn from_node_enum(node: Node) -> Option<Block> {
        if let Node::Block(content) = node {
            Some(content)
        }else{
            None
        }
    }
}

pub type Name = String;

#[derive(Clone, Debug)]
pub enum Var {
    Name(Name),
}

#[derive(Clone, Debug)]
pub enum Stat {
    Empty,
    Break,
    // is_local
    Assign(Vec<Var>, Vec<Box<Expr>>),
    AssignLocal(Vec<Name>, Vec<Box<Expr>>),
    IfElse(Box<Expr>, Box<Node>, Option<Box<Node>>),
    While(Box<Expr>, Box<Node>),
    ForRange(Vec<Name>, Vec<Box<Expr>>, Box<Node>),
}

#[derive(Clone, Debug)]
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