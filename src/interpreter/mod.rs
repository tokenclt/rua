
pub mod on_ast;

#[derive(Debug, Clone, PartialEq)]
pub enum Variable{
    Real(f64),
    Str(String),
    Boole(bool),
}

#[derive(Debug)]
pub enum InterpretError {
    UndefinedVariable,
    Error,
}