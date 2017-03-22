use ir_generator::types::Usize;

#[derive(Debug, PartialEq, Clone)]
pub enum AsmError {
    LabelUndefined,
}

pub type ByteCodeVec = Vec<u8>;