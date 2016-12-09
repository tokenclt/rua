use std::mem::transmute;
use super::opcodes::OpMode;

#[derive(Debug, PartialEq, Clone)]
pub enum ConstType {
    Nil,
    Boole(bool),
    Real(f64),
    Str(String),
}

#[derive(Debug)]
pub struct FunctionChunk {
    pub upvalue_num: Usize,
    pub para_num: Usize,
    pub is_vararg: bool,
    pub stack_size: Usize,
    pub ins_len: Usize,
    pub instructions: Vec<OpMode>,
    pub constants: Vec<u32>, // encoded constant list
    pub funclist_len: Usize,
    pub function_prototypes: Vec<FunctionChunk>,
}

impl FunctionChunk {
    pub fn new() -> FunctionChunk {
        FunctionChunk {
            upvalue_num: 0,
            para_num: 0,
            is_vararg: true,
            stack_size: 0,
            ins_len: 0,
            instructions: vec![],
            constants: vec![],
            funclist_len: 0,
            function_prototypes: vec![],
        }
    }
}

#[derive(Debug)]
pub struct FunctionPrototype {
    pub prototype: FunctionChunk,
    pub upvalue_list: Vec<(bool, u32, u32)>, /* (is_immidiate(depth == 1), pos in upvalue list, parent register number) */
}

impl FunctionPrototype {
    pub fn new(chunk: FunctionChunk, uv_list: Vec<(bool, u32, u32)>) -> FunctionPrototype {
        FunctionPrototype {
            prototype: chunk,
            upvalue_list: uv_list,
        }
    }
}

pub trait ToBytecode {
    fn to_bytecode(&self) -> Vec<u32>;
}

impl ToBytecode for f64 {
    fn to_bytecode(&self) -> Vec<u32> {
        let bitpattern = unsafe { transmute::<f64, u64>(*self) };
        let left = (bitpattern >> 32) as u32;
        let right = bitpattern as u32;
        vec![left, right]
    }
}

impl ToBytecode for String {
    fn to_bytecode(&self) -> Vec<u32> {
        let len = self.as_bytes().len();
        let mut bits = vec![];
        //  complete u32, residue u32
        let (complete, residue) = (len / 4, len % 4);
        let mut byte_iter = self.as_bytes().iter();
        for _ in 0..complete {
            let mut bitpattern: [u8; 4] = [0; 4];
            for byte in &mut bitpattern {
                *byte = *byte_iter.next().unwrap();
            }
            unsafe {
                bits.push(transmute::<[u8; 4], u32>(bitpattern));
            }
        }
        // without zero checking
        // shifting overflow would happen
        if residue > 0 {
            let mut residue_bits = byte_iter.fold(0_u32, |bytes, &byte| (bytes << 8) | byte as u32);
            // left zero pading
            residue_bits = residue_bits << (8 * (4 - residue));
            bits.push(residue_bits);
        }
        bits
    }
}

pub type Usize = u32;