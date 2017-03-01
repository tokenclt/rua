use std::mem::transmute;
use std::collections::HashMap;
use super::opcodes::OpMode;
use super::opcodes::OpName;
use super::opcodes::mask_1;

pub type Usize = u32;
/// jump label
pub type Label = i32;
pub const LFIELDS_PER_FLUSH: u32 = 50;

#[derive(Debug, PartialEq, Clone)]
pub enum ConstType {
    Nil,
    Boole(bool),
    Real(f64),
    Str(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Expect {
    Reg(u32),
    Num(usize),
}

pub fn extract_expect_reg(ex: Option<Expect>) -> Result<Option<u32>, CompileError> {
    match ex {
        Some(Expect::Reg(reg)) => Ok(Some(reg)),
        Some(_) => Err(CompileError::SyntexError),
        None => Ok(None),
    }
}

pub fn extract_expect_num(ex: Option<Expect>) -> Result<usize, CompileError> {
    match ex {
        Some(Expect::Num(num)) => Ok(num),
        _ => Err(CompileError::SyntexError),
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RetExpect {
    Num(u32),
    Indeterminate,
}

#[derive(Debug, PartialEq)]
pub enum CompileError {
    SyntexError,
    InconsistentRet,
    UndefinedSymbol,
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

pub trait RemoveLabel {
    fn remove_label(&self) -> Vec<OpMode>;
}

impl RemoveLabel for Vec<OpMode> {
    // FIXME: what if normal jump exist
    fn remove_label(&self) -> Vec<OpMode> {
        // println!("Before remove: {:?}", self);
        // pass one: remove label and build index
        let mut label_removed = vec![];
        let mut index = HashMap::new();
        let mut pos_counter: i32 = 0;
        for ins in self {
            match ins {
                &OpMode::Label(label) => {
                    index.insert(label, pos_counter);
                }
                _ => {
                    label_removed.push(ins);
                    pos_counter += 1;
                }
            }
        }
        // pass two: replace label with number
        let mut replaced = vec![];
        for (pos, ins) in label_removed.iter().enumerate() {
            match **ins {
                OpMode::rJMP(ref label) => {
                    // TODO: negative jmp
                    let num = index.get(label).expect("Label undefined") - (pos as i32) - 1;
                    if num != 0 {
                        replaced.push(OpMode::iAsBx(OpName::JMP, 0, num));
                    }
                }
                OpMode::rForPrep(reg, ref label) => {
                    let num = index.get(label).expect("Label undefined") - (pos as i32) - 1;
                    replaced.push(OpMode::iAsBx(OpName::FORPREP, reg, num));
                }
                OpMode::rForLoop(reg, ref label) => {
                    let num = index.get(label).expect("Label undefined") - (pos as i32) - 1;
                    replaced.push(OpMode::iAsBx(OpName::FORLOOP, reg, num));
                }
                _ => replaced.push((*ins).clone()),
            }
        }
        replaced
    }
}

/// Convert const value to binary
pub trait ToBytecode {
    fn to_bytecode(&self) -> Vec<u32>;
}

impl ToBytecode for f64 {
    // FIXME:f64 to bytecode
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

/// convert u32 to f8
/// byte floating representation of size
pub trait ToF8 {
    fn to_f8(self) -> u32;
}

impl ToF8 for usize {
    fn to_f8(self) -> u32 {
        assert!(self < u32::max_value() as usize, "usize is too large, can not convert to byte float");
        if self == 0 {
            return self as u32;
        }

        let source = self as u32;
        let power = 32 - source.leading_zeros() - 1;
        let (mag, exp) = if power >= 4 {
            let fract_4 = source & mask_1(4, power - 4);
            let mag = if fract_4 & 1 == 1 {
                (fract_4 >> 1) + 1
            } else {
                fract_4 >> 1
            };
            (mag, power - 2)
        } else {
            let mag = source & mask_1(3, 0);
            (mag, if power >= 2 {power - 2} else {0})
        };

        (exp << 3) | mag
    }
}

pub mod tests{
    use super::*;

    #[test]
    pub fn test_utils() {
        let num_0 = 2_usize;
        let num_1 = 4_usize;
        let num_2 = 7_usize;
        let num_3 = 8_usize;
        let num_5 = 21_usize;
        let num_4 = 20_usize;
        
        assert_eq!(num_0.to_f8(), 2_u32);
        assert_eq!(num_1.to_f8(), 4_u32);
        assert_eq!(num_2.to_f8(), 7_u32);
        assert_eq!(num_3.to_f8(), 8_u32);
        assert_eq!(num_4.to_f8(), 18_u32);
        assert_eq!(num_5.to_f8(), 19_u32);
    }
}
