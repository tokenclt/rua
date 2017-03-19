use ir_generator::types::*;
use ir_generator::opcodes::{OpcodeBuilder, OpMode, OpName};
use self::types::*;
use std::mem::transmute;
use std::collections::HashMap;

pub mod types;

#[derive(Debug)]
pub struct Assembler {
    
}

impl Assembler {
    pub fn assemble(chunk: &FunctionChunk, source_name: &String) -> Result<BytecodeVec, AsmError> {
        Self::asm_chunk(chunk, source_name)
    }
}

impl Assembler {
    fn asm_chunk(chunk: &FunctionChunk, source_name: &String) -> Result<BytecodeVec, AsmError> {
        let mut result = vec![];
        result.append(&mut Self::file_header());
        result.append(&mut Self::encode_source_name(source_name));
        result.append(&mut Self::asm_function_chunk(chunk)?);
        Ok(result)
    }

    fn asm_function_chunk(chunk: &FunctionChunk) -> Result<BytecodeVec, AsmError> {
        let mut result = vec![];
        result.extend_from_slice(&Self::unpack_u32(chunk.first_line));
        result.extend_from_slice(&Self::unpack_u32(chunk.last_line));
        result.push(chunk.upvalue_num as u8);
        result.push(chunk.para_num as u8);
        result.push(if chunk.is_vararg { 2_u8 } else { 0_u8 });
        result.push(chunk.stack_size as u8);

        result.append(&mut Self::asm_block(&chunk.instructions));
        result.append(&mut Self::unpack_constant(&chunk.constants));
        result.append(&mut Self::asm_function_list(&chunk.function_prototypes)?);
        result.extend_from_slice(&Self::unpack_u32(0));
        result.extend_from_slice(&Self::unpack_u32(0));
        result.extend_from_slice(&Self::unpack_u32(0));
        Ok(result)
    }

    /// Generate u8 for instructions
    /// with size field
    #[allow(non_snake_case)]
    fn asm_block(instrs: &Vec<OpMode>) -> BytecodeVec {
        let mut result = vec![];
        result.extend_from_slice(&Self::unpack_u32(instrs.len() as u32));
        let label_removed = Self::remove_label(instrs);
        for instr in &label_removed {
            let packed = match instr {
                &OpMode::iABC(op, A, B, C) => OpcodeBuilder::iABC(op, A, B, C),
                &OpMode::iABx(op, A, Bx) => OpcodeBuilder::iABx(op, A, Bx),
                &OpMode::iAsBx(op, A, sBx) => OpcodeBuilder::iAsBx(op, A, sBx),
                _ => panic!("Can not encode instruction. Has all labels been removed?"),
            };
            result.extend_from_slice(&Self::unpack_u32(packed));
        }
        result
    }

    fn asm_function_list(func_list: &Vec<FunctionChunk>) -> Result<BytecodeVec, AsmError> {
        let mut result = vec![];
        result.extend_from_slice(&Self::unpack_u32(func_list.len() as u32));
        for func_def in func_list {
            result.append(&mut Self::asm_function_chunk(func_def)?);
        }
        Ok(result)
    }

    fn file_header() -> BytecodeVec {
        vec![
            0x1b, 0x4c, 0x75, 0x61,  // header sigature
            0x51, 0x00, 0x01, 0x04,  // size of int, endianese, official, vers
            0x04, 0x04, 0x08, 0x00,  // !interal, size of number, instr, size_
        ]
    }
    fn encode_source_name(src_name: &String) -> BytecodeVec {
        let u32_code = format!("@{}", src_name).to_bytecode();
        Self::unpack_str(u32_code)
    }
}

impl Assembler {
    fn unpack_u32(num: u32) -> [u8; 4] {
        unsafe { transmute::<u32, [u8; 4]>(num) }
    }

    fn unpack_f64(num: f64) -> BytecodeVec {
        let mut result = Vec::with_capacity(8);
        let left = num.to_bytecode()[0];
        let right = num.to_bytecode()[1];
        result.extend_from_slice(&Self::unpack_u32(right));
        result.extend_from_slice(&Self::unpack_u32(left));
        result
    }

    /// convert Vec<u32> to Vec<u8>
    fn unpack_constant(consts: &Vec<ConstType>) -> BytecodeVec {
        let mut const_list = vec![];
        // size of const list
        const_list.extend_from_slice(&Self::unpack_u32(consts.len() as u32));

        for val in consts {
            match val {
                &ConstType::Nil => {
                    const_list.push(0);
                }
                &ConstType::Boole(b) => {
                    const_list.push(1);
                    const_list.push(b as u8);
                }
                &ConstType::Real(num) => {
                    const_list.push(3); // not 2
                    const_list.append(&mut Self::unpack_f64(num));
                }
                &ConstType::Str(ref s) => {
                    const_list.push(4);
                    const_list.append(&mut Self::unpack_str(s.to_bytecode()));
                }
            }
        }
        const_list
    }

    fn unpack_str(str_u32: Vec<u32>) -> BytecodeVec {
        let byte_len = str_u32[0];
        let mut result = Vec::with_capacity(byte_len as usize + 4);
        let mut counter = 0;
        unsafe {
            result.extend_from_slice(&Self::unpack_u32(byte_len));
            for &packed in str_u32.iter().skip(1) {
                for &byte in transmute::<Usize, [u8; 4]>(packed).iter() {
                    result.push(byte);
                    counter += 1;
                    if counter == byte_len {
                        break;
                    }
                }
            }
        }
        result
    }

    fn remove_label(instrs: &Vec<OpMode>) -> Vec<OpMode> {
        // println!("Before remove: {:?}", self);
        // pass one: remove label and build index
        let mut label_removed = vec![];
        let mut index = HashMap::new();
        let mut pos_counter: i32 = 0;
        // where each label locates
        for ins in instrs {
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

pub mod tests {
    use super::*;
    use parser::Parser;
    use ir_generator::CodeGen;
    use std::str::Chars;
    use std::mem::transmute;

    fn split_swap_byte(num: u32) -> [u8; 4] {
        unsafe { transmute::<u32, [u8; 4]>(num.swap_bytes()) }
    }

    macro_rules! add_elem {
        ($cont:ident, $num:expr, b) => {
            $cont.push($num);
        };
        ($cont:ident, $num:expr, w) => {
            $cont.extend_from_slice(&split_swap_byte($num));
        }
    }

    macro_rules! build_bc {
        {$($num:expr ; $tp:ident), *} => {
            {
                let mut bytecode: Vec<u8> = vec![];
                $(
                    add_elem!(bytecode, $num, $tp);
                )*
                bytecode
            } 
        };
    }

    #[test]
    pub fn empty() {
        let ast = Parser::<Chars>::ast_from_text(&String::from("\
            -- Nothing
        "))
            .expect("Syntax Error");
        let mut ir_gen = CodeGen::new();
        assert_eq!(ir_gen.compile(&ast), Ok(()));
        println!("Chunk: {:?}", ir_gen.get_chunk());
        let bytecode = Assembler::assemble(ir_gen.get_chunk(), &"empty.lua".to_string())
            .expect("Assemble failed");

        let expect = build_bc![
            0x1b4c7561 ; w, // header sigature
            0x51000104 ; w, // size of int, endianese, official, version
            0x04040800 ; w, // !interal, size of number, instr, size_t 
            0x0b000000 ; w, // souce name length (11)
            0x40656d70 ; w, // @empty.lua\0
            0x74792e6c ; w,
            0x75 ; b,
            0x61 ; b,
            0x00 ; b,
            0x0 ; w,        // line defined start
            0x0 ; w,        // line defined end
            0x00000202 ; w, // stack, is_vararg, args, upvalues
            // code`
            0x01000000 ; w, // length of code
            0x1e008000 ; w, // return 0 1
            0x0 ; w,        // size of constants
            0x0 ; w,        // size of functions
            0x0 ; w,        // debug info; source line position
            0x0 ; w,        // debug info; local list
            0x0 ; w        // debug info; upvalue list
        ];
        assert_eq!(bytecode, expect);
    }

}