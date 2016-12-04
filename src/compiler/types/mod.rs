use std::mem::transmute;

#[derive(Debug, PartialEq, Clone)]
pub enum ConstType {
    Nil,
    Boole(bool),
    Real(f64),
    Str(String),
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
        let mut residue_bits = byte_iter.fold(0_u32, |bytes, &byte| (bytes << 8) | byte as u32);
        // left zero pading
        residue_bits = residue_bits << (8 * (4 - residue));
        bits.push(residue_bits);
        bits
    }
}

pub type Usize = u32;