#![allow(dead_code)]

const SIZE_C: u32 = 9;
const SIZE_B: u32 = 9;
const SIZE_Bx: u32 = SIZE_C + SIZE_B;
const SIZE_A: u32 = 8;
const SIZE_OP: u32 = 6;

const POS_OP: u32 = 0;
const POS_A: u32 = POS_OP + 0;
const POS_C: u32 = POS_A + SIZE_A;
const POS_B: u32 = POS_C + SIZE_C;
const POS_Bx: u32 = POS_C;

pub fn mask_1(len: u32, posi: u32) -> u32{
    ( u32::max_value() >> (32 - len) ) << posi
}

pub fn mask_0(len: u32, posi: u32) -> u32{
    !mask_1(len, posi)
}



#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn test_mask() {
        assert_eq!(0x00ff0000_u32, mask_1(8, 16));
        assert_eq!(0xffff000f_u32, mask_0(12, 4));
    }
}
