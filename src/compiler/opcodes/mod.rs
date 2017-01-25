#![allow(dead_code)]
use std::collections::HashMap;
use lexer::tokens::FlagType;
use super::types::*;

const SIZE_C: u32 = 9;
const SIZE_B: u32 = 9;
const SIZE_Bx: u32 = SIZE_C + SIZE_B;
const SIZE_A: u32 = 8;
const SIZE_OP: u32 = 6;

const POS_OP: u32 = 0;
const POS_A: u32 = POS_OP + SIZE_OP;
const POS_C: u32 = POS_A + SIZE_A;
const POS_B: u32 = POS_C + SIZE_C;
const POS_Bx: u32 = POS_C;

/// create a 1's mask
pub fn mask_1(len: u32, posi: u32) -> u32 {
    (u32::max_value() >> (32 - len)) << posi
}

/// create a 0's mask
pub fn mask_0(len: u32, posi: u32) -> u32 {
    !mask_1(len, posi)
}

#[allow(non_camel_case_types)]
#[derive(Clone, Debug)]
pub enum OpMode {
    iABC(OpName, u32, u32, u32),
    iABx(OpName, u32, u32),
    iAsBx(OpName, u32, i32),
    /// for jmp instruction
    Label(Label),
    rJMP(Label),
    rForPrep(u32, Label),
    rForLoop(u32, Label),
}

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum OpName {
    // Name           Opcode      Description
    MOVE, //          0           R(A) := R(B)
    LOADK, //         1           R(A) := Kst(Bx)
    LOADBOOL, //      2           R(A) := (Bool)B if(C) pc++
    LOADNIL, //       3           R(A) := ... = R(B) := nil
    GETUPVAL, //      4           R(A) := UpValue[B]

    GETGLOBAL, //     5           R(A) := Gbl(Kst(Bx))
    GETTABLE, //      6           R(A) := R(B)[RK(C)]

    SETGLOBAL, //     7           Glb[Kst(Bx)] := R(A)
    SETUPVAL, //      8           UpValue[B] := RK(C)
    SETTABLE, //      9           R(A)[RK(B)] := RK(C)
    NEWTABLE, //      10          R(A) := {} (size = B, C)
    SELF, //          11          R(A+1) := R(B); R(A) := R(B)[RK(C)]

    ADD, //           12          R(A) := RK(B) + RK(C)
    SUB, //           13          R(A) := RK(B) - RK(C)
    MUL, //           14          R(A) := RK(B) * RK(C)
    DIV, //           15          R(A) := RK(B) / RK(C)
    MOD, //           16          R(A) := RK(B) % RK(C)
    POW, //           17          R(A) := RK(B) ^ RK(C)
    UNM, //           18          R(A) := ~R(B)
    NOT, //           19          R(A) := not R(B)
    LEN, //           20          R(A) := length of R(B)

    CONCAT, //        21          R(A) := R(B) .. ... .. R(C)
    JMP, //           22          pc += sBx
    EQ, //            23          if ((RK(B) == RK(C)) ~= A) then pc++
    LT, //            24          if ((RK(B) <  RK(C)) ~= A) then pc++
    LE, //            25          if ((RK(B) <= RK(C)) ~= A) then pc++

    TEST, //          26          if not (R(A) <=> C) then pc++
    TESTSET, //       27          if (R(B) <=> C) then R(A) := R(B) else pc++
    CALL, //          28          R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
    TAILCALL, //      29          return R(A)(R(A+1), ... ,R(A+B-1))
    RETURN, //        30          return R(A), ... ,R(A+B-2)

    FORLOOP, //       31          R(A)+=R(A+2);
    //                            if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }
    FORPREP, //       32          R(A)-=R(A+2); pc+=sBx
    TFORLOOP, //      33          R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));
    //                            if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++
    SETLIST, //       34          R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B
    CLOSE, //         35          close all variables in the stack up to (>=) R(A)
    CLOSURE, //       36          R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))
    VARARG, //        37          R(A), R(A+1), ..., R(A+B-1) = vararg
}

macro_rules! map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

pub fn get_opflag_opname_map() -> HashMap<FlagType, OpName> {
    map!{
        FlagType::Plus => OpName::ADD,
        FlagType::Minus => OpName::SUB,
        FlagType::Mul => OpName::MUL,
        FlagType::Div => OpName::DIV
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct OpcodeBuilder {
}

#[allow(non_snake_case)]
impl OpcodeBuilder {
    pub fn iABC(op: OpName, A: u32, B: u32, C: u32) -> u32 {
        if A > mask_1(SIZE_A, 0) || B > mask_1(SIZE_B, 0) || C > mask_1(SIZE_C, 0) {
            panic!("Instuction parameter overflow");
        }
        (op as u32) | A << POS_A | B << POS_B | C << POS_C
    }

    pub fn iABx(op: OpName, A: u32, Bx: u32) -> u32 {
        if A > mask_1(SIZE_A, 0) || Bx > mask_0(SIZE_Bx, 0) {
            panic!("Instuction parameter overflow");
        }
        (op as u32) | A << POS_A | Bx << POS_Bx
    }

    pub fn iAsBx(op: OpName, A: u32, sBx: i32) -> u32 {
        // trancate
        let trancated: u32 = ((sBx << ((32 - SIZE_Bx) as u32)) as u32) >> ((32 - SIZE_Bx) as u32);
        if A > mask_1(SIZE_A, 0) {
            panic!("Instuction parameter overflow");
        }
        (op as u32) | A << POS_A | trancated << POS_Bx
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mask() {
        assert_eq!(0x00ff0000_u32, mask_1(8, 16));
        assert_eq!(0xffff000f_u32, mask_0(12, 4));
    }

    #[test]
    fn test_opcode_generator() {
        // loadk 0 1
        assert_eq!(OpcodeBuilder::iABx(OpName::LOADK, 0, 1), 0x00004001_u32);
    }
}
