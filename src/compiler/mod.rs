use self::opcodes::*;
use self::symbol_table::*;
use self::resource_allocator::*;
use self::types::*;
use parser::types::*;
use lexer::tokens::FlagType;
use std::collections::HashMap;

pub mod symbol_table;
pub mod resource_allocator;
pub mod opcodes;
pub mod types;

#[derive(Debug, PartialEq)]
pub enum CompileError {
    SyntexError,
    InconsistentRet,
    UndefinedSymbol,
}

#[derive(Debug)]
pub struct CodeGen {
    symbol_table: ScopedSymbolTableBuilder,
    flag_to_op: HashMap<FlagType, OpName>,
    bytecode: Vec<u32>,
}

// public interface
impl CodeGen {
    pub fn new() -> CodeGen {
        CodeGen {
            symbol_table: ScopedSymbolTableBuilder::new(),
            flag_to_op: get_opflag_opname_map(),
            bytecode: vec![],
        }
    }

    pub fn compile(&mut self, ast: &Node) -> Result<(), CompileError> {
        self.visit_unit(ast)
    }
}

// visit method
impl CodeGen {
    fn visit_unit(&mut self, node: &Node) -> Result<(), CompileError> {
        // TODO: add header
        // root block should not have retstat
        self.visit_chunk(node).and_then(|r| if let None = r {
            Ok(())
        } else {
            Err(CompileError::SyntexError)
        })
    }
    /// visit function body
    fn visit_chunk(&mut self, node: &Node) -> Result<Option<Usize>, CompileError> {

        let mut res_alloc = ResourceAlloc::new();
        let mut bytecodes = Vec::<u32>::new();
        let ret_num = try!(self.visit_block(node, &mut res_alloc, &mut bytecodes));
        // number of upvalues
        self.bytecode.push(0);
        // number of parameters
        self.bytecode.push(0);
        // is_vararg (always 2 for top level function )
        self.bytecode.push(2);
        // maximum stack size ( number of register used )
        self.bytecode.push(res_alloc.reg_alloc.size());
        // list of instructions
        //    size
        self.bytecode.push(bytecodes.len() as u32);
        //    instructions
        self.bytecode.append(&mut bytecodes);
        // list of constants
        self.bytecode.append(&mut res_alloc.const_alloc.dump());
        // list of function prototypes
        Ok(ret_num)
    }

    /// ret: number of returned
    /// None means no return stat
    fn visit_block(&mut self,
                   node: &Node,
                   res_alloc: &mut ResourceAlloc,
                   bytecodes: &mut Vec<u32>)
                   -> Result<Option<u32>, CompileError> {
        if let Node::Block(Block { ref stats, ref ret }) = *node {
            let mut ret_num = None;
            for stat in stats {
                let stat_result = try!(self.visit_stat(stat, res_alloc, bytecodes));
                if ret_num == None {
                    ret_num = stat_result;
                } else {
                    if ret_num != stat_result {
                        return Err(CompileError::InconsistentRet);
                    }
                }
            }
            Ok(ret_num)
        } else {
            panic!("Block should be ensured by parser");
        }
    }

    /// ret:
    fn visit_stat(&mut self,
                  stat: &Stat,
                  res_alloc: &mut ResourceAlloc,
                  bytecodes: &mut Vec<u32>)
                  -> Result<Option<u32>, CompileError> {
                      println!("{:?}", stat);
        match *stat {
            Stat::Assign(ref varlist, ref exprlist) => {
                // visit each expr, and get result register
                // if error is throwed in visit_expr()
                // collect will early stop
                let reg_list = exprlist.into_iter()
                    .map(|expr| self.visit_expr(expr, res_alloc, bytecodes))
                    .filter(|r| r.is_ok())
                    .collect::<Vec<_>>();
                // check early stop
                if reg_list.len() != exprlist.len() {
                    return Err(CompileError::SyntexError);
                }
                if varlist.len() == reg_list.len() {
                    for (var, expr) in varlist.into_iter().zip(reg_list.into_iter()) {
                        match *var {
                            Var::Name(ref name) => {
                                let const_pos = self.prepare_global_value(name, res_alloc); /* add name to const list and define global symbol */
                                CodeGen::emit_iABx(bytecodes,
                                                   OpName::SETGLOBAL,
                                                   expr.unwrap(),
                                                   const_pos);
                            }
                        }
                    }
                    Ok(None)
                } else {
                    // loadnil and omit
                    unimplemented!()
                }
            }
            _ => unimplemented!(),
        }
    }

    /// ret: a register hold the value
    fn visit_expr(&mut self,
                  expr: &Expr,
                  res_alloc: &mut ResourceAlloc,
                  bytecodes: &mut Vec<u32>)
                  -> Result<Usize, CompileError> {
        match *expr {
            Expr::Num(num) => {
                let const_pos = res_alloc.const_alloc.push(ConstType::Real(num));
                let reg = res_alloc.reg_alloc.get_register(None);
                CodeGen::emit_iABx(bytecodes, OpName::LOADK, reg, const_pos);
                Ok(reg)
            }
            Expr::BinOp(flag, ref left, ref right) => {
                // use left register as result register
                let left_reg = try!(self.visit_expr(left, res_alloc, bytecodes));
                let right_reg = try!(self.visit_expr(right, res_alloc, bytecodes));
                let op = self.flag_to_op.get(&flag).unwrap().clone();
                CodeGen::emit_iABC(bytecodes, op, left_reg, left_reg, right_reg);
                Ok(left_reg)
            }
            Expr::Var(ref var) => self.visit_var(var, res_alloc, bytecodes),
            _ => unimplemented!(),
        }
    }

    //  ret: register saves the varible
    fn visit_var(&mut self,
                 var: &Var,
                 res_alloc: &mut ResourceAlloc,
                 bytecodes: &mut Vec<u32>)
                 -> Result<Usize, CompileError> {
        match *var {
            Var::Name(ref name) => {
                let (scope, pos) =
                    try!(self.symbol_table.lookup(name).ok_or(CompileError::UndefinedSymbol));
                match scope {
                    SymbolScope::Global => {
                        let const_pos = res_alloc.const_alloc.push(ConstType::Str(name.clone()));
                        let reg = res_alloc.reg_alloc.get_register(None);
                        CodeGen::emit_iABx(bytecodes, OpName::GETGLOBAL, reg, const_pos);
                        Ok(reg)
                    }
                    _ => unimplemented!(),
                }
            }
        }
    }
}

impl CodeGen {
    /// allocate name in const
    /// and define in global scope
    fn prepare_global_value(&mut self, name: &str, res_alloc: &mut ResourceAlloc) -> Usize {
        let pos = res_alloc.const_alloc.push(ConstType::Str(name.to_string()));
        self.symbol_table.define_global(name);
        pos
    }

    /// put iABx instruction in bytecode vector
    #[allow(non_snake_case)]
    fn emit_iABx(bytecodes: &mut Vec<u32>, op: OpName, A: u32, Bx: u32) {
        bytecodes.push(OpcodeBuilder::iABx(op, A, Bx));
    }

    #[allow(non_snake_case)]
    fn emit_iABC(bytecodes: &mut Vec<u32>, op: OpName, A: u32, B: u32, C: u32) {
        bytecodes.push(OpcodeBuilder::iABC(op, A, B, C));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::Parser;
    use std::str::Chars;
    #[test]
    fn global_arith() {
        let ast = Parser::<Chars>::ast_from_text(&String::from("\
            a, b = 2.5 , 2 * 4
            c = (a + b) / 10.5
        "))
            .unwrap();

        let expected_output :Vec<u32> = vec![
            0, // number of upvalues
            0, // number of parameters
            2, // is_vararg
        ];

        let mut compiler = CodeGen::new();
        assert_eq!(compiler.compile(&ast), Ok(()));
        for &bytecode in &compiler.bytecode {
            print!("{:08X} ,", bytecode);
        }
        panic!();
    }
}