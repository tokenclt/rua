use self::opcodes::*;
use self::symbol_table::*;
use self::resource_allocator::*;
use self::types::*;
use parser::types::*;
use lexer::tokens::FlagType;
use std::collections::HashMap;
use std::ptr;

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
    root_function: FunctionChunk,
}

// public interface
impl CodeGen {
    pub fn new() -> CodeGen {
        CodeGen {
            symbol_table: ScopedSymbolTableBuilder::new(),
            flag_to_op: get_opflag_opname_map(),
            root_function: FunctionChunk::new(),
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
        // is_vararg (always 2 for top level function )
        self.visit_function(node, None, &vec![], true)
            .map(|mut func| self.root_function = func.prototype)
    }
    /// visit function body, ret: upvalue_num, prototype
    /// assuming scope is newly initiated
    fn visit_function(&mut self,
                      node: &Node,
                      parent_alloc: Option<&mut ResourceAlloc>,
                      paras: &Vec<Name>,
                      is_vararg: bool)
                      -> Result<FunctionPrototype, CompileError> {

        let mut res_alloc = ResourceAlloc::new().parent(if parent_alloc.is_some() {
            parent_alloc.unwrap() as *mut ResourceAlloc
        } else {
            ptr::null_mut()
        });
        let mut instructions = Vec::<OpMode>::new();
        let mut func_chunk = FunctionChunk::new();
        //  define parameters and reserve registers
        for name in paras {
            let pos = res_alloc.reg_alloc.push(Some(name));
            self.symbol_table.define_local(name, pos);
        }
        //  visit body instuctions
        try!(self.visit_block(node, &mut res_alloc, &mut instructions));
        // number of upvalues
        func_chunk.upvalue_num = res_alloc.upvalue_alloc.size() as Usize;
        // number of parameters
        func_chunk.para_num = paras.len() as Usize;
        func_chunk.is_vararg = is_vararg;
        // maximum stack size ( number of register used )
        func_chunk.stack_size = res_alloc.reg_alloc.size();
        // list of instructions
        //    size
        func_chunk.ins_len = instructions.len() as Usize;
        //    instructions
        func_chunk.instructions = instructions;
        // list of constants
        func_chunk.constants = res_alloc.const_alloc.dump();
        // list of function prototypes
        func_chunk.funclist_len = res_alloc.function_alloc.size() as Usize;
        func_chunk.function_prototypes = res_alloc.function_alloc.get_function_prototypes();
        Ok(FunctionPrototype::new(func_chunk, res_alloc.upvalue_alloc.into_list()))
    }

    /// ret: number of returned
    /// None means no return stat
    fn visit_block(&mut self,
                   node: &Node,
                   res_alloc: &mut ResourceAlloc,
                   instructions: &mut Vec<OpMode>)
                   -> Result<(Option<u32>), CompileError> {
        if let Node::Block(Block { ref stats, ref ret }) = *node {
            let mut ret_num = None;
            for stat in stats {
                let stat_result = try!(self.visit_stat(stat, res_alloc, instructions));
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
                  instructions: &mut Vec<OpMode>)
                  -> Result<Option<u32>, CompileError> {
        match *stat {
            // could be global or local`
            Stat::Assign(ref varlist, ref exprlist) => {
                // visit each expr, and get result register
                // if error is throwed in visit_expr()
                // collect will early stop
                let reg_list = try!(self.visit_exprlist(exprlist, res_alloc, instructions));
                if varlist.len() == reg_list.len() {
                    for (var, expr) in varlist.into_iter().zip(reg_list.into_iter()) {
                        match *var {
                            Var::Name(ref name) => {
                                // lookup , confirm if symbol is already defined
                                match self.symbol_table.lookup(name) {
                                    Some((SymbolScope::Global, _)) |
                                    None => {
                                        let const_pos = self.prepare_global_value(name, res_alloc); /* add name to const list and define global symbol */
                                        CodeGen::emit_iABx(instructions,
                                                           OpName::SETGLOBAL,
                                                           expr.1, // only need reg pos
                                                           const_pos);
                                    }
                                    Some((SymbolScope::Local, pos)) => {
                                        CodeGen::emit_iABx(instructions,
                                                           OpName::MOVE,
                                                           pos,
                                                           expr.1);
                                    }
                                    Some((SymbolScope::UpValue(_), _)) => unimplemented!(),
                                }
                            }
                        }
                    }
                    Ok(None)
                } else {
                    // loadnil and omit
                    unimplemented!()
                }
            }
            // bind to new local
            Stat::AssignLocal(ref namelist, ref exprlist) => {
                let reg_list = try!(self.visit_exprlist(exprlist, res_alloc, instructions));
                if namelist.len() == reg_list.len() {
                    for (name, (is_temp, expr_reg)) in namelist.into_iter()
                        .zip(reg_list.into_iter()) {
                        if is_temp {
                            res_alloc.reg_alloc.push_set(name, expr_reg);
                            self.symbol_table.define_local(name, expr_reg);
                        } else {
                            let pos = res_alloc.reg_alloc.push(Some(name));
                            self.symbol_table.define_local(name, pos);
                            CodeGen::emit_iABx(instructions, OpName::MOVE, pos, expr_reg);
                        }
                    }
                    Ok(None)
                } else {
                    // loadnil, omit
                    unimplemented!()
                }
            }
            Stat::Ret(ref exprlist) => {
                // FIXME: reduce register use
                let reg_list = try!(self.visit_exprlist(exprlist, res_alloc, instructions));
                // allocate consecutive register for return
                let start_register = res_alloc.reg_alloc.size() as u32;
                for &(_, source_reg) in &reg_list {
                    let ret_reg = res_alloc.reg_alloc.push(None);
                    CodeGen::emit_iABx(instructions, OpName::MOVE, ret_reg, source_reg);
                }
                // return statement
                // if B == 1, no expr returned
                // if B >= 1 return R(start_register) .. R(start_register + B - 2)
                CodeGen::emit_iABx(instructions,
                                   OpName::RETURN,
                                   start_register,
                                   (reg_list.len() + 1) as u32);
                Ok(Some(res_alloc.reg_alloc.size()))
            }
            _ => unimplemented!(),
        }
    }

    /// ret: (is_temp, a register hold the value)
    fn visit_expr(&mut self,
                  expr: &Expr,
                  res_alloc: &mut ResourceAlloc,
                  instructions: &mut Vec<OpMode>)
                  -> Result<(bool, Usize), CompileError> {
        match *expr {
            Expr::Num(num) => {
                let const_pos = res_alloc.const_alloc.push(ConstType::Real(num));
                let reg = res_alloc.reg_alloc.push(None);
                CodeGen::emit_iABx(instructions, OpName::LOADK, reg, const_pos);
                Ok((true, reg))
            }
            Expr::BinOp(flag, ref left, ref right) => {
                // use left register as result register
                let (is_temp, left_reg) = try!(self.visit_expr(left, res_alloc, instructions));
                let (_, right_reg) = try!(self.visit_expr(right, res_alloc, instructions));
                let op = self.flag_to_op.get(&flag).unwrap().clone();
                // destructive op only generate for temp register
                if is_temp {
                    CodeGen::emit_iABC(instructions, op, left_reg, left_reg, right_reg);
                    Ok((true, left_reg))
                } else {
                    let reg = res_alloc.reg_alloc.push(None);
                    CodeGen::emit_iABC(instructions, op, reg, left_reg, right_reg);
                    Ok((true, reg))
                }
            }
            Expr::Var(ref var) => self.visit_var(var, res_alloc, instructions),
            Expr::FunctionDef((ref namelist, is_vararg), ref function_body) => {
                self.symbol_table.initialize_scope();
                //  child function prototype should be wrapped in another scope
                let function_prototype =
                    try!(self.visit_function(function_body, Some(res_alloc), namelist, is_vararg));
                self.symbol_table.finalize_scope();
                //  push function prototype in function list
                let func_pos = res_alloc.function_alloc.push(function_prototype.prototype);
                //  temporary register for function
                let reg = res_alloc.reg_alloc.push(None);
                CodeGen::emit_iABx(instructions, OpName::CLOSURE, reg, func_pos);
                //  generate virtual move instructions
                //  helping vm to manage upvalue
                for (is_immidiate, pos_in_vl, pos_in_parent) in function_prototype.upvalue_list {
                    //  move: pass the variable in current lexical scope to closure
                    if is_immidiate {
                        CodeGen::emit_iABx(instructions, OpName::MOVE, pos_in_vl, pos_in_parent);
                    } else {
                        // getupval: pass upvalue to the closure
                        CodeGen::emit_iABx(instructions,
                                           OpName::GETUPVAL,
                                           pos_in_vl,
                                           pos_in_parent);
                    }
                }
                Ok((true, reg))
            }
            _ => unimplemented!(),
        }
    }

    fn visit_exprlist(&mut self,
                      exprlist: &Vec<Box<Expr>>,
                      res_alloc: &mut ResourceAlloc,
                      instructions: &mut Vec<OpMode>)
                      -> Result<Vec<(bool, u32)>, CompileError> {
        let reg_list = exprlist.into_iter()
            .map(|expr| self.visit_expr(expr, res_alloc, instructions))
            .filter(|r| r.is_ok())
            .map(|r| r.unwrap())
            .collect::<Vec<_>>();
        if reg_list.len() != exprlist.len() {
            Err(CompileError::SyntexError)
        } else {
            Ok(reg_list)
        }
    }

    /// ret: (is_temp, register saves the varible)
    fn visit_var(&mut self,
                 var: &Var,
                 res_alloc: &mut ResourceAlloc,
                 instructions: &mut Vec<OpMode>)
                 -> Result<(bool, Usize), CompileError> {
        match *var {
            Var::Name(ref name) => {
                let (scope, pos) =
                    try!(self.symbol_table.lookup(name).ok_or(CompileError::UndefinedSymbol));
                match scope {
                    SymbolScope::Global => {
                        let const_pos = res_alloc.const_alloc.push(ConstType::Str(name.clone()));
                        let reg = res_alloc.reg_alloc.push(None);
                        CodeGen::emit_iABx(instructions, OpName::GETGLOBAL, reg, const_pos);
                        Ok((true, reg))
                    }
                    SymbolScope::UpValue(depth) => {
                        let immidiate_upvalue_pos =
                            unsafe { res_alloc.propagate_upvalue(name, pos, depth) };
                        let reg = res_alloc.reg_alloc.push(None);
                        // todo: optimize, reduce register number
                        CodeGen::emit_iABx(instructions,
                                           OpName::GETUPVAL,
                                           reg,
                                           immidiate_upvalue_pos);
                        Ok((true, reg))
                    }
                    SymbolScope::Local => Ok((false, pos)),
                }
            }
        }
    }
}

impl CodeGen {
    /// allocate name in const
    /// and define in global scope
    /// avoiding duplication included
    fn prepare_global_value(&mut self, name: &str, res_alloc: &mut ResourceAlloc) -> Usize {
        let pos = res_alloc.const_alloc.push(ConstType::Str(name.to_string()));
        self.symbol_table.define_global(name);
        pos
    }

    /// put iABx instruction in bytecode vector
    #[allow(non_snake_case)]
    fn emit_iABx(instructions: &mut Vec<OpMode>, op: OpName, A: u32, Bx: u32) {
        instructions.push(OpMode::iABx(op, A, Bx));
    }

    #[allow(non_snake_case)]
    fn emit_iABC(instructions: &mut Vec<OpMode>, op: OpName, A: u32, B: u32, C: u32) {
        instructions.push(OpMode::iABC(op, A, B, C));
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
            local c = (a + b) / 10.5
        "))
            .unwrap();

        let mut compiler = CodeGen::new();
        assert_eq!(compiler.compile(&ast), Ok(()));
    }

    #[test]
    fn function_def() {
        let ast = Parser::<Chars>::ast_from_text(&String::from("\
            local a = 2
            func = function()
                local b = 3 
                return a + b
            end
        "))
            .unwrap();

        let mut compiler = CodeGen::new();
        assert_eq!(compiler.compile(&ast), Ok(()));
        println!("{:?}", compiler.root_function);
        panic!("boom");
    }
}