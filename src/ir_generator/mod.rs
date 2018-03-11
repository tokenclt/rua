use self::opcodes::*;
use self::symbol_table::*;
use self::resource_allocator::*;
use self::types::*;
use parser::types::*;
use lexer::tokens::FlagType;
use std::collections::HashMap;
use std::ptr;
use std::cmp;

pub mod symbol_table;
pub mod resource_allocator;
pub mod opcodes;
pub mod types;

#[derive(Debug)]
pub struct IRGen {
    symbol_table: ScopedSymbolTableBuilder,
    flag_to_op: HashMap<FlagType, OpName>,
    root_function: FunctionChunk,
}

// public interface
impl IRGen {
    pub fn new() -> IRGen {
        IRGen {
            symbol_table: ScopedSymbolTableBuilder::new(),
            flag_to_op: get_opflag_opname_map(),
            root_function: FunctionChunk::new(),
        }
    }

    pub fn generate_ir(&mut self, ast: &Node) -> Result<(), CompileError> {
        self.visit_unit(ast)
    }

    pub fn get_chunk(&mut self, src_name: &str) -> &FunctionChunk {
        self.root_function.source_name = Some(src_name.to_string());
        &self.root_function
    }
}

// visit method
impl IRGen {
    fn visit_unit(&mut self, node: &Node) -> Result<(), CompileError> {
        // TODO: add header
        // root block should not have retstat
        // is_vararg (always 2 for top level function )
        if let Node::Block(ref block) = *node {
            self.visit_function(block, None, &vec![], true)
                .map(|func| self.root_function = func.prototype)
        } else {
            // can not find entry block
            Err(CompileError::SyntexError)
        }
    }
    /// visit function body, ret: upvalue_num, prototype
    /// assuming scope is newly initiated
    fn visit_function(
        &mut self,
        block: &Block,
        parent_alloc: Option<&mut ResourceAlloc>,
        paras: &Vec<Name>,
        is_vararg: bool,
    ) -> Result<FunctionPrototype, CompileError> {
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
        self.visit_block(block, &mut res_alloc, &mut instructions)?;
        // add a return, may be redundant
        IRGen::emit_iABC(&mut instructions, OpName::RETURN, 0, 1, 0);
        // number of upvalues
        func_chunk.upvalue_num = res_alloc.upvalue_alloc.size() as Usize;
        // number of parameters
        func_chunk.para_num = paras.len() as Usize;
        func_chunk.is_vararg = is_vararg;
        // maximum stack size ( number of register used )
        func_chunk.stack_size = cmp::max(res_alloc.reg_alloc.size(), MIN_STACK_SIZE);
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
        Ok(FunctionPrototype::new(
            func_chunk,
            res_alloc.upvalue_alloc.into_list(),
        ))
    }

    /// ret: number of returned
    /// None means no return stat
    fn visit_block(
        &mut self,
        block: &Block,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
    ) -> Result<(), CompileError> {
        for stat in &block.stats {
            self.visit_stat(stat, res_alloc, instructions)?;
        }
        // if ret statement exists
        if let Some(ref ret_exprs) = block.ret {
            self.visit_stat(&Stat::Ret(ret_exprs.clone()), res_alloc, instructions)?;
        }
        Ok(())
    }

    /// ret:
    fn visit_stat(
        &mut self,
        stat: &Stat,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
    ) -> Result<(), CompileError> {
        match *stat {
            // could be global or local`
            Stat::Assign(ref varlist, ref exprlist) => {
                // visit each expr, and get result register
                //       trim varlist and exprlist into equal length and
                //       return new varlist and exprlist
                //       this method will generate load nill instruction
                //       and have a special handler for GeneralCall
                //       loadnill should be performed at the end
                let (varlist, exprlist) =
                    self.adjust_list(varlist, exprlist, res_alloc, instructions)?;
                for (ref var, ref expr) in varlist.into_iter().zip(exprlist.into_iter()) {
                    match *var {
                        Var::Name(ref name) => {
                            // lookup , confirm if symbol is already defined
                            match self.symbol_table.lookup(name) {
                                Some((SymbolScope::Global, _)) | None => {
                                    let const_pos = self.prepare_global_value(name, res_alloc); /* add name to const list and define global symbol */
                                    let (_, reg) =
                                        self.visit_r_expr(expr, res_alloc, instructions, None)?;
                                    IRGen::emit_iABx(
                                        instructions,
                                        OpName::SETGLOBAL,
                                        reg,
                                        const_pos,
                                    );
                                }
                                Some((SymbolScope::Local, pos)) => {
                                    self.visit_r_expr(
                                        expr,
                                        res_alloc,
                                        instructions,
                                        Some(Expect::Reg(pos)),
                                    )?;
                                }
                                Some((SymbolScope::UpValue(_), _)) => unimplemented!(),
                            }
                        }
                        Var::PrefixExp(ref prefix_expr) => {
                            let (_, value_creg) =
                                self.reg_constid_merge(expr, res_alloc, instructions, None)?;
                            self.visit_l_expr(prefix_expr, value_creg, res_alloc, instructions)?;
                        }
                        _ => unimplemented!(),
                    }
                }
                Ok(())
            }
            // bind to new local
            Stat::AssignLocal(ref namelist, ref exprlist) => {
                let (namelist, exprlist) =
                    self.adjust_list(namelist, exprlist, res_alloc, instructions)?;
                let reg_list = self.visit_r_exprlist(&exprlist, res_alloc, instructions)?;
                for (ref name, (is_temp, expr_reg)) in
                    namelist.into_iter().zip(reg_list.into_iter())
                {
                    if is_temp {
                        res_alloc.reg_alloc.push_set(name, expr_reg);
                        self.symbol_table.define_local(name, expr_reg);
                    } else {
                        let pos = res_alloc.reg_alloc.push(Some(name));
                        self.symbol_table.define_local(name, pos);
                        IRGen::emit_iABC(instructions, OpName::MOVE, pos, expr_reg, 0);
                    }
                }
                Ok(())
            }
            Stat::Ret(ref exprlist) => {
                // first: allocate a chunk of conjective registers
                let reg_list = (0..exprlist.len())
                    .map(|_| res_alloc.reg_alloc.push(None))
                    .collect::<Vec<_>>();
                // if is void return, start_register is not needed
                let ret_num = reg_list.len(); // save moved value
                let start_register = if ret_num > 0 { reg_list[0] } else { 0 };
                // second: visit each expr with expect return register
                for (expr, reg) in exprlist.into_iter().zip(reg_list.into_iter()) {
                    try!(self.visit_r_expr(expr, res_alloc, instructions, Some(Expect::Reg(reg))));
                }
                // return statement
                // if B == 1, no expr returned
                // if B >= 1 return R(start_register) .. R(start_register + B - 2)
                IRGen::emit_iABC(
                    instructions,
                    OpName::RETURN,
                    start_register,
                    (ret_num + 1) as u32,
                    0,
                );
                Ok(())
            }
            Stat::IfElse(ref test_expr, ref then_block, ref else_block) => {
                // if then else
                if let &Some(ref else_block) = else_block {
                    let then_label = res_alloc.label_alloc.new_label();
                    let else_label = res_alloc.label_alloc.new_label();
                    let next_label = res_alloc.label_alloc.new_label();
                    // jmp is not needed for then_block
                    let mut raw = self.visit_boolean_expr(
                        test_expr,
                        res_alloc,
                        then_label,
                        else_label,
                        false,
                    )?;
                    raw.push(OpMode::Label(then_label));
                    self.visit_block(then_block, res_alloc, &mut raw)?;
                    raw.push(OpMode::rJMP(next_label));
                    raw.push(OpMode::Label(else_label));
                    self.visit_block(else_block, res_alloc, &mut raw)?;
                    raw.push(OpMode::Label(next_label));

                    instructions.append(&mut raw);
                } else {
                    // if else
                    let then_label = res_alloc.label_alloc.new_label();
                    let next_label = res_alloc.label_alloc.new_label();
                    let mut raw = self.visit_boolean_expr(
                        test_expr,
                        res_alloc,
                        then_label,
                        next_label,
                        false,
                    )?;
                    raw.push(OpMode::Label(then_label));
                    self.visit_block(then_block, res_alloc, &mut raw)?;
                    raw.push(OpMode::Label(next_label));

                    instructions.append(&mut raw);
                }
                Ok(())
            }
            Stat::While(ref test_expr, ref do_block) => {
                let begin_label = res_alloc.label_alloc.new_label();
                let do_label = res_alloc.label_alloc.new_label();
                let next_label = res_alloc.label_alloc.new_label();
                // set exit for Break stat
                res_alloc.set_loop_exit(next_label);
                let mut raw = vec![OpMode::Label(begin_label)];
                raw.append(&mut self.visit_boolean_expr(
                    test_expr,
                    res_alloc,
                    do_label,
                    next_label,
                    false,
                )?);
                raw.push(OpMode::Label(do_label));
                self.visit_block(do_block, res_alloc, &mut raw)?;
                raw.push(OpMode::rJMP(begin_label));
                raw.push(OpMode::Label(next_label));
                // clear exit
                res_alloc.clear_loop_exit();

                instructions.append(&mut raw);
                Ok(())
            }
            Stat::ForNumeric(ref name, ref start, ref end, ref step, ref block) => {
                let block_label = res_alloc.label_alloc.new_label();
                let test_label = res_alloc.label_alloc.new_label();
                let next_label = res_alloc.label_alloc.new_label();
                let mut raw = vec![];
                // four adjacent registers
                let start_reg = res_alloc.reg_alloc.push(None);
                let end_reg = res_alloc.reg_alloc.push(None);
                let step_reg = res_alloc.reg_alloc.push(None);
                //      exposed as local
                let local_reg = res_alloc.reg_alloc.push(Some(name));
                self.symbol_table.define_local(name, local_reg);
                self.visit_r_expr(start, res_alloc, &mut raw, Some(Expect::Reg(start_reg)))?;
                self.visit_r_expr(end, res_alloc, &mut raw, Some(Expect::Reg(end_reg)))?;
                self.visit_r_expr(step, res_alloc, &mut raw, Some(Expect::Reg(step_reg)))?;
                raw.push(OpMode::rForPrep(start_reg, test_label));
                raw.push(OpMode::Label(block_label));
                // set exit
                res_alloc.set_loop_exit(next_label);
                self.visit_block(block, res_alloc, &mut raw)?;
                res_alloc.clear_loop_exit();
                raw.push(OpMode::Label(test_label));
                raw.push(OpMode::rForLoop(start_reg, block_label));
                raw.push(OpMode::Label(next_label));

                instructions.append(&mut raw);
                Ok(())
            }
            Stat::Break => {
                if let Some(exit_label) = res_alloc.get_loop_exit() {
                    instructions.push(OpMode::rJMP(exit_label));
                    Ok(())
                } else {
                    Err(CompileError::SyntexError)
                }
            }
            Stat::GeneralCall(ref func_name, ref args, is_vararg) => self.visit_general_call(
                func_name,
                args,
                is_vararg,
                RetExpect::Num(0),
                res_alloc,
                instructions,
            ).and(Ok(())),
            Stat::ColonCall(ref table_expr, ref func_name, ref args, is_vararg) => {
                self.visit_colon_call(
                    table_expr,
                    func_name,
                    args,
                    is_vararg,
                    RetExpect::Num(0),
                    res_alloc,
                    instructions,
                ).and(Ok(()))
            }
            _ => unimplemented!(),
        }
    }

    /// ret: (is_temp, a register hold the value)
    /// expect: where the result should be stored or how many result should be returned
    fn visit_r_expr(
        &mut self,
        expr: &Expr,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
        expect: Option<Expect>,
    ) -> Result<(bool, Usize), CompileError> {
        match *expr {
            Expr::Nil => {
                let reg = if let Some(expect) = extract_expect_reg(expect)? {
                    expect
                } else {
                    res_alloc.reg_alloc.push(None)
                };
                IRGen::emit_iABx(instructions, OpName::LOADNIL, reg, reg);
                Ok((true, reg))
            }
            Expr::Num(num) => {
                let const_pos = res_alloc.const_alloc.push(ConstType::Real(num));
                let reg = if let Some(expect) = extract_expect_reg(expect)? {
                    expect
                } else {
                    res_alloc.reg_alloc.push(None)
                };
                IRGen::emit_iABx(instructions, OpName::LOADK, reg, const_pos);
                Ok((true, reg))
            }
            Expr::Boole(value) => {
                let bit = if value { 1 } else { 0 };
                let reg = if let Some(expect) = extract_expect_reg(expect)? {
                    expect
                } else {
                    res_alloc.reg_alloc.push(None)
                };
                IRGen::emit_iABC(instructions, OpName::LOADBOOL, reg, bit, 0);
                Ok((true, reg))
            }
            Expr::Str(ref s) => {
                let const_pos = res_alloc.const_alloc.push(ConstType::Str(s.clone()));
                let reg = if let Some(expect) = extract_expect_reg(expect)? {
                    expect
                } else {
                    res_alloc.reg_alloc.push(None)
                };
                IRGen::emit_iABx(instructions, OpName::LOADK, reg, const_pos);
                Ok((true, reg))
            }
            Expr::BinOp(flag, ref left, ref right) => {
                // use left register as result register
                // TODO: ignore left associative to generate optimized code
                match flag {
                    FlagType::Plus | FlagType::Minus | FlagType::Mul | FlagType::Div => {
                        let (is_temp, left_reg) =
                            self.reg_constid_merge(left, res_alloc, instructions, None)?;
                        let (_, right_reg) =
                            self.reg_constid_merge(right, res_alloc, instructions, None)?;
                        let op = self.flag_to_op
                            .get(&flag)
                            .expect("BinOp not defined")
                            .clone();
                        // destructive op only generate for temp register
                        let (result_is_temp, result_reg) =
                            if let Some(expect) = extract_expect_reg(expect)? {
                                (false, expect)
                            } else {
                                if is_temp {
                                    (true, left_reg)
                                } else {
                                    (true, res_alloc.reg_alloc.push(None))
                                }
                            };
                        IRGen::emit_iABC(instructions, op, result_reg, left_reg, right_reg);
                        Ok((result_is_temp, result_reg))
                    }
                    _ => self.visit_logic_arith(expr, res_alloc, instructions, expect),
                }
            }
            Expr::UnaryOp(op, ref left) => match op {
                FlagType::Minus => unimplemented!(),
                FlagType::Plus => self.visit_r_expr(left, res_alloc, instructions, expect),
                _ => self.visit_logic_arith(expr, res_alloc, instructions, expect),
            },
            Expr::Var(ref var) => {
                self.visit_var(var, res_alloc, instructions, extract_expect_reg(expect)?)
            }
            Expr::FunctionDef((ref namelist, is_vararg), ref function_body) => {
                self.symbol_table.initialize_scope();
                //  child function prototype should be wrapped in another scope
                let function_prototype =
                    self.visit_function(function_body, Some(res_alloc), namelist, is_vararg)?;
                self.symbol_table.finalize_scope();
                //  push function prototype in function list
                let func_pos = res_alloc.function_alloc.push(function_prototype.prototype);
                //  temporary register for function
                let reg = if let Some(expect) = extract_expect_reg(expect)? {
                    expect
                } else {
                    res_alloc.reg_alloc.push(None)
                };
                IRGen::emit_iABx(instructions, OpName::CLOSURE, reg, func_pos);
                //  generate virtual move instructions
                //  helping vm to manage upvalue
                for (is_immidiate, pos_in_vl, pos_in_parent) in function_prototype.upvalue_list {
                    //  move: pass the variable in current lexical scope to closure
                    if is_immidiate {
                        IRGen::emit_iABC(instructions, OpName::MOVE, pos_in_vl, pos_in_parent, 0);
                    } else {
                        // getupval: pass upvalue to the closure
                        IRGen::emit_iABx(instructions, OpName::GETUPVAL, pos_in_vl, pos_in_parent);
                    }
                }
                Ok((true, reg))
            }
            Expr::GeneralCall(ref expr, ref args, is_vararg) => {
                let central_reg = self.visit_general_call(
                    expr,
                    args,
                    is_vararg,
                    RetExpect::Num(1),
                    res_alloc,
                    instructions,
                )?;
                Ok((true, central_reg))
            }
            Expr::ColonCall(ref _table_expr, ref _func_name, ref _args, _is_vararg) => unimplemented!(),

            Expr::TableCtor(ref entrys) => {
                self.visit_table_ctor(entrys, res_alloc, instructions, expect)
            }
            Expr::TableRef(ref table, ref key) => {
                self.visit_table_ref(table, key, res_alloc, instructions, expect)
            } // _ => {
              //     println!("Unmatched expr: {:?}", expr);
              //     unimplemented!();
              // }
        }
    }

    fn visit_l_expr(
        &mut self,
        expr: &Expr,
        value_creg: u32,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
    ) -> Result<(), CompileError> {
        match *expr {
            Expr::TableRef(ref table, ref key) => {
                let (_, table_reg) = self.visit_r_expr(table, res_alloc, instructions, None)?;
                let key_creg = self.visit_table_key(key, res_alloc, instructions)?;
                IRGen::emit_iABC(
                    instructions,
                    OpName::SETTABLE,
                    table_reg,
                    key_creg,
                    value_creg,
                );
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn visit_logic_arith(
        &mut self,
        expr: &Expr,
        res_alloc: &mut ResourceAlloc,
        instruction: &mut Vec<OpMode>,
        expect: Option<Expect>,
    ) -> Result<(bool, u32), CompileError> {
        let result_reg = if let Some(expect) = extract_expect_reg(expect)? {
            expect
        } else {
            res_alloc.reg_alloc.push(None)
        };

        let true_label = res_alloc.label_alloc.new_label();
        let false_label = res_alloc.label_alloc.new_label();
        let mut raw = self.visit_boolean_expr(expr, res_alloc, true_label, false_label, true)?;
        raw.push(OpMode::Label(false_label));
        IRGen::emit_iABC(&mut raw, OpName::LOADBOOL, result_reg, 0, 1);
        raw.push(OpMode::Label(true_label));
        IRGen::emit_iABC(&mut raw, OpName::LOADBOOL, result_reg, 1, 0);
        instruction.append(&mut raw);
        Ok((true, result_reg))
    }

    fn visit_boolean_expr(
        &mut self,
        expr: &Expr,
        res_alloc: &mut ResourceAlloc,
        true_br: Label,
        false_br: Label,
        fall_through: bool,
    ) -> Result<Vec<OpMode>, CompileError> {
        match *expr {
            Expr::BinOp(op, ref left, ref right) => {
                match op {
                    FlagType::OR => {
                        let label_for_right = res_alloc.label_alloc.new_label();
                        let mut left_raw = self.visit_boolean_expr(
                            left,
                            res_alloc,
                            true_br,
                            label_for_right,
                            true,
                        )?;
                        let mut right_raw = self.visit_boolean_expr(
                            right,
                            res_alloc,
                            true_br,
                            false_br,
                            fall_through,
                        )?;
                        // merge
                        left_raw.push(OpMode::Label(label_for_right));
                        left_raw.append(&mut right_raw);
                        Ok(left_raw)
                    }
                    FlagType::AND => {
                        let label_for_right = res_alloc.label_alloc.new_label();
                        let mut left_raw = self.visit_boolean_expr(
                            left,
                            res_alloc,
                            label_for_right,
                            false_br,
                            false,
                        )?;
                        let mut right_raw = self.visit_boolean_expr(
                            right,
                            res_alloc,
                            true_br,
                            false_br,
                            fall_through,
                        )?;
                        left_raw.push(OpMode::Label(label_for_right));
                        left_raw.append(&mut right_raw);
                        Ok(left_raw)
                    }
                    FlagType::LESS
                    | FlagType::LEQ
                    | FlagType::GREATER
                    | FlagType::GEQ
                    | FlagType::EQ
                    | FlagType::NEQ => {
                        let mut raw = vec![];
                        let (_, left_reg) =
                            self.reg_constid_merge(left, res_alloc, &mut raw, None)?;
                        let (_, right_reg) =
                            self.reg_constid_merge(right, res_alloc, &mut raw, None)?;
                        let (op_name, test_bool) = match op {
                            FlagType::LESS => (OpName::LT, true),
                            FlagType::LEQ => (OpName::LE, true),
                            FlagType::GREATER => (OpName::LE, false),
                            FlagType::GEQ => (OpName::LT, false),
                            FlagType::EQ => (OpName::EQ, true),
                            FlagType::NEQ => (OpName::EQ, false),
                            _ => panic!("should not appear"),
                        };
                        // adjust code arrangement according to fall_through
                        let (test_int, path) = if fall_through {
                            (test_bool as u32, true_br)
                        } else {
                            ((!test_bool) as u32, false_br)
                        };
                        raw.push(OpMode::iABC(op_name, test_int, left_reg, right_reg));
                        raw.push(OpMode::rJMP(path));
                        // IRGen::emit_iAsBx(&mut raw, OpName::JMP, 0, false_br);
                        Ok(raw)
                    }
                    _ => panic!("expression not accept as boolean"),
                }
            }
            Expr::UnaryOp(op, ref left) => match op {
                FlagType::Not => {
                    self.visit_boolean_expr(left, res_alloc, false_br, true_br, !fall_through)
                }
                _ => panic!("expression not accept as boolean"),
            },
            Expr::Var(ref var) => {
                let mut raw = vec![];
                let (_, reg) = self.visit_var(var, res_alloc, &mut raw, None)?;
                if fall_through == true {
                    // fall to true path
                    raw.push(OpMode::iABx(OpName::TEST, reg, 1));
                    raw.push(OpMode::rJMP(true_br));
                // IRGen::emit_iAsBx(&mut raw, OpName::JMP, 0, false_br);
                } else {
                    // fall to false path
                    raw.push(OpMode::iABx(OpName::TEST, reg, 0));
                    raw.push(OpMode::rJMP(false_br));
                    // IRGen::emit_iAsBx(&mut raw, OpName::JMP, 0, true_br);
                }
                Ok(raw)
            }
            Expr::Boole(value) => {
                let mut raw = vec![];
                if value {
                    raw.push(OpMode::rJMP(true_br));
                } else {
                    IRGen::emit_iAsBx(&mut raw, OpName::JMP, 0, false_br);
                    raw.push(OpMode::rJMP(false_br));
                }
                Ok(raw)
            }
            _ => panic!("expression not accept"),
        }
    }

    fn visit_r_exprlist(
        &mut self,
        exprlist: &Vec<Expr>,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
    ) -> Result<Vec<(bool, u32)>, CompileError> {
        let reg_list = exprlist
            .into_iter()
            .map(|expr| self.visit_r_expr(expr, res_alloc, instructions, None))
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
    fn visit_var(
        &mut self,
        var: &Var,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
        expect_reg: Option<u32>,
    ) -> Result<(bool, Usize), CompileError> {
        match *var {
            Var::Name(ref name) => {
                let (scope, pos) = try!(
                    self.symbol_table
                        .lookup(name)
                        .ok_or(CompileError::UndefinedSymbol)
                );
                match scope {
                    SymbolScope::Global => {
                        let const_pos = res_alloc.const_alloc.push(ConstType::Str(name.clone()));
                        let reg = if let Some(expect) = expect_reg {
                            expect
                        } else {
                            res_alloc.reg_alloc.push(None)
                        };
                        IRGen::emit_iABx(instructions, OpName::GETGLOBAL, reg, const_pos);
                        Ok((true, reg))
                    }
                    SymbolScope::UpValue(depth) => {
                        let immidiate_upvalue_pos =
                            unsafe { res_alloc.propagate_upvalue(name, pos, depth) };
                        let reg = if let Some(expect) = expect_reg {
                            expect
                        } else {
                            res_alloc.reg_alloc.push(None)
                        };
                        // todo: optimize, reduce register number
                        IRGen::emit_iABx(
                            instructions,
                            OpName::GETUPVAL,
                            reg,
                            immidiate_upvalue_pos,
                        );
                        Ok((true, reg))
                    }
                    SymbolScope::Local => {
                        if let Some(expect) = expect_reg {
                            if expect != pos {
                                IRGen::emit_iABC(instructions, OpName::MOVE, expect, pos, 0);
                                Ok((false, expect)) // caller-provided register is viewed as none temp
                            } else {
                                Ok((false, expect))
                            }
                        } else {
                            // no expected register provided
                            Ok((false, pos))
                        }
                    }
                }
            }
            Var::Reg(reg) => Ok((true, reg)),
            Var::PrefixExp(ref _expr) => unimplemented!(),
        }
    }

    fn visit_general_call(
        &mut self,
        expr: &Expr,
        args: &Vec<Expr>,
        _is_vararg: bool,
        expect_ret: RetExpect,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
    ) -> Result<u32, CompileError> {
        // todo: vararg
        // get function name
        let func_pos = res_alloc.reg_alloc.push(None);
        let ret_field: u32;
        let arg_field: u32;

        // allocate register
        match expect_ret {
            RetExpect::Num(ret_num) => {
                // allocate register
                for _ in 0..(cmp::max(ret_num, args.len() as u32)) {
                    res_alloc.reg_alloc.push(None);
                }
                ret_field = ret_num + 1;
            }
            RetExpect::Indeterminate => {
                ret_field = 0;
            }
        }
        self.visit_r_expr(expr, res_alloc, instructions, Some(Expect::Reg(func_pos)))?;
        arg_field = self.visit_args(args, func_pos, res_alloc, instructions)?;
        IRGen::emit_iABC(instructions, OpName::CALL, func_pos, arg_field, ret_field);
        Ok(func_pos)
    }

    fn visit_colon_call(
        &mut self,
        table_expr: &Expr,
        func_name: &Name,
        args: &Vec<Expr>,
        _is_vararg: bool,
        expect_ret: RetExpect,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
    ) -> Result<u32, CompileError> {
        // make
        let func_pos = res_alloc.reg_alloc.push(None);
        let table_pos = res_alloc.reg_alloc.push(None);
        let ret_field: u32;
        let arg_field: u32;
        // allocate registers
        match expect_ret {
            RetExpect::Num(ret_num) => {
                for _ in 0..(cmp::max(ret_num, args.len() as u32)) {
                    res_alloc.reg_alloc.push(None);
                }
                ret_field = ret_num + 1;
            }
            RetExpect::Indeterminate => {
                ret_field = 0;
            }
        }
        self.visit_r_expr(
            table_expr,
            res_alloc,
            instructions,
            Some(Expect::Reg(table_pos)),
        )?;
        let name_pos = self.visit_table_key(
            &Expr::Var(Var::Name(func_name.clone())),
            res_alloc,
            instructions,
        )?;
        IRGen::emit_iABC(instructions, OpName::SELF, func_pos, table_pos, name_pos);
        arg_field = match self.visit_args(args, func_pos, res_alloc, instructions)? {
            0 => 0,
            i @ _ => i + 1,
        };
        IRGen::emit_iABC(instructions, OpName::CALL, func_pos, arg_field, ret_field);
        Ok(func_pos)
    }

    fn visit_args(
        &mut self,
        args: &Vec<Expr>,
        func_pos: u32,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
    ) -> Result<u32, CompileError> {
        let mut args_reg = func_pos + 1;
        let arg_field;
        if args.is_empty() {
            return Ok(1);
        }
        if let Expr::GeneralCall(ref t_func, ref t_args, t_is_vararg) = args[args.len() - 1] {
            for i in 0..(args.len() - 1) {
                self.visit_r_expr(
                    &args[i],
                    res_alloc,
                    instructions,
                    Some(Expect::Reg(args_reg)),
                )?;
                args_reg += 1;
            }
            self.visit_general_call(
                t_func,
                t_args,
                t_is_vararg,
                RetExpect::Indeterminate,
                res_alloc,
                instructions,
            )?;
            arg_field = 0;
        } else if let Expr::ColonCall(ref t_table, ref t_func_name, ref t_args, t_is_vararg) =
            args[args.len() - 1]
        {
            for i in 0..(args.len() - 1) {
                self.visit_r_expr(
                    &args[i],
                    res_alloc,
                    instructions,
                    Some(Expect::Reg(args_reg)),
                )?;
                args_reg += 1;
            }
            self.visit_colon_call(
                t_table,
                t_func_name,
                t_args,
                t_is_vararg,
                RetExpect::Indeterminate,
                res_alloc,
                instructions,
            )?;
            arg_field = 0;
        } else {
            for expr in args {
                self.visit_r_expr(expr, res_alloc, instructions, Some(Expect::Reg(args_reg)))?;
                args_reg += 1;
            }
            arg_field = args.len() as u32 + 1;
        }
        Ok(arg_field)
    }

    fn adjust_list<T: Clone>(
        &mut self,
        varlist: &Vec<T>,
        exprlist: &Vec<Expr>,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
    ) -> Result<(Vec<T>, Vec<Expr>), CompileError> {
        // balanced
        if varlist.len() == exprlist.len() {
            return Ok((varlist.clone(), exprlist.clone()));
        }
        // imbalanced & trancate
        else if varlist.len() < exprlist.len() {
            // discard resisual expressions
            let remain = varlist.len();
            let mut truncated = exprlist.clone();
            truncated.truncate(remain);
            return Ok((varlist.clone(), truncated));
        }
        // imbalanced & one function call
        else {
            if exprlist.len() == 1 {
                if let Expr::GeneralCall(ref expr, ref args, is_vararg) = exprlist[0] {
                    let central_reg = self.visit_general_call(
                        expr,
                        args,
                        is_vararg,
                        RetExpect::Num(varlist.len() as u32),
                        res_alloc,
                        instructions,
                    )?;
                    let expr_regs = (central_reg..(central_reg + varlist.len() as u32))
                        .map(|reg| Expr::Var(Var::Reg(reg)))
                        .collect();
                    return Ok((varlist.clone(), expr_regs));
                }
            }
            // imbalanced & loadnill
            let num = (varlist.len() - exprlist.len()) as u32;
            let start_reg = res_alloc.reg_alloc.push(None);
            let mut extended = exprlist.clone();
            extended.push(Expr::Var(Var::Reg(start_reg)));
            for _ in 1..num {
                let reg = res_alloc.reg_alloc.push(None);
                extended.push(Expr::Var(Var::Reg(reg)));
            }
            IRGen::emit_iABx(instructions, OpName::LOADNIL, start_reg, num - 1);
            return Ok((varlist.clone(), extended));
        }
    }

    fn visit_table_ref(
        &mut self,
        table: &Expr,
        key: &Expr,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
        expect: Option<Expect>,
    ) -> Result<(bool, u32), CompileError> {
        let reg = if let Some(expect) = extract_expect_reg(expect)? {
            expect
        } else {
            res_alloc.reg_alloc.push(None)
        };
        let (_, table_reg) = self.visit_r_expr(table, res_alloc, instructions, None)?;
        let key_creg = self.visit_table_key(key, res_alloc, instructions)?;
        IRGen::emit_iABC(instructions, OpName::GETTABLE, reg, table_reg, key_creg);
        Ok((true, reg))
    }

    fn visit_table_key(
        &mut self,
        key: &Expr,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
    ) -> Result<u32, CompileError> {
        match key {
            &Expr::Var(Var::Name(ref name)) => {
                Ok(0x100 | res_alloc.const_alloc.push(ConstType::Str(name.clone())))
            }
            _ => {
                let (_, reg_or_const) = self.reg_constid_merge(key, res_alloc, instructions, None)?;
                Ok(reg_or_const)
            }
        }
    }

    fn visit_table_ctor(
        &mut self,
        entrys: &Vec<TableEntry>,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
        expect: Option<Expect>,
    ) -> Result<(bool, u32), CompileError> {
        // create table
        let result_reg = if let Some(expect) = extract_expect_reg(expect)? {
            expect
        } else {
            res_alloc.reg_alloc.push(None)
        };

        // empty table
        if entrys.is_empty() {
            return Ok((true, result_reg));
        }
        let (hash_part, array_part) = Self::split_table_entrys(entrys);

        IRGen::emit_iABC(
            instructions,
            OpName::NEWTABLE,
            result_reg,
            array_part.len().to_f8(),
            hash_part.len().to_f8(),
        );

        // add elements in the array_part
        // fine, I'll follow the standard
        let flush_num = array_part.len() as u32 / LFIELDS_PER_FLUSH;
        let residue_num = array_part.len() as u32 % LFIELDS_PER_FLUSH;
        let mut iter = array_part.into_iter();
        // allocate register for list element
        for _ in 0..(if flush_num > 0 {
            LFIELDS_PER_FLUSH
        } else {
            residue_num
        }) {
            res_alloc.reg_alloc.push(None);
        }
        // first dealing with flush chuck
        for flush_id in 0..flush_num {
            // reuse register pool in each flush
            let mut dest_reg = result_reg + 1;
            for _ in 0..LFIELDS_PER_FLUSH {
                self.visit_r_expr(
                    &iter.next().unwrap(),
                    res_alloc,
                    instructions,
                    Some(Expect::Reg(dest_reg)),
                )?;
                dest_reg += 1;
            }
            // FIXME: consider when flush_id is too large to encode
            IRGen::emit_iABC(
                instructions,
                OpName::SETLIST,
                result_reg,
                LFIELDS_PER_FLUSH,
                flush_id + 1,
            );
        }
        // then finish residue part
        let mut dest_reg = result_reg + 1;
        for _ in 0..residue_num {
            self.visit_r_expr(
                &iter.next().unwrap(),
                res_alloc,
                instructions,
                Some(Expect::Reg(dest_reg)),
            )?;
            dest_reg += 1;
        }
        if residue_num > 0 {
            IRGen::emit_iABC(
                instructions,
                OpName::SETLIST,
                result_reg,
                residue_num,
                flush_num + 1,
            );
        }
        // add pairs in the hash_part
        for (key, value) in hash_part {
            let key_rkc = self.visit_table_key(&key, res_alloc, instructions)?;
            let (_, value_rkc) = self.reg_constid_merge(&value, res_alloc, instructions, None)?;
            IRGen::emit_iABC(
                instructions,
                OpName::SETTABLE,
                result_reg,
                key_rkc,
                value_rkc,
            );
        }
        Ok((true, result_reg))
    }
}

impl IRGen {
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

    #[allow(non_snake_case)]
    fn emit_iAsBx(instructions: &mut Vec<OpMode>, op: OpName, A: u32, sBx: i32) {
        instructions.push(OpMode::iAsBx(op, A, sBx));
    }
}

/// helper functions
impl IRGen {
    /// split Vec<TableEntry> to hash part(named) and array part(unnamed)
    fn split_table_entrys(entrys: &Vec<TableEntry>) -> (Vec<(Expr, Expr)>, Vec<Expr>) {
        let mut hash_part = vec![];
        let mut array_part = vec![];
        for &(ref key, ref value) in entrys {
            match key {
                &Some(ref k) => hash_part.push((k.clone(), value.clone())),
                &None => array_part.push(value.clone()),
            }
        }
        (hash_part, array_part)
    }

    /// for RK(_) field, register id and const id can be merged
    /// use 9th bit as sign bit
    /// 8bit as num field
    /// signed (1) for const_id
    /// unsigned (0) for register
    fn reg_constid_merge(
        &mut self,
        expr: &Expr,
        res_alloc: &mut ResourceAlloc,
        instructions: &mut Vec<OpMode>,
        expect: Option<Expect>,
    ) -> Result<(bool, u32), CompileError> {
        let reg_or_const = match expr {
            &Expr::Num(num) => (
                false,
                0x100 | res_alloc.const_alloc.push(ConstType::Real(num)),
            ),
            &Expr::Boole(boolean) => (
                false,
                0x100 | res_alloc.const_alloc.push(ConstType::Boole(boolean)),
            ),
            &Expr::Str(ref s) => (
                false,
                0x100 | res_alloc.const_alloc.push(ConstType::Str(s.clone())),
            ),
            _ => self.visit_r_expr(expr, res_alloc, instructions, expect)?,
        };
        Ok(reg_or_const)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use parser::Parser;
    use std::str::Chars;
    #[test]
    pub fn global_arith() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            a, b = 2.5 , 2 * 4
            local c = (a + b) / 10.5
        ",
        )).unwrap();

        let mut compiler = IRGen::new();
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
    }

    #[test]
    fn function_def() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            local a = 2
            func = function()
                local b = 3 
                return a + b, b + a
            end
        ",
        )).unwrap();

        let mut compiler = IRGen::new();
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
    }

    #[test]
    pub fn function_call() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            local a = 2
            func = function(para)
                return a + para, 0
            end
            local b, c = func(1, 2)
            local d = b + c
        ",
        )).unwrap();
        let mut compiler = IRGen::new();
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
        // println!("{:?}", compiler.root_function);
    }
    #[test]
    pub fn boolean() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            local a, b = true, false
            local c = not ( 2 <= 3 or a == b )
        ",
        )).unwrap();
        let mut compiler = IRGen::new();
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
        // println!("{:?}", compiler.root_function);
    }
    #[test]
    pub fn if_else_clause() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            local a, b = true, false
            local c
            if a ~= b and 2 < 3 then
                c = 1
            elseif 3 <= 4 then
                c = 0
            else 
                c = 2
            end
        ",
        )).expect("Parse Error");
        let mut compiler = IRGen::new();
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
        // println!("{:?}", compiler.root_function);
    }
    #[test]
    pub fn while_clause() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            local i, sum = 0, 0
            while i <= 100 do 
                sum = sum + i
                if sum == 5000 then
                    break
                end
                i = i + 1
            end
        ",
        )).expect("Parse Error");
        let mut compiler = IRGen::new();
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
        // println!("Byte Code: {:?}", compiler.root_function);
    }
    #[test]
    pub fn numeric_for_clause() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            local sum = 0
            for i = 1, 100, 1 do
                sum = sum + i
                if sum == 1000 then
                    break
                end
            end
        ",
        )).expect("Parse Error");
        let mut compiler = IRGen::new();
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
        println!("Byte code: {:?}", compiler.root_function);
    }
    #[test]
    pub fn constant_format() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            local a, b, c = 1, 2, 1.5
        ",
        )).expect("Parse Error");
        let mut compiler = IRGen::new();
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
        println!("Byte code: {:?}", compiler.root_function);
    }
    #[test]
    pub fn table_constructor() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            local table = { name = 'Ann', age = 10 + 8, 
                           ['sch' + 'ool'] = 'SEIEE';  
                           1, 2, 3 }
        ",
        )).expect("Parse Error");
        let mut compiler = IRGen::new();
        println!("Ast: {:?}", ast);
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
        println!("Byte code: {:?}", compiler.root_function);
    }
    #[test]
    pub fn set_get_table() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            local table = { name = 'Ann' }
            (table)['age'] = 12
            table['subtable'] = { 1, 2 }
            local a, n, sub = table['age'], table['name'], 
                              table['subtable'][1]
        ",
        )).expect("Parse Error");
        let mut compiler = IRGen::new();
        println!("Ast: {:?}", ast);
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
        println!("Byte code: {:?}", compiler.root_function);
    }
    #[test]
    pub fn table_func() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            table = nil --Bypass
            local stu = {name='Ann', grades={'A', 'A-', 'B+'}}

            stu.change_name = function(self)
                self.name = 'Lee'
            end

            stu.grades.add_grade = function(self, g)
                table.insert(self, g)
            end

            stu.change_name(stu)
            stu.grades:add_grade('C')
        ",
        )).expect("Parse Error");
        let mut compiler = IRGen::new();
        println!("Ast: {:?}", ast);
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
        println!("Byte code: {:?}", compiler.root_function);
    }

    #[test]
    fn regression_one() {
        let ast = Parser::<Chars>::ast_from_text(&String::from(
            "\
            a = 1 + 2
            print(a)
        ",
        )).expect("Parse error");

        let mut compiler = IRGen::new();
        assert_eq!(compiler.generate_ir(&ast), Ok(()));
    }
}
