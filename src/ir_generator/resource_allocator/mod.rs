use std::collections::HashMap;
use std::ptr;
use super::types::*;

#[derive(Debug)]
pub struct RegisterAlloc {
    name_indexer: HashMap<String, u32>,
    counter: u32,
}

// TODO: add register pool for nameless varibles
//       so that temporary register can be reused
impl RegisterAlloc {
    pub fn new() -> RegisterAlloc {
        RegisterAlloc {
            name_indexer: HashMap::new(),
            counter: 0,
        }
    }

    pub fn push(&mut self, name: Option<&str>) -> Usize {
        match name {
            Some(name) => {
                self.name_indexer.insert(name.to_string(), self.counter);
                self.increament_count();
                self.counter - 1
            }
            None => {
                self.name_indexer.insert("__TEMP__".to_string() + &self.counter.to_string(),
                                         self.counter);
                self.increament_count();
                self.counter - 1
            }  
        }
    }

    //  allocate an register with provided register position
    //  used when convering a temperary register to a named register
    pub fn push_set(&mut self, name: &str, pos: Usize) -> Usize {
        self.name_indexer.insert(name.to_string(), pos);
        // do not need to increase counter
        pos
    }

    pub fn size(&self) -> Usize {
        self.counter
    }

    fn increament_count(&mut self) {
        self.counter += 1;
        if self.counter > 0xFF {
            panic!("Register number overflow");
        }
    }
}

#[derive(Debug)]
pub struct ConstAlloc {
    storage: Vec<ConstType>,
    str_index: HashMap<String, usize>,
}

impl ConstAlloc {
    pub fn new() -> ConstAlloc {
        ConstAlloc {
            storage: Vec::new(),
            str_index: HashMap::new(),
        }
    }

    pub fn push(&mut self, val: ConstType) -> Usize {
        if self.storage.len() >= 0xFF {
            panic!("Const value number overflow");
        }
        match val {
            ConstType::Str(ref s) => {
                if let Some(&final_pos) = self.str_index.get(s) {
                    final_pos as Usize
                } else {
                    self.storage.push(val.clone());
                    let final_pos = self.storage.len() - 1;
                    self.str_index.insert(s.clone(), final_pos);
                    final_pos as Usize
                }
            }
            _ => {
                self.storage.push(val.clone());
                (self.storage.len() - 1) as Usize
            }
        }
    }

    pub fn size(&self) -> usize {
        self.storage.len()
    }

    pub fn dump(self) -> Vec<ConstType> {
        self.storage
    }
}

#[derive(Debug)]
pub struct UpValueAlloc {
    name_indexer: HashMap<String, (Usize, Usize, Usize)>, /* (depth, pos in upvalue list, pos in parent stack) */
    counter: usize,
}

impl UpValueAlloc {
    pub fn new() -> UpValueAlloc {
        UpValueAlloc {
            name_indexer: HashMap::new(),
            counter: 0,
        }
    }

    pub fn push(&mut self, name: &str, depth: Usize, final_pos_in_parent: Usize) -> Usize {
        match self.name_indexer.get(name) {
            Some(&(_, pos, _)) => pos, // avoid duplication
            None => {
                self.name_indexer.insert(name.to_string(),
                                         (depth, self.counter as Usize, final_pos_in_parent));
                self.counter = self.counter + 1;
                (self.counter - 1) as Usize
            }
        }
    }

    pub fn into_list(self) -> Vec<(bool, Usize, Usize)> {
        let mut list = Vec::with_capacity(self.name_indexer.len());
        for &(depth, pos_in_ul, pos_in_parent) in self.name_indexer.values() {
            // is_immidiate
            // when is_immidiate, generate move
            //     else generate getupvalue
            list.push((if depth > 1 { false } else { true }, pos_in_ul, pos_in_parent));
        }
        list
    }

    /// if upvalue is already pushed into upvalue list
    pub fn get(&self, name: &str) -> Option<Usize> {
        self.name_indexer.get(name).map(|t| t.0) // extract pos in upvalue list
    }

    // modify the register number where upvalue stored in upper scope
    pub fn set_upper_index(&mut self, name: &str, final_pos: Usize) {
        let &(depth, self_final_pos, _) = self.name_indexer.get(name).unwrap();
        *self.name_indexer.get_mut(name).unwrap() = (depth, self_final_pos, final_pos);
    }

    pub fn size(&self) -> usize {
        self.name_indexer.len()
    }
}

#[derive(Debug)]
/// function prototypes
pub struct FunctionAlloc {
    functions: Vec<FunctionChunk>,
}

impl FunctionAlloc {
    pub fn new() -> FunctionAlloc {
        FunctionAlloc { functions: vec![] }
    }

    pub fn push(&mut self, function: FunctionChunk) -> Usize {
        self.functions.push(function);
        (self.functions.len() - 1) as Usize
    }

    pub fn size(&self) -> usize {
        self.functions.len()
    }

    pub fn get_function_prototypes(self) -> Vec<FunctionChunk> {
        self.functions
    }
}

#[derive(Debug)]
/// label allocator
pub struct LabelAlloc {
    counter: i32,
}

impl LabelAlloc {
    pub fn new() -> LabelAlloc {
        LabelAlloc { counter: 0 }
    }

    pub fn new_label(&mut self) -> i32 {
        self.counter += 1;
        self.counter
    }
}

#[derive(Debug)]
pub struct ResourceAlloc {
    pub reg_alloc: RegisterAlloc,
    pub const_alloc: ConstAlloc,
    pub function_alloc: FunctionAlloc,
    pub upvalue_alloc: UpValueAlloc,
    pub label_alloc: LabelAlloc,
    pub parent: *mut ResourceAlloc,
    loop_exit: Option<Label>,
}

impl ResourceAlloc {
    pub fn new() -> ResourceAlloc {
        ResourceAlloc {
            reg_alloc: RegisterAlloc::new(),
            const_alloc: ConstAlloc::new(),
            function_alloc: FunctionAlloc::new(),
            upvalue_alloc: UpValueAlloc::new(),
            label_alloc: LabelAlloc::new(),
            parent: ptr::null_mut(),
            loop_exit: None,
        }
    }

    pub fn parent(mut self, p: *mut ResourceAlloc) -> ResourceAlloc {
        self.parent = p;
        self
    }

    pub unsafe fn propagate_upvalue(&mut self,
                                    upvalue_name: &str,
                                    final_pos: Usize,
                                    depth: Usize)
                                    -> Usize {
        //  upvalue is already pushed into upvalue list
        if let Some(pos) = self.upvalue_alloc.get(upvalue_name) {
            return pos;
        }

        let mut current = self as *mut ResourceAlloc;
        let mut parent = self.parent;
        let mut upvalue_index = (*current).upvalue_alloc.push(upvalue_name, depth, final_pos);
        // savefe the immidiate parent upvalue position, for returning
        let immidiate_upvalue_index = upvalue_index;
        // excute when depth >= 2
        for i in 1..depth {
            //  alloc upvalue in parent, and get where upvalue stored
            upvalue_index = (*parent).upvalue_alloc.push(upvalue_name, depth - i, final_pos);
            //  use this value to update current upvalue list
            (*current).upvalue_alloc.set_upper_index(upvalue_name, upvalue_index);
            // move up
            current = parent;
            parent = (*parent).parent;
        }
        immidiate_upvalue_index
    }

    pub fn set_loop_exit(&mut self, label: Label) {
        self.loop_exit = Some(label);
    }

    pub fn clear_loop_exit(&mut self) {
        self.loop_exit = None;
    }

    pub fn get_loop_exit(&self) -> Option<Label> {
        self.loop_exit
    }
}
