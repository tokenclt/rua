use std::collections::HashMap;
use super::types::{ConstType, Usize, ToBytecode};

#[derive(Debug)]
pub struct RegisterAlloc {
    name_indexer: HashMap<String, u32>,
    counter: u32,
}

impl RegisterAlloc {
    pub fn new() -> RegisterAlloc {
        RegisterAlloc {
            name_indexer: HashMap::new(),
            counter: 0,
        }
    }

    pub fn get_register(&mut self, name: Option<&str>) -> Usize {
        match name {
            Some(name) => {
                match self.name_indexer.get(name) {
                    Some(&reg) => reg,
                    None => {
                        self.name_indexer.insert(name.to_string(), self.counter);
                        self.counter += 1;
                        self.counter - 1
                    }
                }
            }
            None => {
                self.name_indexer.insert("__TEMP__".to_string() + &self.counter.to_string(), self.counter);
                self.counter += 1;
                self.counter - 1
            }  
        }

    }

    pub fn size(&self) -> Usize {
        self.counter
    }
}

#[derive(Debug)]
pub struct ConstAlloc {
    storage: Vec<ConstType>,
    str_index: HashMap<String, usize>,
}

impl ConstAlloc {
    pub fn new() -> ConstAlloc {
        ConstAlloc { storage: Vec::new() ,str_index: HashMap::new()}
    }

    pub fn push(&mut self, val: ConstType) -> Usize {
        match val{
            ConstType::Str(ref s) => {
                if let Some(&pos) = self.str_index.get(s) {
                    pos as Usize
                }else{
                    self.storage.push(val.clone());
                    let pos = self.storage.len() - 1;
                    self.str_index.insert(s.clone(), pos);
                    pos as Usize
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

    pub fn dump(&self) -> Vec<Usize> {
        let mut const_list = Vec::<u32>::new();
        //  size of constant list
        const_list.push(self.size() as Usize);
        for val in &self.storage {
            match *val {
                ConstType::Nil => {
                    //  type 0 = Nil
                    const_list.push(0);
                }
                ConstType::Boole(b) => {
                    //  type 1 = Boolean
                    const_list.push(1);
                    const_list.push(b as u32);
                }
                ConstType::Real(num) => {
                    //  type 2 = Number
                    const_list.push(2);
                    const_list.append(&mut num.to_bytecode());
                }
                ConstType::Str(ref s) => {
                    const_list.push(3);
                    // type 3 = string
                    const_list.push(s.len() as Usize);
                    const_list.append(&mut s.to_bytecode());
                }
            }
        }
        const_list
    }
}

#[derive(Debug)]
pub struct ResourceAlloc {
    pub reg_alloc: RegisterAlloc,
    pub const_alloc: ConstAlloc,
}

impl ResourceAlloc {
    pub fn new() -> ResourceAlloc {
        ResourceAlloc {
            reg_alloc: RegisterAlloc::new(),
            const_alloc: ConstAlloc::new(),
        }
    }
}
