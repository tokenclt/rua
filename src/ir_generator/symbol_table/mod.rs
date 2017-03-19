use std::collections::{HashMap, HashSet};
use std::ptr;
use super::types::Usize;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Real,
    Str,
    Boole,
    UnKnow,
    Nil,
    Tuple(Vec<SymbolType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolScope {
    Local,
    UpValue(u32), // path_length, immidiate parent: 1
    Global,
}

#[derive(Debug)]
pub struct SymbolTable {
    symbols: HashMap<String, SymbolType>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable { symbols: HashMap::new() }
    }

    pub fn define(&mut self, name: &str, t: SymbolType) {
        // insert or update
        let slot = self.symbols.entry(name.to_string()).or_insert(t.clone());
        *slot = t;
    }

    pub fn lookup(&self, name: &str) -> Option<SymbolType> {
        self.symbols.get(name).cloned()
    }
}

#[derive(Debug)]
struct Scope {
    symbols: HashMap<String, Usize>,
    children: Vec<Scope>,
    parent: *mut Scope,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            symbols: HashMap::new(),
            children: vec![],
            parent: ptr::null_mut(),
        }
    }
}

#[derive(Debug)]
pub struct ScopedSymbolTableBuilder {
    current: *mut Scope,
    global_scope: HashSet<String>,
    root_scope: Box<Scope>,
}

#[allow(dead_code)]
impl ScopedSymbolTableBuilder {
    pub fn new() -> ScopedSymbolTableBuilder {
        let mut builder = ScopedSymbolTableBuilder {
            current: ptr::null_mut(),
            root_scope: Box::new(Scope::new()),
            global_scope: HashSet::new(),
        };
        unsafe {
            builder.current = builder.root_scope.as_mut() as *mut Scope;
        }
        builder
    }

    pub fn initialize_scope(&mut self) {
        unsafe {
            let current = &mut *self.current;
            current.children.push(Scope::new());
            let mut new = current.children.last_mut().unwrap();
            new.parent = self.current;
            self.current = new as *mut Scope;
        }
    }

    // TODO: add delete precessure
    pub fn finalize_scope(&mut self) {
        unsafe {
            (*self.current).children.clear();
            self.current = (*self.current).parent;
        }
    }

    pub fn define_global(&mut self, name: &str) {
        let slot = self.global_scope.insert(name.to_string());
    }

    pub fn define_local(&mut self, name: &str, pos: Usize) {
        unsafe {
            // avoiding duplication included
            let slot = (*self.current).symbols.entry(name.to_string()).or_insert(pos);
            *slot = pos;
        }
    }

    // lookup in current scope or parent scope
    // ret: scope type, pos
    pub fn lookup(&self, name: &str) -> Option<(SymbolScope, Usize)> {
        unsafe {
            let mut cursor = self.current;
            //  symbol can be find in current scope
            //  as_ref : check nullptr
            if let Some(_) = cursor.as_ref() {
                if let Some(&pos) = (*cursor).symbols.get(name) {
                    return Some((SymbolScope::Local, pos));
                }
            }
            // if not find, move up
            cursor = (*cursor).parent;
            let mut path_length = 1_u32;
            while let Some(_) = cursor.as_ref() {
                if let Some(&pos) = (*cursor).symbols.get(name) {
                    // pos: register in current level, not immidiate 
                    return Some((SymbolScope::UpValue(path_length), pos));
                } else {
                    path_length = path_length + 1;
                    cursor = (*cursor).parent;
                }
            }
        }
        // if not found, then lookup in global scope
        if self.global_scope.contains(name) {
            Some((SymbolScope::Global, 0))
        } else {
            None
        }
    }
}
