use std::collections::HashMap;
use std::ptr;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Real,
    Str,
    Boole,
    UnKnow,
    Nil,
    Tuple(Vec<SymbolType>),
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
struct Scope{
    symbols: HashMap<String, SymbolType>,
    children: Vec<Scope>,
    parent: *mut Scope,
}

impl Scope {
    pub fn new() -> Scope{
        Scope{symbols: HashMap::new(), children: vec![], parent: ptr::null_mut()}
    }
}

#[derive(Debug)]
pub struct ScopedSymbolTableBuilder{
    current: *mut Scope,
    global_scope: Box<Scope>,
}

#[allow(dead_code)]
impl ScopedSymbolTableBuilder {
    pub fn new() -> ScopedSymbolTableBuilder{
        let mut builder = ScopedSymbolTableBuilder{current: ptr::null_mut(), 
        global_scope: Box::new(Scope::new())};
        builder.current = Box::into_raw(builder.global_scope);
        unsafe{
            builder.global_scope = Box::from_raw(builder.current);    
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

    pub fn finalize_scope(&mut self) {
        unsafe{
            self.current = (*self.current).parent;
        }
    }

    pub fn define_global(&mut self, name: &str, t: SymbolType) {
        let slot = self.global_scope.symbols.entry(name.to_string()).or_insert(t.clone());
        *slot = t;
    } 

    pub fn define_local(&mut self, name: &str, t: SymbolType) {
        unsafe{
            let slot = (*self.current).symbols.entry(name.to_string()).or_insert(t.clone());
            *slot = t;
        }
    }

    pub fn lookup(&self, name: &str) -> Option<SymbolType> {
        unsafe{
            let mut cursor = self.current;
            // as_ref() is amazing!!! 
            // hide difference between Box and pointer
            while let Some(_) = cursor.as_ref() {
                print!("{:?}", *cursor);
                if let Some(t) = (*cursor).symbols.get(name) {
                    
                    return Some(t.clone());
                }else{
                    cursor = (*cursor).parent;
                }
            }
            return None;
        }
    } 
}
