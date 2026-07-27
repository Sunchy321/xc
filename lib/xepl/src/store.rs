use std::collections::HashMap;

use xc_ast::func::Func;
use xc_ast::ptr::P;
use xc_span::Symbol;

use crate::value::Value;

#[derive(Clone)]
pub struct Store {
    variables: HashMap<Symbol, Value>,
    functions: HashMap<Symbol, P<Func>>,
}

impl Store {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: Symbol, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn remove(&mut self, name: Symbol) {
        self.variables.remove(&name);
    }

    pub fn get(&self, name: Symbol) -> Option<&Value> {
        self.variables.get(&name)
    }

    pub fn add_func(&mut self, func: P<Func>) {
        self.functions.insert(func.ident.name, func);
    }

    pub fn get_func(&self, name: Symbol) -> Option<&P<Func>> {
        self.functions.get(&name)
    }
}
