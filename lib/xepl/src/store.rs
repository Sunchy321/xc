use std::collections::HashMap;

use xc_span::Symbol;

use crate::value::Value;

#[derive(Clone)]
pub struct Store {
    variables: HashMap<Symbol, Value>,
}

impl Store {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: Symbol, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn get(&self, name: Symbol) -> Option<&Value> {
        self.variables.get(&name)
    }
}
