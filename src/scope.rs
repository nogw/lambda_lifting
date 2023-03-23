use crate::term::Expr;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Scope {
    pub prefix: String,
    pub context: HashMap<String, Expr>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            prefix: String::new(),
            context: HashMap::new(),
        }
    }

    pub fn new_scope(&mut self, prefix: &String) {
        self.prefix = prefix.clone();
        self.context.clear();
    }

    pub fn lookup(&self, key: String) -> Expr {
        if let Some(value) = self.context.get(&key) {
            return value.clone();
        }

        Expr::Var(key)
    }
}
