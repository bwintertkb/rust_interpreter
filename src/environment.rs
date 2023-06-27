use std::collections::HashMap;

use crate::object::Objects;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Environment {
    pub store: HashMap<String, Objects>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, key: &str) -> Option<Objects> {
        self.store.get(key).cloned()
    }

    pub fn set(&mut self, key: String, value: Objects) -> Objects {
        self.store.insert(key, value.clone());
        value
    }
}
