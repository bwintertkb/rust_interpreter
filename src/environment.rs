use std::{collections::HashMap, hash::Hash};

use crate::object::Objects;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Environment {
    pub store: Map,
    pub outer: Option<*mut Environment>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Map(HashMap<String, Objects>);

impl Hash for Map {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (key, value) in &self.0 {
            key.hash(state);
            value.hash(state);
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, key: &str) -> Option<Objects> {
        let mut obj = self.store.0.get(key).cloned();

        if obj.is_none() && self.outer.is_some() {
            obj = unsafe { self.outer.clone().unwrap().as_mut().unwrap().get(key) }
        }
        obj
    }

    pub fn set(&mut self, key: String, value: Objects) -> Objects {
        self.store.0.insert(key, value.clone());
        value
    }
}

pub fn new_enclosed_environment(outer: *mut Environment) -> Environment {
    let mut env = Environment::default();
    env.outer = Some(outer);
    env
}
