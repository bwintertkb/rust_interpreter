use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

use crate::object::{EnvCell, Objects};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Environment {
    pub store: Map,
    pub outer: Option<EnvCell>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Map(pub HashMap<String, Objects>);

impl Hash for Map {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (key, value) in &self.0 {
            key.hash(state);
            value.hash(state);
        }
    }
}

impl Environment {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self::default()))
    }

    pub fn get(&self, key: &str) -> Option<Objects> {
        let mut obj = self.store.0.get(key).cloned();

        if obj.is_none() && self.outer.is_some() {
            obj = self.outer.clone().unwrap().0.as_ref().borrow().get(key);
        }
        obj
    }

    pub fn set(&mut self, key: String, value: Objects) -> Objects {
        self.store.0.insert(key, value.clone());
        value
    }
}

pub fn new_enclosed_environment(outer: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
    Rc::new(RefCell::new(Environment {
        outer: Some(EnvCell(outer)),
        ..Default::default()
    }))
}
