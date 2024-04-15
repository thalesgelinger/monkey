use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Env {
    store: HashMap<String, Object>,
    outer: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: &Env) -> Env {
        Env {
            store: HashMap::new(),
            outer: Some(Box::new(outer.clone())),
        }
    }

    pub fn get(&self, key: String) -> Option<Object> {
        let value = self.store.get(&key).cloned();
        match value {
            Some(v) => Some(v),
            None => match &self.outer {
                Some(outer) => outer.get(key),
                None => value,
            },
        }
    }

    pub fn set(&mut self, key: String, value: Object) -> () {
        self.store.insert(key, value);
    }
}
