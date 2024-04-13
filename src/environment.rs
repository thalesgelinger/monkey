use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Env {
    store: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, key: String) -> Option<Object> {
        self.store.get(&key).cloned()
    }

    pub fn set(&mut self, key: String, value: Object) -> () {
        self.store.insert(key, value);
    }
}
