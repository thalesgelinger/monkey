use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::Object;

#[derive(Debug, Clone)]
pub struct Env {
    store: RefCell<HashMap<String, Object>>,
    outer: Option<Rc<Env>>,
}

impl Env {
    pub fn new() -> Rc<Env> {
        Rc::new(Env {
            store: HashMap::new().into(),
            outer: None,
        })
    }

    pub fn new_enclosed(outer: &Rc<Env>) -> Rc<Env> {
        Rc::new(Env {
            store: HashMap::new().into(),
            outer: Some(Rc::clone(outer)),
        })
    }

    pub fn get(&self, key: String) -> Option<Object> {
        let value = self.store.borrow().get(&key).cloned();
        match value {
            Some(v) => Some(v),
            None => match &self.outer {
                Some(outer) => outer.get(key),
                None => value,
            },
        }
    }

    pub fn set(&self, key: String, value: Object) -> () {
        self.store.borrow_mut().insert(key, value);
    }
}
