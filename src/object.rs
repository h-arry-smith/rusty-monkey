use std::{collections::HashMap, fmt::Display};

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(int) => write!(f, "{}", int),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Null => write!(f, "null"),
            Object::Return(object) => write!(f, "{}", object),
        }
    }
}

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn set(&mut self, identifier_literal: String, object: Object) -> Option<Object> {
        self.store.insert(identifier_literal, object)
    }

    pub fn get(&self, identifier_literal: &str) -> Option<Object> {
        self.store.get(identifier_literal).cloned()
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
