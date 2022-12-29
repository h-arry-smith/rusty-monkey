use std::{collections::HashMap, fmt::Display};

use crate::ast::{Identifier, Stmt};

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Function(Vec<Identifier>, Stmt, Box<Environment>),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(int) => write!(f, "{}", int),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Null => write!(f, "null"),
            Object::Return(object) => write!(f, "{}", object),
            Object::Function(parameters, _, _) => write!(f, "<fn/{}>", parameters.len()),
            Object::String(string) => write!(f, "{}", string),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    pub outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosing(outer_env: Box<Environment>) -> Self {
        let mut environment = Environment::new();
        environment.outer = Some(outer_env);

        environment
    }

    pub fn set(&mut self, identifier_literal: String, object: Object) -> Option<Object> {
        self.store.insert(identifier_literal, object)
    }

    pub fn get(&self, identifier_literal: &str) -> Option<Object> {
        match self.store.get(identifier_literal) {
            Some(object) => Some(object.clone()),
            None => {
                if let Some(ref outer) = self.outer {
                    outer.get(identifier_literal)
                } else {
                    None
                }
            }
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
