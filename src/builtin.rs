use std::collections::HashMap;

use crate::{
    evaluator::{EvaluationError, EvaluationResult},
    object::Object,
};

pub type BuiltinFunctions = HashMap<String, Object>;

pub fn build_builtins() -> BuiltinFunctions {
    let mut functions = HashMap::new();

    functions.insert("len".to_string(), Object::Builtin(monkey_len));

    functions
}

fn monkey_len(arguments: Vec<Object>) -> EvaluationResult {
    if arguments.len() != 1 {
        return Err(EvaluationError(format!(
            "wrong number of arguments. got={}, want=1",
            arguments.len()
        )));
    }

    let arg = arguments.first().unwrap();

    match arg {
        Object::String(string) => Ok(Object::Integer(string.len() as i64)),
        _ => Err(EvaluationError(
            "argument to `len` not supported".to_string(),
        )),
    }
}
