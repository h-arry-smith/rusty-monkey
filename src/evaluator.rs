use std::fmt::Display;

use crate::{
    ast::*,
    builtin::build_builtins,
    object::{Environment, Object, FALSE, NULL, TRUE},
};

pub type EvaluationResult = Result<Object, EvaluationError>;
pub struct EvaluationError(pub String);
impl Display for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn eval_program(program: Program, env: &mut Environment) -> EvaluationResult {
    let mut result = NULL;

    for statement in program.statements {
        result = eval_statement(statement, env)?;

        if let Object::Return(value) = result {
            return Ok(*value);
        }
    }

    Ok(result)
}
fn eval_statement(statement: Stmt, env: &mut Environment) -> EvaluationResult {
    match statement {
        Stmt::Let(identifier, expr) => {
            let value = eval_expression(expr, env)?;
            env.set(identifier.0, value.clone());
            Ok(value)
        }
        Stmt::Return(expr) => {
            let value = eval_expression(expr, env)?;
            Ok(Object::Return(Box::new(value)))
        }
        Stmt::Expr(expr) => eval_expression(expr, env),
        Stmt::Block(_) => eval_block_statement(statement, env),
    }
}

fn eval_block_statement(block: Stmt, env: &mut Environment) -> EvaluationResult {
    let mut result = NULL;

    if let Stmt::Block(statements) = block {
        for statement in statements {
            result = eval_statement(statement, env)?;

            if let Object::Return(_) = result {
                return Ok(result);
            }
        }
    }

    Ok(result)
}

fn eval_expression(expr: Expr, env: &mut Environment) -> EvaluationResult {
    match expr {
        Expr::Identifier(literal) => eval_identifier(&literal, env),
        Expr::IntegerLiteral(integer) => Ok(Object::Integer(integer)),
        Expr::Prefix(operator, right) => {
            let right = eval_expression(*right, env)?;
            eval_prefix_expression(operator, right)
        }
        Expr::Infix(left, operator, right) => {
            let left = eval_expression(*left, env)?;
            let right = eval_expression(*right, env)?;
            eval_infix_expression(operator, left, right)
        }
        Expr::Boolean(boolean) => Ok(boolean_object(boolean)),
        Expr::If(condition, consequence, alternative) => {
            eval_if_expression(*condition, *consequence, alternative, env)
        }
        Expr::Functionliteral(params, body) => {
            Ok(Object::Function(params, *body, Box::new(env.clone())))
        }
        Expr::Call(function, arguments) => {
            let function = eval_expression(*function, env)?;
            let args = eval_expressions(arguments, env)?;

            apply_function(function, args)
        }
        Expr::StringLiteral(string) => Ok(Object::String(string)),
    }
}

fn eval_expressions(
    exprs: Vec<Expr>,
    env: &mut Environment,
) -> Result<Vec<Object>, EvaluationError> {
    let mut result = Vec::new();
    for expr in exprs {
        let evaluated = eval_expression(expr, env)?;
        result.push(evaluated);
    }

    Ok(result)
}

fn eval_prefix_expression(operator: String, right: Object) -> EvaluationResult {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(EvaluationError(format!(
            "unknown operator: {} {}",
            operator, right
        ))),
    }
}

fn eval_bang_operator_expression(right: Object) -> EvaluationResult {
    match right {
        TRUE => Ok(FALSE),
        FALSE => Ok(TRUE),
        NULL => Ok(TRUE),
        _ => Ok(FALSE),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> EvaluationResult {
    match right {
        Object::Integer(integer) => Ok(Object::Integer(-integer)),
        _ => Err(EvaluationError(format!("unknown operator: -{}", right))),
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> EvaluationResult {
    match left {
        Object::Integer(left) => match right {
            Object::Integer(right) => eval_integer_infix_expression(operator, left, right),
            _ => Err(EvaluationError(format!(
                "type mismatch {} {} {}",
                left, operator, right
            ))),
        },
        Object::String(left) => match right {
            Object::String(right) => eval_string_infix_expression(operator, left, right),
            _ => Err(EvaluationError(format!(
                "type mismatch {} {} {}",
                left, operator, right
            ))),
        },
        _ => match operator.as_str() {
            "==" => Ok(boolean_object(left == right)),
            "!=" => Ok(boolean_object(left != right)),
            _ => Err(EvaluationError(format!(
                "unknown operator: {} {} {}",
                left, operator, right
            ))),
        },
    }
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> EvaluationResult {
    let result = match operator.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => boolean_object(left < right),
        ">" => boolean_object(left > right),
        "==" => boolean_object(left == right),
        "!=" => boolean_object(left != right),
        _ => {
            return Err(EvaluationError(format!(
                "unknown operator: {} {} {}",
                left, operator, right
            )));
        }
    };

    Ok(result)
}

fn eval_string_infix_expression(operator: String, left: String, right: String) -> EvaluationResult {
    let result = match operator.as_str() {
        "+" => Object::String(format!("{}{}", left, right)),
        _ => {
            return Err(EvaluationError(format!(
                "unknown operator: {} {} {}",
                left, operator, right
            )));
        }
    };

    Ok(result)
}

fn eval_if_expression(
    condition: Expr,
    consequence: Stmt,
    alternative: Option<Box<Stmt>>,
    env: &mut Environment,
) -> EvaluationResult {
    let condition = eval_expression(condition, env)?;

    if is_truthy(condition) {
        eval_statement(consequence, env)
    } else if let Some(alt) = alternative {
        eval_statement(*alt, env)
    } else {
        Ok(NULL)
    }
}

fn eval_identifier(literal: &str, env: &mut Environment) -> EvaluationResult {
    if let Some(object) = env.get(literal) {
        return Ok(object);
    }

    let builtins = build_builtins();

    if let Some(builtin) = builtins.get(literal) {
        return Ok(builtin.clone());
    }

    return Err(EvaluationError(format!(
        "identifier not found: {}",
        literal
    )));
}

fn apply_function(function: Object, arguments: Vec<Object>) -> EvaluationResult {
    match function {
        Object::Function(parameters, body, environment) => {
            let mut extended_env = extend_function_environment(*environment, parameters, arguments);
            let evaluated = eval_block_statement(body, &mut extended_env)?;

            match evaluated {
                Object::Return(value) => Ok(*value),
                _ => Ok(evaluated),
            }
        }
        Object::Builtin(builtin_fn) => builtin_fn(arguments),
        _ => Err(EvaluationError(format!("not a function: {}", function))),
    }
}

fn extend_function_environment(
    outer_environment: Environment,
    parameters: Vec<Identifier>,
    arguments: Vec<Object>,
) -> Environment {
    let mut extended_env = Environment::new_enclosing(Box::new(outer_environment));

    for (i, parameter) in parameters.iter().enumerate() {
        extended_env.set(parameter.0.clone(), arguments[i].clone());
    }

    extended_env
}

fn boolean_object(boolean: bool) -> Object {
    if boolean {
        TRUE
    } else {
        FALSE
    }
}

fn is_truthy(object: Object) -> bool {
    match object {
        Object::Boolean(boolean) => boolean,
        Object::Null => false,
        _ => true,
    }
}
