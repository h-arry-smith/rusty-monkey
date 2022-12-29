use std::fmt::Display;

use crate::{
    ast::*,
    object::{Object, FALSE, NULL, TRUE, Environment},
};

pub type EvaluationResult = Result<Object, EvaluationError>;
pub struct EvaluationError(String);
impl Display for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn eval_program(program: Program, env: &mut Environment) -> EvaluationResult {
    let mut result = NULL;

    for statement in program.statements {
        result = eval_statement(statement, env)?;

        if let Object::Return(value) = result { return Ok(*value); }
    }

    Ok(result)
}
fn eval_statement(statement: Stmt, env: &mut Environment) -> EvaluationResult {
    match statement {
        Stmt::Let(identifier, expr) => {
            let value = eval_expression(expr, env)?;
            env.set(identifier.0, value.clone());
            Ok(value)
        },
        Stmt::Return(expr) => {
            let value = eval_expression(expr, env)?;
            Ok(Object::Return(Box::new(value)))
        },
        Stmt::Expr(expr) => eval_expression(expr, env),
        Stmt::Block(_) => eval_block_statement(statement, env),
    }
}

fn eval_block_statement(block: Stmt, env: &mut Environment) -> EvaluationResult {
    let mut result = NULL;

    if let Stmt::Block(statements) = block {
        for statement in statements {
            result = eval_statement(statement, env)?;

            if let Object::Return(_) = result { return Ok(result); }
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
        Expr::If(condition, consequence, alternative) => eval_if_expression(*condition, *consequence, alternative, env),
        Expr::Functionliteral(_, _) => todo!(),
        Expr::Call(_, _) => todo!(),
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> EvaluationResult {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(EvaluationError(format!("unknown operator: {} {}", operator, right))),
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
            _ => Err(EvaluationError(format!("type mismatch {} {} {}", left, operator, right)))
        }
        _ => {
            match operator.as_str() {
                "==" => Ok(boolean_object(left == right)),
                "!=" => Ok(boolean_object(left != right)),
                _ => Err(EvaluationError(format!("unknown operator: {} {} {}", left, operator, right)))
            }
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
        _ => { return Err(EvaluationError(format!("unknown operator: {} {} {}", left, operator, right))); }
    };

    Ok(result)
}

fn eval_if_expression(condition: Expr, consequence: Stmt, alternative: Option<Box<Stmt>>, env: &mut Environment) -> EvaluationResult {
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
    match env.get(literal) {
        Some(object) => Ok(object),
        None => Err(EvaluationError(format!("identifier not found: {}", literal))),
    }
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
        _ => true
    }
}
