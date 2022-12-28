use crate::{
    ast::*,
    object::{Object, FALSE, NULL, TRUE},
};

pub fn eval_program(program: Program) -> Object {
    eval_statements(program.statements)
}

fn eval_statements(statements: Vec<Stmt>) -> Object {
    let mut result = NULL;

    for statement in statements {
        result = eval_statement(statement);
    }

    result
}

fn eval_statement(statement: Stmt) -> Object {
    match statement {
        Stmt::Let(_, _) => todo!(),
        Stmt::Return(_) => todo!(),
        Stmt::Expr(expr) => eval_expression(expr),
        Stmt::Block(_) => todo!(),
    }
}

fn eval_expression(expr: Expr) -> Object {
    match expr {
        Expr::Identifier(_) => todo!(),
        Expr::IntegerLiteral(integer) => Object::Integer(integer),
        Expr::Prefix(operator, right) => {
            let right = eval_expression(*right);
            eval_prefix_expression(operator, right)
        }
        Expr::Infix(left, operator, right) => {
            let left = eval_expression(*left);
            let right = eval_expression(*right);
            eval_infix_expression(operator, left, right)
        }
        Expr::Boolean(boolean) => boolean_object(boolean),
        Expr::If(_, _, _) => todo!(),
        Expr::Functionliteral(_, _) => todo!(),
        Expr::Call(_, _) => todo!(),
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => NULL,
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(integer) => Object::Integer(-integer),
        _ => NULL,
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    match left {
        Object::Integer(left) => match right {
            Object::Integer(right) => eval_integer_infix_expression(operator, left, right),
            _ => NULL
        }
        _ => {
            match operator.as_str() {
                "==" => boolean_object(left == right),
                "!=" => boolean_object(left != right),
                _ => NULL 
            }
        },
    }
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Object {
    match operator.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => boolean_object(left < right),
        ">" => boolean_object(left > right),
        "==" => boolean_object(left == right),
        "!=" => boolean_object(left != right),
        _ => NULL,
    }
}

fn boolean_object(boolean: bool) -> Object {
    if boolean {
        TRUE
    } else {
        FALSE
    }
}
