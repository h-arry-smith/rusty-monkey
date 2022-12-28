use crate::{ast::*, object::Object};

pub fn eval_program(program: Program) -> Object {
    eval_statements(program.statements)
}

fn eval_statements(statements: Vec<Stmt>) -> Object {
    let mut result = Object::Null;

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
        Expr::Prefix(_, _) => todo!(),
        Expr::Infix(_, _, _) => todo!(),
        Expr::Boolean(_) => todo!(),
        Expr::If(_, _, _) => todo!(),
        Expr::Functionliteral(_, _) => todo!(),
        Expr::Call(_, _) => todo!(),
    }
}
