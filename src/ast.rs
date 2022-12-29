#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum Stmt {
    Let(Identifier, Expr),
    Return(Expr),
    Expr(Expr),
    Block(Vec<Stmt>),
}

#[derive(Debug)]
pub enum Expr {
    Identifier(String),
    IntegerLiteral(i64),
    Prefix(String, Box<Expr>),
    Infix(Box<Expr>, String, Box<Expr>),
    Boolean(bool),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    Functionliteral(Vec<Expr>, Box<Stmt>),
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}
