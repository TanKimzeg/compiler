use crate::ast::Exp;
use crate::ast::Block;

pub enum Stmt {
    Ret(Option<Exp>),
    LVal(String, Exp),
    Block(Block),
    Exp(Option<Exp>),
    Cond(Exp, Box<Stmt>, Option<Box<Stmt>>),
    While(Exp, Box<Stmt>),
    Break,
    Continue,
}
