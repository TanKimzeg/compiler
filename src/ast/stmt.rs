use crate::ast::Exp;
use crate::ast::Block;
use crate::ast::LVal;

pub enum Stmt {
    Ret(Option<Exp>),
    Assign(LVal, Exp),
    Block(Block),
    Exp(Option<Exp>),
    Cond(Exp, Box<Stmt>, Option<Box<Stmt>>),
    While(Exp, Box<Stmt>),
    Break,
    Continue,
}
