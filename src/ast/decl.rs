use crate::ast::exp::Exp;


pub enum Decl {
    ConstDecl(ConstDecl),
}

pub struct ConstDecl {
    pub ty: BType,
    pub defs: Vec<ConstDef>,
}

pub enum BType {
    Int,
}

pub struct ConstDef {
    pub id: String,
    pub init: ConstInit,
}

pub enum ConstInit {
    ConstExp(Exp),
}

