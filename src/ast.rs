#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,

}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub id: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug)]
pub struct Stmt {
    pub exp: Exp,
}

#[derive(Debug, Clone)]
pub struct Exp {
    pub unary_exp: UnaryExp,
}

#[derive(Debug, Clone)]
pub enum UnaryExp {
    PExp(PrimaryExp),
    OpExp(UnaryOP, Box<UnaryExp>),
}

impl UnaryExp {
    pub fn new_pexp(pe: PrimaryExp) -> Self {
        UnaryExp::PExp(pe)
    }

    pub fn new_opexp(op: UnaryOP, ue: UnaryExp) -> Self {
        UnaryExp::OpExp(op, Box::new(ue))
    }
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
}

impl PrimaryExp {
    pub fn new_exp(exp: Exp) -> Self {
        PrimaryExp::Exp(Box::new(exp))
    }

    pub fn new_num(num: i32) -> Self {
        PrimaryExp::Number(num)
    }
}

#[derive(Debug,Clone)]
pub enum UnaryOP {
    Plus,
    Minus,
    Not,
}
