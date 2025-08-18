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
pub enum Stmt {
    Ret(Exp),
}

#[derive(Debug, Clone)]
pub enum Exp {
    Single(UnaryExp),
}

#[derive(Debug, Clone)]
pub enum UnaryExp {
    PExp(PrimaryExp),
    OpExp(UnaryOP, Box<UnaryExp>),
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
}

#[derive(Debug,Clone)]
pub enum UnaryOP {
    Plus,
    Minus,
    Not,
}
