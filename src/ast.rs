pub struct CompUnit {
    pub func_def: FuncDef,

}

pub struct FuncDef {
    pub func_type: FuncType,
    pub id: String,
    pub block: Block,
}

pub enum FuncType {
    Int,
}

pub struct Block {
    pub stmt: Stmt,
}

pub enum Stmt {
    Ret(Exp),
}

pub enum Exp {
    LOrExp(LOrExp),
}

pub enum LOrExp {
    Single(LAndExp),
    // OAExp(Box<LOrExp>, Box<LAndExp>),
    Binary(Box<Binary<LOrExp, LAndExp>>),
}

pub enum LAndExp {
    Single(EqExp),
    Binary(Box<Binary<LAndExp, EqExp>>),
}


pub enum EqExp {
    Single(RelExp),
    Binary(Box<Binary<EqExp, RelExp>>),
}

pub enum RelExp {
    Single(AddExp),
    // ROAExp(Box<RelExp>, RelOp, Box<AddExp>),
    Binary(Box<Binary<RelExp, AddExp>>),
}

pub enum AddExp {
    Single(MulExp),
    // AOMExp(Box<AddExp>, AddOp, Box<MulExp>),
    Binary(Box<Binary<AddExp, MulExp>>),
}

pub enum BinOp {
    Add, Sub,
    Mul, Div, Mod,
    LT, GT, LE, GE,
    Eq, NEq,
    LAnd, LOr,
}

pub enum MulExp {
    Single(UnaryExp),
    // MOUExp(Box<MulExp>, MulOp, Box<UnaryExp>),
    Binary(Box<Binary<MulExp, UnaryExp>>),
}

pub struct Binary<T, S> {
    pub lhs: Box<T>,
    pub op: BinOp,
    pub rhs: Box<S>,
}


pub enum UnaryExp {
    PExp(PrimaryExp),
    OpExp(UnaryOP, Box<UnaryExp>),
}

pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
}

pub enum UnaryOP {
    Plus,
    Minus,
    Not,
}
