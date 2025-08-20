use crate::ast::block::*;


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
