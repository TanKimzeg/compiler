use std::collections::HashMap;

use koopa::ir::Value;

use crate::ast::stmt::*;
use crate::ast::decl::*;

pub type SymbolTable = HashMap<String, IdentInfo>;

pub enum IdentInfo {
    Const(i32),
    Var(VarInfo),
}

pub struct VarInfo {
    pub dest: Value,
}
pub struct Block {
    pub items: Vec<BlockItem>,
    pub symbols: SymbolTable,
}

impl Block {
    pub fn new(items: Vec<BlockItem>) -> Self {
        let symbols = SymbolTable::new();
        Self { items, symbols }
    }
}

pub enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}