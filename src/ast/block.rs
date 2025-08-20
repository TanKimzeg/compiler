use std::collections::HashMap;

use crate::ast::stmt::*;
use crate::ast::decl::*;

pub struct Block {
    pub items: Vec<BlockItem>,
    pub symbols: HashMap<String, i32>,
}

impl Block {
    pub fn new(items: Vec<BlockItem>) -> Self {
        let symbols = HashMap::new();
        Self { items, symbols }
    }
}

pub enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}