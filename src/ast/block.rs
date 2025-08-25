use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use koopa::ir::{ Value, Function };

use crate::ast::stmt::*;
use crate::ast::decl::*;

pub type Symbols = Rc<RefCell<SymbolTable>>;
pub trait SymbolsExt {
	fn insert(&mut self, id: String, info: Rc<IdentInfo>);
	fn get(&self, id: &str) -> Option<Rc<IdentInfo>>;
	fn with_parent(&mut self, parent: Symbols);
}
impl SymbolsExt for Symbols {
	fn insert(&mut self, id: String, info: Rc<IdentInfo>) {
		self.borrow_mut().insert(id, info);
	}

	fn get(&self, id: &str) -> Option<Rc<IdentInfo>> {
		self.borrow().get(id)
	}

	fn with_parent(&mut self, parent: Symbols) {
		self.borrow_mut().with_parent(parent);
	}
}
pub struct SymbolTable {
	locals: HashMap<String, Rc<IdentInfo>>,
	parent: Option<Symbols>,
}
impl SymbolTable {
	pub fn new() -> Symbols {
		Rc::new(RefCell::new(Self {
			locals: HashMap::new(),
			parent: None,
		}))
	}

	pub fn with_parent(&mut self, parent: Symbols) {
		self.parent = Some(Rc::clone(&parent));
	}

	fn insert(&mut self, id: String, info: Rc<IdentInfo>) {
		self.locals.insert(id, info);
	}

	pub fn declare(&mut self, id: String, info: Rc<IdentInfo>) -> Result<(), String> {
		if self.locals.contains_key(&id) {
			Err(format!("Redefinition of identifier `{}`", id))
		} else {
			self.locals.insert(id, info);
			Ok(())
		}
	}

	pub fn get(&self, id: &str) -> Option<Rc<IdentInfo>> {
		if let Some(info) = self.locals.get(id) {
			Some(Rc::clone(info))
		} else if let Some(parent) = &self.parent {
			parent.borrow().get(id)
		} else {
			None
		}
	} 
}

pub enum IdentInfo {
	Const(i32),
	Var(Value),
	Func(Function),
}

pub struct Block {
	pub items: Vec<BlockItem>,
	pub symbols: Rc<RefCell<SymbolTable>>,
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