use koopa::ir::Type;

use crate::ast::exp::Exp;


pub enum Decl {
	ConstDecl(ConstDecl),
	VarDecl(VarDecl),
}

pub struct ConstDecl {
	pub ty: Type,
	pub defs: Vec<ConstDef>,
}

pub struct ConstDef {
	pub id: String,
	pub init: ConstInit,
}

pub enum ConstInit {
	ConstExp(Exp),
}

pub struct VarDecl {
	pub ty: Type,
	pub defs: Vec<VarDef>,
}

pub struct VarDef {
	pub id: String,
	pub init: Option<VarInit>,
}

pub enum VarInit {
	VarExp(Exp),
}