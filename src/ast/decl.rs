use crate::ast::exp::Exp;


pub enum Decl {
	ConstDecl(ConstDecl),
	VarDecl(VarDecl),
}

pub struct ConstDecl {
	pub ty: BType,
	pub defs: Vec<ConstDef>,
}

#[derive(Clone)]
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

pub struct VarDecl {
	pub ty: BType,
	pub defs: Vec<VarDef>,
}

pub struct VarDef {
	pub id: String,
	pub init: Option<VarInit>,
}

pub enum VarInit {
	VarExp(Exp),
}