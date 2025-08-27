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

pub enum ConstDef {
	ConstExp(String, ConstInit),
	ConstArray(String, Exp, ConstInit),
}

pub enum ConstInit {
	ConstExp(Exp),
	ConstArray(Vec<Exp>),
}

pub struct VarDecl {
	pub ty: Type,
	pub defs: Vec<VarDef>,
}

pub enum VarDef {
	Var(String, Option<ValInit>),
	ArrayVar(String, Exp, Option<ValInit>),
}

pub enum ValInit {
	Exp(Exp),
	Array(Vec<Exp>),
}