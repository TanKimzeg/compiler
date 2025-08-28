use koopa::ir::{builder::{LocalInstBuilder, ValueBuilder}, *};

use crate::ast::{exp::Exp, Calc, Compile, Context, Symbols};


pub enum Decl {
	ConstDecl(ConstDecl),
	VarDecl(VarDecl),
}

pub struct ConstDecl {
	pub ty: Type,
	pub defs: Vec<ConstDef>,
}

pub trait ArrayInit<T> {
	fn flat_fill(dims: &[Exp], inits: &[T], c: &mut Context) -> Result<Vec<Value>, String>;
	fn flat_fill_global(dims: &[Exp], inits: &[T], st: &Symbols) -> Result<Vec<i32>, String>;
	fn ty(dims: &[i32]) -> Type {
		if dims.len() == 0 {
			Type::get_i32()
		} else {
			Type::get_array(
				Self::ty(&dims[1..]),
				dims[0] as usize
			)
		}
	}
	fn elem_ptrs(base: Value, indices: &[i32], c: &mut Context) -> Vec<Value> {
		let size:i32 = indices.iter().product();
		let mut idxs = Vec::with_capacity(size as usize);
		// 生成每个维度的索引值
		let mut elem_idx = vec![0; indices.len()];
		for _ in 0..size {
			idxs.push(elem_idx.clone());
			for i in (0..indices.len()).rev() {
				if elem_idx[i] + 1 < indices[i] {
					elem_idx[i] += 1;
					break;
				} else {
					elem_idx[i] = 0;
				}
			}
		}
		let mut gep = base;
		let mut ptrs = Vec::with_capacity(size as usize);
		for idx in idxs {
			for i in idx {
				let idx_val = c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(i);
				gep = c.program.func_mut(c.curr_func).dfg_mut().new_value().get_elem_ptr(gep, idx_val);
				c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(gep).unwrap();
			}
			ptrs.push(gep);
			gep = base;
		}
		ptrs
	}

	fn aggregate(vals: Vec<i32>, dims: &[i32], p: &mut Program) -> Value; 
}

pub struct Array<T> {
	pub id: String,
	pub dims: Vec<Exp>,
	pub init: T,
}
pub enum ConstDef {
	ConstExp(String, ConstInit),
	ConstArray(Array<ConstInit>),
}

pub enum ConstInit {
	ConstExp(Exp),
	ConstArray(Vec<ConstInit>),
}

impl ArrayInit<ConstInit> for Array<ConstInit> {
	fn flat_fill(dims: &[Exp], inits: &[ConstInit], c: &mut Context) -> Result<Vec<Value>, String> {
		let mut vals = Vec::new();
		let mut elem_cnt: usize = 0;

		for init in inits {
			match init {
				ConstInit::ConstExp(exp) => {
					vals.push(exp.compile(c));
					elem_cnt += 1;
				}
				ConstInit::ConstArray(arr) => {
					// 初始化列表必须对齐数组维度的边界
					let mut border: usize = 1;
					let mut suffix: usize = 0;
					for dim in dims.iter().skip(1).rev() {
						border *= dim.calc(&c.symbols) as usize;
						if elem_cnt % border == 0 {
							suffix += 1;
						} else {
							break;
						}
					}
					let pad: usize = dims[dims.len()-suffix..].iter().map(
						|e| e.calc(&c.symbols) as usize
					).product();
					elem_cnt += pad;
					let alligned = &dims[dims.len()-suffix..];
					vals.extend(Array::<ConstInit>::flat_fill(&alligned, &arr, c)?);
				}
			}
		}
		// 填充0
		let d: usize = dims.iter().map(
			|e| e.calc(&c.symbols) as usize)
			.product();
		if elem_cnt > d {
			Err(format!("Array constant size mismatch: declared size {}, but got {}", d, elem_cnt))
		} else {
			vals.extend(vec![
				c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(0); d - elem_cnt
			]);
			Ok(vals)
		}
	}

	fn flat_fill_global(dims: &[Exp], inits: &[ConstInit], st: &Symbols) -> Result<Vec<i32>, String> {
		let mut vals = Vec::new();
		let mut elem_cnt: usize = 0;

		for init in inits {
			match init {
				ConstInit::ConstExp(exp) => {
					vals.push(exp.calc(&st));
					elem_cnt += 1;
				}
				ConstInit::ConstArray(arr) => {
					// 初始化列表必须对齐数组维度的边界
					let mut border: usize = 1;
					let mut suffix: usize = 0;
					for dim in dims.iter().skip(1).rev() {
						border *= dim.calc(&st) as usize;
						if elem_cnt % border == 0 {
							suffix += 1;
						} else {
							break;
						}
					}
					let pad: usize = dims[dims.len()-suffix..].iter().map(
						|e| e.calc(&st) as usize
					).product();
					elem_cnt += pad;
					let alligned = &dims[dims.len()-suffix..];
					assert_ne!(alligned.len(), 0, "Array constant initializer not aligned with array dimensions");
					vals.extend(Array::<ConstInit>::flat_fill_global(&alligned, &arr, st)?);
				}
			}
		}
		// 填充0
		let d: usize = dims.iter().map(
			|e| e.calc(&st) as usize)
			.product();
		if elem_cnt > d {
			Err(format!("Array constant size mismatch: declared size {}, but got {}", d, elem_cnt))
		} else {
			vals.extend(vec![ 0; d - elem_cnt ]);
			Ok(vals)
		}
	}

	fn aggregate(vals: Vec<i32>, dims: &[i32], p: &mut Program) -> Value {
		if dims.len() == 1 {
			let mut res = Vec::new();
			for v in vals {
				res.push(p.new_value().integer(v));
			}
			p.new_value().aggregate(res)
		} else {
			let step: usize = dims[1..].iter().product::<i32>() as usize;
			let mut res = Vec::new();
			for chunk in vals.chunks(step) {
				let sub_agg = <Array<ConstInit> as ArrayInit<ConstInit>>::aggregate(chunk.to_vec(), &dims[1..], p);
				res.push(sub_agg);
			}
			p.new_value().aggregate(res)
		}
	}
}

pub struct VarDecl {
	pub ty: Type,
	pub defs: Vec<VarDef>,
}

pub enum VarDef {
	Var(String, Option<ValInit>),
	ArrayVar(Array<Option<ValInit>>),
}

pub enum ValInit {
	Exp(Exp),
	Array(Vec<ValInit>),
}

impl ArrayInit<ValInit> for Array<ValInit> {
	fn flat_fill(dims: &[Exp], inits: &[ValInit], c: &mut Context) -> Result<Vec<Value>, String> {
		let mut vals = Vec::new();
		let mut elem_cnt: usize = 0;
		for init in inits {
			match init {
				ValInit::Exp(exp) => {
					vals.push(exp.compile(c));
					elem_cnt += 1;
				}
				ValInit::Array(arr) => {
					// 初始化列表必须对齐数组维度的边界
					let mut border: usize = 1;
					let mut suffix: usize = 0;
					for dim in dims.iter().skip(1).rev() {
						border *= dim.calc(&c.symbols) as usize;
						if elem_cnt % border == 0 {
							suffix += 1;
						} else {
							break;
						}
					}
					let pad: usize = dims[dims.len()-suffix..].iter().map(
						|e| e.calc(&c.symbols) as usize
					).product();
					elem_cnt += pad;
					let alligned = &dims[dims.len()-suffix..];
					vals.extend(Array::<ValInit>::flat_fill(&alligned, &arr, c)?);
				}
			}
		}
		// 填充0
		let d: usize = dims.iter().map(
			|e| e.calc(&c.symbols) as usize)
			.product();
		if elem_cnt > d {
			Err(format!("Array variable size mismatch: declared size {}, but got {}", d, elem_cnt))
		} else {
			vals.extend(vec![
				c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(0); d - elem_cnt
			]);
			Ok(vals)
		}
	}

	fn flat_fill_global(dims: &[Exp], inits: &[ValInit], st: &Symbols) -> Result<Vec<i32>, String> {
		let mut vals = Vec::new();
		let mut elem_cnt: usize = 0;
		for init in inits {
			match init {
				ValInit::Exp(exp) => {
					vals.push(exp.calc(&st));
					elem_cnt += 1;
				}
				ValInit::Array(arr) => {
					// 初始化列表必须对齐数组维度的边界
					let mut border: usize = 1;
					let mut suffix: usize = 0;
					for dim in dims.iter().skip(1).rev() {
						border *= dim.calc(&st) as usize;
						if elem_cnt % border == 0 {
							suffix += 1;
						} else {
							break;
						}
					}
					let pad: usize = dims[dims.len()-suffix..].iter().map(
						|e| e.calc(&st) as usize
					).product();
					elem_cnt += pad;
					let alligned = &dims[dims.len()-suffix..];
					vals.extend(Array::<ValInit>::flat_fill_global(&alligned, &arr, st)?);
				}
			}
		}
		// 填充0
		let d: usize = dims.iter().map(
			|e| e.calc(&st) as usize)
			.product();
		if elem_cnt > d {
			Err(format!("Array variable size mismatch: declared size {}, but got {}", d, elem_cnt))
		} else {
			vals.extend(vec![ 0; d - elem_cnt ]);
			Ok(vals)
		}
	}

	fn aggregate(vals: Vec<i32>, dims: &[i32], p: &mut Program) -> Value {
			if dims.len() == 1 {
			let mut res = Vec::new();
			for v in vals {
				res.push(p.new_value().integer(v));
			}
			p.new_value().aggregate(res)
		} else {
			let step: usize = dims[1..].iter().product::<i32>() as usize;
			let mut res = Vec::new();
			for chunk in vals.chunks(step) {
				let sub_agg = <Array<ValInit> as ArrayInit<ValInit>>::aggregate(chunk.to_vec(), &dims[1..], p);
				res.push(sub_agg);
			}
			p.new_value().aggregate(res)
		}		
	}
}