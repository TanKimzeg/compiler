use std::rc::Rc;
use koopa::ir::{Program, FunctionData, Type};

use crate::ast::block::*;


pub struct Module {
	pub units: Vec<CompUnit>,
}
impl Module {
  pub fn build_symbols(&self, program: &mut Program) -> Symbols {
    let st = SymbolTable::new();
    for unit in &self.units {
      match unit {
        CompUnit::Func(func_def) => {
					let func = program.new_func(FunctionData::with_param_names(
						format!("@{}", func_def.id),
						func_def.params.iter().
						map(|(id, ty)| {
							let mut id = id.clone();
							id.insert(0, '@');
							(Some(id.to_string()), ty.clone())
						}).collect(),
						func_def.func_type.clone(),
					));
          st.borrow_mut().insert(
            func_def.id.clone(), 
            Rc::new(IdentInfo::Func(func))
          );
        }
      }
    }
    st
  }
}

pub enum CompUnit {
  Func(FuncDef)
}

pub struct FuncDef {
  pub func_type: Type,
  pub id: String,
  pub params: Vec<(String, Type)>,
  pub block: Block,
}
