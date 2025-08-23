use std::rc::Rc;
use crate::ast::block::*;
use crate::ast::BType;


pub struct Module {
	pub units: Vec<CompUnit>,
}
impl Module {
  pub fn build_symbols(&self) -> Symbols {
    let st = SymbolTable::new();
    for unit in &self.units {
      match unit {
        CompUnit::Func(func_def) => {
          st.borrow_mut().insert(
            func_def.id.clone(), 
            Rc::new(IdentInfo::Func(FuncInfo {
              ret_type: func_def.func_type.clone(),
              param_types: func_def.params.iter().map(|p| p.ty.clone()).collect(),
            }))
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
  pub func_type: FuncType,
  pub id: String,
  pub params: Vec<FuncFParam>,
  pub block: Block,
}
pub struct FuncInfo {
  pub ret_type: FuncType,
  pub param_types: Vec<BType>,
}

#[derive(Clone)]
pub enum FuncType {
  Int,
  Void,
}

pub struct FuncFParam {
  pub ty: BType,
  pub id: String,
}

