use std::rc::Rc;
use koopa::ir::{builder::{GlobalInstBuilder, ValueBuilder}, FunctionData, Program, Type};

use crate::ast::{block::*, decl::*, exp::*};


pub struct Module {
	pub units: Vec<CompUnit>,
}
impl Module {
  pub fn build_symbols(&self, program: &mut Program) -> Symbols {
    let st = SymbolTable::new();
    Module::decl_lib_funcs(program, &st);
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
          st.borrow_mut().declare(
            func_def.id.clone(), 
            Rc::new(IdentInfo::Func(func))
          ).unwrap();
        }
        CompUnit::Decl(decl) => {
          match decl {
            Decl::ConstDecl(const_decl) => {
              for const_def in &const_decl.defs {
                let id = &const_def.id;
                let ConstInit::ConstExp(exp) = &const_def.init;
                let val = exp.calc(&st);
                st.borrow_mut().declare(id.clone(),
                Rc::new(IdentInfo::Const(val))
              ).unwrap();
              }
            }
            Decl::VarDecl(varl_decl) => {
              for var_def in &varl_decl.defs {
                let id = &var_def.id;
                match &var_def.init {
                  None => {
                    let new_var = program.new_value().zero_init(Type::get_i32());
                    let var_val = program.new_value().global_alloc(new_var);
                    program.set_value_name(var_val, Some(format!("@{}", id)));
                    st.borrow_mut().declare(
                      id.clone(),
                      Rc::new(IdentInfo::Var(var_val)),
                    ).unwrap();
                  },
                  Some(VarInit::VarExp(exp)) => {
                    let val = program.new_value().integer(exp.calc(&st));
                    let new_var = program.new_value().global_alloc( val);
                    program.set_value_name(new_var, Some(format!("@{}", id)));
                    st.borrow_mut().declare(
                      id.clone(),
                      Rc::new(IdentInfo::Var(new_var)),
                    ).unwrap();
                  }
                }
              }
            }
          }
        }
      }
    }
    st
  }
  fn decl_lib_funcs(program: &mut Program, global_st: &Symbols) {
    // 函数声明用new方法
    let getint = program.new_func(FunctionData::new(
      "@getint".into(), vec![], Type::get_i32()));
    let getch = program.new_func(FunctionData::new(
      "@getch".into(), vec![], Type::get_i32()));
    let getarray = program.new_func(FunctionData::new(
      "@getarray".into(), 
      vec![Type::get_pointer(Type::get_i32())], 
      Type::get_i32()));
    let putint = program.new_func(FunctionData::new(
      "@putint".into(), vec![Type::get_i32()], Type::get_unit()));
    let putch = program.new_func(FunctionData::new(
      "@putch".into(), vec![Type::get_i32()], Type::get_unit()));
    let putarray = program.new_func(FunctionData::new(
      "@putarray".into(), 
      vec![Type::get_i32(), Type::get_pointer(Type::get_i32())], 
      Type::get_unit()));
    let starttime = program.new_func(FunctionData::new(
      "@starttime".into(), vec![], Type::get_unit()));
    let stoptime = program.new_func(FunctionData::new(
      "@stoptime".into(), vec![], Type::get_unit()));
    global_st.borrow_mut().declare("getint".into(), Rc::new(IdentInfo::Func(getint))).unwrap();
    global_st.borrow_mut().declare("getch".into(), Rc::new(IdentInfo::Func(getch))).unwrap();
    global_st.borrow_mut().declare("getarray".into(), Rc::new(IdentInfo::Func(getarray))).unwrap();
    global_st.borrow_mut().declare("putint".into(), Rc::new(IdentInfo::Func(putint))).unwrap();
    global_st.borrow_mut().declare("putch".into(), Rc::new(IdentInfo::Func(putch))).unwrap();
    global_st.borrow_mut().declare("putarray".into(), Rc::new(IdentInfo::Func(putarray))).unwrap();
    global_st.borrow_mut().declare("starttime".into(), Rc::new(IdentInfo::Func(starttime))).unwrap();
    global_st.borrow_mut().declare("stoptime".into(), Rc::new(IdentInfo::Func(stoptime))).unwrap();
}
}

pub enum CompUnit {
  Func(FuncDef),
  Decl(Decl),
}

pub struct FuncDef {
  pub func_type: Type,
  pub id: String,
  pub params: Vec<(String, Type)>,
  pub block: Block,
}
