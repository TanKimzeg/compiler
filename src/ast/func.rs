use std::rc::Rc;
use koopa::ir::{Program, FunctionData, Type};

use crate::ast::block::*;


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
          st.borrow_mut().insert(
            func_def.id.clone(), 
            Rc::new(IdentInfo::Func(func))
          );
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
    global_st.borrow_mut().insert("getint".into(), Rc::new(IdentInfo::Func(getint)));
    global_st.borrow_mut().insert("getch".into(), Rc::new(IdentInfo::Func(getch)));
    global_st.borrow_mut().insert("getarray".into(), Rc::new(IdentInfo::Func(getarray)));
    global_st.borrow_mut().insert("putint".into(), Rc::new(IdentInfo::Func(putint)));
    global_st.borrow_mut().insert("putch".into(), Rc::new(IdentInfo::Func(putch)));
    global_st.borrow_mut().insert("putarray".into(), Rc::new(IdentInfo::Func(putarray)));
    global_st.borrow_mut().insert("starttime".into(), Rc::new(IdentInfo::Func(starttime)));
    global_st.borrow_mut().insert("stoptime".into(), Rc::new(IdentInfo::Func(stoptime)));
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
