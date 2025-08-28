use core::panic;
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
                match const_def {
                  ConstDef::ConstExp(id, exp) => {
                    if let ConstInit::ConstExp(exp) = exp {
                      let val = exp.calc(&st);
                      st.borrow_mut().declare(id.clone(),
                      Rc::new(IdentInfo::Const(val))
                      ).unwrap();
                    } else {
                      panic!("Constant {} initialized with array, expected expression", id);
                    }
                  }
                  ConstDef::ConstArray(array) => {
                    if let ConstInit::ConstArray(inits) = &array.init {
                    let inits = <Array<ConstInit> as ArrayInit<ConstInit>>::flat_fill_global(&array.dims, inits, &st).unwrap();
                    let dims = array.dims.iter().map(|e| e.calc(&st)).collect::<Vec<i32>>();
                    let const_array = <Array<ConstInit> as ArrayInit<ConstInit>>::aggregate(inits, &dims, program);
                    let array_val = program.new_value().global_alloc(const_array);
                    program.set_value_name(array_val, Some(format!("@{}", array.id)));
                    st.borrow_mut().declare(array.id.clone(),
                    Rc::new(IdentInfo::ConstArray(array_val))
                    ).unwrap();
                    } else {
                      panic!("Constant array {} initialized with expression, expected array", array.id);
                    }
                  }
                }
              }
            }
            Decl::VarDecl(varl_decl) => {
              for var_def in &varl_decl.defs {
                match var_def {
                  VarDef::Var(id, init) => {
                    match init {
                      None => {
                        let new_var = program.new_value().zero_init(Type::get_i32());
                        let var_val = program.new_value().global_alloc(new_var);
                        program.set_value_name(var_val, Some(format!("@{}", id)));
                        st.borrow_mut().declare(
                          id.clone(),
                          Rc::new(IdentInfo::Var(var_val)),
                        ).unwrap();
                      },
                      Some(exp) => {
                        if let ValInit::Exp(exp) = exp {
                          let val = program.new_value().integer(exp.calc(&st));
                          let new_var = program.new_value().global_alloc( val);
                          program.set_value_name(new_var, Some(format!("@{}", id)));
                          st.borrow_mut().declare(
                            id.clone(),
                            Rc::new(IdentInfo::Var(new_var)),
                          ).unwrap();
                        } else {
                          panic!("Variable {} initialized with array, expected expression", id);
                        }
                      }
                    }
                  }
                  VarDef::ArrayVar(array) => {
                    match array.init {
                      None => {
                        let new_array = program.new_value().zero_init(
                          <Array<ValInit> as ArrayInit<ValInit>>::ty(array.dims.iter().map(
                            |e| e.calc(&st)
                          ).collect::<Vec<i32>>().as_slice())
                        );
                        let array_val = program.new_value().global_alloc(new_array);
                        program.set_value_name(array_val, Some(format!("@{}", array.id)));
                        st.borrow_mut().declare(
                          array.id.clone(),
                          Rc::new(IdentInfo::MutArray(array_val)),
                        ).unwrap();
                      }
                      Some(ref inits) => {
                        if let ValInit::Array(inits) = &inits {
                        let vals = <Array<ValInit> as ArrayInit<ValInit>>::flat_fill_global(&array.dims, inits, &st).unwrap();
                        let dims = array.dims.iter().map(|e| e.calc(&st)).collect::<Vec<i32>>();
                        let init_array = <Array<ValInit> as ArrayInit<ValInit>>::aggregate(vals, &dims, program);
                        let array_val = program.new_value().global_alloc(init_array);
                        program.set_value_name(array_val, Some(format!("@{}", array.id)));
                        st.borrow_mut().declare(
                          array.id.clone(),
                          Rc::new(IdentInfo::MutArray(array_val)),
                        ).unwrap();
                        } else {
                          panic!("Array variable {} initialized with expression, expected array", array.id);
                        }
                      }
                    }
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
