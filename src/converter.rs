use core::panic;
use std::rc::Rc;
use crate::ast::*;
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use koopa::ir::*;

pub fn ast2ir(ast: &mut Module) -> Program {
  let mut program = Program::new();
  let global_st = ast.build_symbols(&mut program);

  for unit in &mut ast.units {
    match unit {
      CompUnit::Func(func_def) => {
        traverse_func(&mut program, func_def, &global_st);
      }
      _ => { },
    }
  }
  program
}

fn traverse_func(program: &mut Program, func_def: &mut FuncDef, global_st: &Symbols) {
  let info = global_st.borrow_mut().get(func_def.id.as_str())
  .expect(format!("Function '{}' not found in symbol table", func_def.id).as_str());
  let IdentInfo::Func(func) = info.as_ref() else {
    panic!("Identifier '{}' is not a function", func_def.id);
  };

  // entry basic block
    let func_data = program.func_mut(*func);
    let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
    func_data.layout_mut().bbs_mut().extend([entry]);
    let params = func_data.params().to_vec();
  let block = &mut func_def.block;
  block.symbols.with_parent(Rc::clone(global_st));
  assert_eq!(params.len(), func_def.params.len());
  for (i, arg) in params.iter().enumerate() {
    let slot = program.func_mut(*func).dfg_mut().new_value().alloc(func_def.params[i].1.clone());
    program.func_mut(*func).layout_mut().bb_mut(entry).insts_mut().push_key_back(slot).unwrap();
    let store = program.func_mut(*func).dfg_mut().new_value().store(*arg, slot);
    program.func_mut(*func).layout_mut().bb_mut(entry).insts_mut().push_key_back(store).unwrap();
    block.symbols.borrow_mut().declare(
      func_def.params[i].0.clone(), 
      Rc::new(IdentInfo::Var(slot)),
    ).unwrap();
  }
  let mut ctx = Context {
    bb: entry,
    program: program,
    curr_func: *func,
    symbols: Rc::clone(&block.symbols),
    while_ctx: Vec::new(),
  };
  let ret_bb = traverse_block(block, &mut ctx);
  let ret = program.func_mut(*func).dfg_mut().new_value().ret(None);
  program.func_mut(*func).layout_mut().bb_mut(ret_bb).insts_mut().push_key_back(ret).unwrap();
}


fn traverse_block(block: &mut Block, c: &mut Context) -> BasicBlock {
  for item in &mut block.items {
    match item {
      BlockItem::Stmt(stmt) => {c.bb = process_stmt(stmt, c);},

      // 建立符号表
      BlockItem::Decl(decl) => match decl {
        Decl::ConstDecl(const_decl) => {
          for const_def in &const_decl.defs {
            match const_def {
              ConstDef::ConstExp(id, exp) => {
                if let ConstInit::ConstExp(exp) = exp {
                  let val = exp.calc(&c.symbols);
                  c.symbols.borrow_mut().declare(
                    id.clone(), 
                    Rc::new(IdentInfo::Const(val))
                  ).unwrap();
                } else {
                  panic!("Constant {} initialized with array, expected expression", id);
                }
              }
              ConstDef::ConstArray(id, dim, exps) => {
                if let ConstInit::ConstArray(exps) = exps {
                  let d = dim.calc(&c.symbols) as usize;
                  assert!(exps.len() <= d, "Initializer has more elements than array size");
                  let mut init_vals: Vec<Value> = exps.iter().map(|e| e.compile(c)).collect();
                  init_vals.extend(vec![
                    c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(0); d - exps.len()]);
                  let const_array = c.program.func_mut(c.curr_func).dfg_mut().new_value().alloc(
                    Type::get_array(Type::get_i32(), d)
                  );
                  c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(const_array).unwrap();
                  for (i, val) in init_vals.iter().enumerate() {
                    let idx_val = c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(i as i32);
                    let gep = c.program.func_mut(c.curr_func).dfg_mut().new_value().get_elem_ptr(const_array, idx_val);
                    c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(gep).unwrap();
                    let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(*val, gep);
                    c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
                  }
                  c.symbols.borrow_mut().declare(
                    id.clone(), 
                    Rc::new(IdentInfo::ConstArray(const_array))
                  ).unwrap();
                } else {
                  panic!("Constant {} initialized with expression, expected array", id);
                }
              }
            }
          }
        },
        Decl::VarDecl(varl_decl) => {
          for var_def in &varl_decl.defs {
            match var_def {
              VarDef::Var(id, init) => {
                match init {
                  None => {
                    let var_val = c.program.func_mut(c.curr_func).dfg_mut().new_value().alloc(varl_decl.ty.clone());
                    c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(var_val).unwrap();
                    c.symbols.borrow_mut().declare(
                      id.clone(),
                      Rc::new(IdentInfo::Var(var_val)),
                    ).unwrap();
                  },
                  Some(exp) => {
                    if let ValInit::Exp(exp) = exp {
                      let val = exp.compile(c);
                      let new_var = c.program.func_mut(c.curr_func).dfg_mut().new_value().alloc(varl_decl.ty.clone());
                      c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(new_var).unwrap();
                      let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(val, new_var);
                      c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
                      c.symbols.borrow_mut().declare(
                        id.clone(),
                        Rc::new(IdentInfo::Var(new_var)),
                      ).unwrap();
                    } else {
                      panic!("Variable {} initialized with array, expected expression", id);
                    }
                  }
                }
              }
              VarDef::ArrayVar(id, dim, init) => {
                match init {
                  None => {
                    let new_array = c.program.func_mut(c.curr_func).dfg_mut().new_value().alloc(
                      Type::get_array(Type::get_i32(), dim.calc(&c.symbols) as usize)
                    );
                    c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(new_array).unwrap();
                    c.symbols.borrow_mut().declare(
                      id.clone(),
                      Rc::new(IdentInfo::MutArray(new_array)),
                    ).unwrap();
                  }
                  Some(exps) => {
                    if let ValInit::Array(exps) = exps {
                      let d = dim.calc(&c.symbols) as usize;
                      assert!(exps.len() <= d, "Initializer has more elements than array size");
                      let array_val = c.program.func_mut(c.curr_func).dfg_mut().new_value().alloc(
                        Type::get_array(Type::get_i32(), d)
                      );
                      c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(array_val).unwrap();
                      c.symbols.borrow_mut().declare(
                        id.clone(),
                        Rc::new(IdentInfo::MutArray(array_val)),
                      ).unwrap();
                      let mut vals: Vec<Value> = exps.iter().map(|e| e.compile(c)).collect();
                      // 如果初始化列表元素少于数组维度，剩余部分补0
                      vals.extend(vec![
                        c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(0); d - exps.len()]);
                      // 逐元素存储
                      for (i, val) in vals.iter().enumerate() {
                        let idx_val = c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(i as i32);
                        let gep = c.program.func_mut(c.curr_func).dfg_mut().new_value().get_elem_ptr(array_val, idx_val);
                        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(gep).unwrap();
                        let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(*val, gep);
                        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
                      }
                    } else {
                      panic!("Array variable {} initialized with expression, expected array", id);
                    }
                  }
                }
              }
            }
          }
        }
      },
    }
  }
  c.bb
}

fn process_stmt(stmt: &mut Stmt, c: &mut Context) -> BasicBlock {
  match stmt {
    Stmt::Ret(exp) => {
      let ret_val = match exp {
        Some(exp) => Some(exp.compile(c)),
        None => None,
      };
      let ret = c.program.func_mut(c.curr_func).dfg_mut().new_value().ret(ret_val);
      c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(ret).unwrap();
      // unreachable block after return
      c.bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      c.program.func_mut(c.curr_func).layout_mut().bbs_mut().push_key_back(c.bb).unwrap();
    },
    Stmt::Assign(lval, exp) => {
      let val = exp.compile(c);
      match lval {
        LVal::Ident(id) => {
          let symbols_ref = c.symbols.borrow().get(id);
          let info = symbols_ref.as_deref().unwrap();
          match info {
            IdentInfo::Const(_) => panic!("Cannot assign to constant '{}'", id),
            IdentInfo::Func(_) => panic!("'{}' is a function", id),
            IdentInfo::ConstArray(_) => panic!("'{}' is a const array", id),
            IdentInfo::MutArray(_) => panic!("'{}' is a mutable array", id),
            IdentInfo::Var(var) => {
              let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(val, *var);
              c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
            }
          }
        }
        LVal::ArrayElem(id, idx_exp) => {
          let symbols_ref = c.symbols.borrow().get(id);
          let info = symbols_ref.as_deref().unwrap();
          match info {
            IdentInfo::Const(_) => panic!("'{}' is a constant", id),
            IdentInfo::Func(_) => panic!("'{}' is a function", id),
            IdentInfo::ConstArray(_) => panic!("'{}' is a const array", id),
            IdentInfo::Var(_) => { unreachable!("'{}' is a variable", id); }
            IdentInfo::MutArray(arr) => {
              let idx_val = idx_exp.compile(c);
              let gep = c.program.func_mut(c.curr_func).dfg_mut().new_value().get_elem_ptr(*arr, idx_val);
              c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(gep).unwrap();
              let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(val, gep);
              c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
            }
          }
        }
      }
    },
    Stmt::Block(inner_block) => {
      // 子块建立父指针并创建子符号表上下文
      inner_block.symbols.with_parent(Rc::clone(&c.symbols));
      let mut child_ctx = Context {
        bb: c.bb,
        program: c.program,
        curr_func: c.curr_func,
        symbols: Rc::clone(&inner_block.symbols),
        while_ctx: c.while_ctx.clone(),
      };
      c.bb = traverse_block(inner_block, &mut child_ctx);
    },
    Stmt::Exp(exp) => {
      if let Some(e) = exp {
        let _ = e.compile(c);
      }
    },
    Stmt::Cond(cond, then_b, else_b) => {
      let cond_val = cond.compile(c);
      let then_bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      let end_bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      c.program.func_mut(c.curr_func).layout_mut().bbs_mut().extend([then_bb, end_bb]);

      // then 分支
      {
        let mut then_ctx = Context { 
          bb: then_bb, 
          program: c.program,
          curr_func: c.curr_func,
          symbols: Rc::clone(&c.symbols),
          while_ctx: c.while_ctx.clone()
        };
        let then_end_bb = process_stmt(then_b, &mut then_ctx);
        let jump = c.program.func_mut(c.curr_func).dfg_mut().new_value().jump(end_bb);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(then_end_bb).insts_mut().push_key_back(jump).unwrap();
      }
      // else 分支（如有）
      if let Some(else_b) = else_b {
        let else_bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
        c.program.func_mut(c.curr_func).layout_mut().bbs_mut().extend([else_bb]);
        let br = c.program.func_mut(c.curr_func).dfg_mut().new_value().branch(cond_val, then_bb, else_bb);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(br).unwrap();

        let mut else_ctx = Context { 
          bb: else_bb, 
          program: c.program,
          curr_func: c.curr_func,
          symbols: Rc::clone(&c.symbols),
          while_ctx: c.while_ctx.clone()
        };
        let else_end_bb = process_stmt(else_b, &mut else_ctx);
        let jump = c.program.func_mut(c.curr_func).dfg_mut().new_value().jump(end_bb);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(else_end_bb).insts_mut().push_key_back(jump).unwrap();
      } else {
        let br = c.program.func_mut(c.curr_func).dfg_mut().new_value().branch(cond_val, then_bb, end_bb);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(br).unwrap();

      }
      c.bb = end_bb;
    },
    Stmt::While(cond, stmt) => {
      let while_entry_bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      let while_body_bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      let while_end_bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      c.while_ctx.push(WhileContext { entry: while_entry_bb, end: while_end_bb });
      c.program.func_mut(c.curr_func).layout_mut().bbs_mut().extend([while_entry_bb, while_body_bb, while_end_bb]);
      let jump = c.program.func_mut(c.curr_func).dfg_mut().new_value().jump(while_entry_bb);
      c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(jump).unwrap();

      // while entry bb
      {
        c.bb = while_entry_bb;
        // compile 方法会修改传入的 BasicBlock, 不能污染真正的 while 入口
        let cond_val = cond.compile(c);
        let br = c.program.func_mut(c.curr_func).dfg_mut().new_value().branch(cond_val, while_body_bb, while_end_bb);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(br).unwrap();
      }

      // while body bb
      {
        let mut while_ctx = Context { 
          bb: while_body_bb, 
          program: c.program,
          curr_func: c.curr_func,
          symbols: Rc::clone(&c.symbols),
          while_ctx: c.while_ctx.clone()
        };
        let body_end_bb = process_stmt(stmt, &mut while_ctx);
        let jump = c.program.func_mut(c.curr_func).dfg_mut().new_value().jump(while_entry_bb);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(body_end_bb).insts_mut().push_key_back(jump).unwrap();
      }
      c.while_ctx.pop();
      c.bb = while_end_bb;
    },
    Stmt::Break => {
      let while_ctx = c.while_ctx.last().expect("break not in while");
      let jump = c.program.func_mut(c.curr_func).dfg_mut().new_value().jump(while_ctx.end);
      c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(jump).unwrap();
      // unreachable block after break
      c.bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      c.program.func_mut(c.curr_func).layout_mut().bbs_mut().push_key_back(c.bb).unwrap();
    },
    Stmt::Continue => {
      let while_ctx = c.while_ctx.last().expect("continue not in while");
      let jump = c.program.func_mut(c.curr_func).dfg_mut().new_value().jump(while_ctx.entry);
      c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(jump).unwrap();
      // unreachable block after continue
      c.bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      c.program.func_mut(c.curr_func).layout_mut().bbs_mut().push_key_back(c.bb).unwrap();
    }
  }
  c.bb
}
pub fn generate_bb_name() -> String {
  static mut BB_COUNT: u32 = 0;
  unsafe {
    BB_COUNT += 1;
    format!("%bb{}", BB_COUNT)
  }
}