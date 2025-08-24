use std::rc::Rc;
use crate::ast::*;
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder};
use koopa::ir::*;

pub fn ast2ir(ast: &mut Module) -> Program {
  let mut program = Program::new();
  let global_st = ast.build_symbols(&mut program);

  for unit in &mut ast.units {
    match unit {
      CompUnit::Func(func_def) => {
        traverse_func(&mut program, func_def, &global_st);
      }
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
    block.symbols.borrow_mut().insert(
      func_def.params[i].0.clone(), 
      Rc::new(IdentInfo::Var(slot)),
    );
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
            let id = &const_def.id;
            let ConstInit::ConstExp(exp) = &const_def.init;
            let val = exp.calc(&c.symbols);
            c.symbols.borrow_mut().insert(id.clone(), Rc::new(IdentInfo::Const(val)));
          }
        },
        Decl::VarDecl(varl_decl) => {
          for var_def in &varl_decl.defs {
            let id = &var_def.id;
            let new_var = c.program.func_mut(c.curr_func).dfg_mut().new_value().alloc(Type::get_i32());
            c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(new_var).unwrap();
            match &var_def.init {
              None => {
                c.symbols.borrow_mut().insert(
                  id.clone(),
                  Rc::new(IdentInfo::Var(new_var )),
                );
              },
              Some(VarInit::VarExp(exp)) => {
                let val = exp.compile(c);
                let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(val, new_var);
                c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
                c.symbols.borrow_mut().insert(
                  id.clone(),
                  Rc::new(IdentInfo::Var(new_var)),
                );
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
    Stmt::LVal(id, exp) => {
      let val = exp.compile(c);
      if let Some(IdentInfo::Var(var)) = c.symbols.borrow().get(id).as_deref() {
        let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(val, *var);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
      } else {
        panic!("Variable '{}' not found in symbol table", id);
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
        let while_entry_bb = while_entry_bb;
        // compile 方法会修改传入的 BasicBlock, 不能污染真正的 while 入口
        let cond_val = cond.compile(c);
        let br = c.program.func_mut(c.curr_func).dfg_mut().new_value().branch(cond_val, while_body_bb, while_end_bb);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(while_entry_bb).insts_mut().push_key_back(br).unwrap();
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