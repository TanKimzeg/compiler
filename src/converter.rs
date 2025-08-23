use std::rc::Rc;
use crate::ast::*;
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder};
use koopa::ir::*;

pub fn ast2ir(ast: &mut CompUnit) -> Program {
    let mut program = Program::new();

    let func = program.new_func(FunctionData::with_param_names(
        format!("@{}", ast.func_def.id),
        Vec::new(),
        Type::get_i32(),
    ));

    let func_data = program.func_mut(func);
    // entry basic block
    let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
    func_data.layout_mut().bbs_mut().extend([entry]);

    let block = &mut ast.func_def.block;
    let mut ctx = Context {
        bb: entry,
        func_data,
        symbols: Rc::clone(&block.symbols),
    };
    let ret_bb = traverse_block(block, &mut ctx);
    let ret = func_data.dfg_mut().new_value().ret(None);
    func_data.layout_mut().bb_mut(ret_bb).insts_mut().push_key_back(ret).unwrap();
    program
}

struct Context<'a> {
  bb: BasicBlock,
  func_data: &'a mut FunctionData,
  symbols: Symbols, 
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
            let new_var = c.func_data.dfg_mut().new_value().alloc(Type::get_i32());
            c.func_data.layout_mut().bb_mut(c.bb).insts_mut().push_key_back(new_var).unwrap();
            match &var_def.init {
              None => {
                c.symbols.borrow_mut().insert(
                  id.clone(),
                  Rc::new(IdentInfo::Var(VarInfo { dest: new_var })),
                );
              },
              Some(VarInit::VarExp(exp)) => {
                let val = exp.compile(&mut c.bb, c.func_data, Rc::clone(&c.symbols));
                let store = c.func_data.dfg_mut().new_value().store(val, new_var);
                c.func_data.layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
                c.symbols.borrow_mut().insert(
                  id.clone(),
                  Rc::new(IdentInfo::Var(VarInfo { dest: new_var })),
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
        Some(exp) => Some(exp.compile(&mut c.bb, c.func_data, Rc::clone(&c.symbols))),
        None => None,
      };
      let ret = c.func_data.dfg_mut().new_value().ret(ret_val);
      c.func_data.layout_mut().bb_mut(c.bb).insts_mut().push_key_back(ret).unwrap();
      // unreachable block after return
      c.bb = c.func_data.dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      c.func_data.layout_mut().bbs_mut().push_key_back(c.bb).unwrap();
    },
    Stmt::LVal(id, exp) => {
      let val = exp.compile(&mut c.bb, c.func_data, Rc::clone(&c.symbols));
      if let Some(IdentInfo::Var(var_info)) = c.symbols.borrow().get(id).as_deref() {
        let store = c.func_data.dfg_mut().new_value().store(val, var_info.dest);
        c.func_data.layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
      } else {
        panic!("Variable '{}' not found in symbol table", id);
      }
    },
    Stmt::Block(inner_block) => {
      // 子块建立父指针并创建子符号表上下文
      inner_block.symbols.with_parent(Rc::clone(&c.symbols));
      let mut child_ctx = Context {
        bb: c.bb,
        func_data: c.func_data,
        symbols: Rc::clone(&inner_block.symbols),
      };
      c.bb = traverse_block(inner_block, &mut child_ctx);
    },
    Stmt::Exp(exp) => {
      if let Some(e) = exp {
        let _ = e.compile(&mut c.bb, c.func_data, Rc::clone(&c.symbols));
      }
    },
    Stmt::Cond(cond, then_b, else_b) => {
      let cond_val = cond.compile(&mut c.bb, c.func_data, Rc::clone(&c.symbols));
      let then_bb = c.func_data.dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      let end_bb = c.func_data.dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
      c.func_data.layout_mut().bbs_mut().extend([then_bb, end_bb]);

      // then 分支
      {
        let mut then_ctx = Context { bb: then_bb, func_data: c.func_data, symbols: Rc::clone(&c.symbols) };
        let then_end_bb = process_stmt(then_b, &mut then_ctx);
        let jump = c.func_data.dfg_mut().new_value().jump(end_bb);
        c.func_data.layout_mut().bb_mut(then_end_bb).insts_mut().push_key_back(jump).unwrap();
      }
      // else 分支（如有）
      if let Some(else_b) = else_b {
        let else_bb = c.func_data.dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
        c.func_data.layout_mut().bbs_mut().extend([else_bb]);
        let br = c.func_data.dfg_mut().new_value().branch(cond_val, then_bb, else_bb);
        c.func_data.layout_mut().bb_mut(c.bb).insts_mut().push_key_back(br).unwrap();

        let mut else_ctx = Context { bb: else_bb, func_data: c.func_data, symbols: Rc::clone(&c.symbols) };
        let else_end_bb = process_stmt(else_b, &mut else_ctx);
        let jump = c.func_data.dfg_mut().new_value().jump(end_bb);
        c.func_data.layout_mut().bb_mut(else_end_bb).insts_mut().push_key_back(jump).unwrap();
      } else {
        let br = c.func_data.dfg_mut().new_value().branch(cond_val, then_bb, end_bb);
        c.func_data.layout_mut().bb_mut(c.bb).insts_mut().push_key_back(br).unwrap();

      }
      c.bb = end_bb;
    },
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