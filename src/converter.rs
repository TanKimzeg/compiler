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
  let mut block_context = Context {
    block,
    bb: entry,
    func_data,
  };
  traverse_block(&mut block_context);
  program
}

struct Context<'a> {
  block: &'a mut Block,
  bb: BasicBlock,
  func_data: &'a mut FunctionData,
}

fn traverse_block(c: &mut Context) {
  for item in &mut c.block.items {
    match item {
      BlockItem::Stmt(stmt) => match stmt {
        Stmt::Ret(exp) => {
          let ret_val = match exp {
            Some(exp) => Some(exp.compile(c.bb, c.func_data, Rc::clone(&c.block.symbols))),
            None => None,
          };
          let ret = c.func_data.dfg_mut().new_value().ret(ret_val);
          c.func_data.layout_mut().bb_mut(c.bb).insts_mut().push_key_back(ret)
              .unwrap();
        },
        Stmt::LVal(id, exp) => {
          let val = exp.compile(c.bb, c.func_data, Rc::clone(&c.block.symbols));
          if let Some(IdentInfo::Var(var_info)) = c.block.symbols.get(id).as_deref() {
            let store = c.func_data.dfg_mut().new_value().store(val, var_info.dest);
            c.func_data.layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
          } else {
            panic!("Variable '{}' not found in symbol table", id);
          }
        },
        Stmt::Block(inner_block) => {
          // let inner_bb = c.func_data.dfg_mut().new_bb().basic_block(None);
          // c.func_data.layout_mut().bbs_mut().extend([inner_bb]);
          inner_block.symbols.with_parent(Rc::clone(&c.block.symbols));
          let mut inner_context = Context {
            block: inner_block,
            bb: c.bb,
            func_data: c.func_data,
          };
          traverse_block(&mut inner_context);
        },
        Stmt::Exp(exp) => {
          if let Some(e) = exp {
            let _ = e.compile(c.bb, c.func_data, Rc::clone(&c.block.symbols));
          }
        }
      },

      // 建立符号表
      BlockItem::Decl(decl) => match decl {
        Decl::ConstDecl(const_decl) => {
          for const_def in &const_decl.defs {
            let id = &const_def.id;
            let ConstInit::ConstExp(exp) = &const_def.init;
            c.block.symbols.insert(
              id.clone(),
              Rc::new(IdentInfo::Const(exp.calc(&c.block.symbols))),
            );
          }
        },
        Decl::VarDecl(varl_decl) => {
          for var_def in &varl_decl.defs {
            let id = &var_def.id;
            let new_var = c.func_data.dfg_mut().new_value()
                .alloc(Type::get_i32());
            c.func_data.layout_mut().bb_mut(c.bb).insts_mut().
              push_key_back(new_var).unwrap();
            match &var_def.init {
              None => {
                c.block.symbols.insert(
                  id.clone(),
                  Rc::new(IdentInfo::Var(VarInfo { dest: new_var }))
                );
              },
              Some(VarInit::VarExp(exp)) => {
                let val = exp.compile(c.bb, c.func_data, Rc::clone(&c.block.symbols));
                let store = c.func_data.dfg_mut().new_value().store(val, new_var);
                c.func_data.layout_mut().bb_mut(c.bb).insts_mut().
                  push_key_back(store).unwrap();
                c.block.symbols.insert(
                  id.clone(),
                  Rc::new(IdentInfo::Var(VarInfo { dest: new_var }))
                );
              }
            }
          }
        }
      },
    }
  }
}