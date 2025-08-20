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
  let entry = func_data .dfg_mut() .new_bb() .basic_block(Some("%entry".into()));
  func_data.layout_mut().bbs_mut().extend([entry]);

  let block = &mut ast.func_def.block;
  for item in &block.items {
    match item {
      BlockItem::Stmt(stmt) => match stmt {
        Stmt::Ret(exp) => {
          let ret_val = exp.compile(entry, func_data, &block.symbols);
          let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
          func_data .layout_mut() .bb_mut(entry) .insts_mut() .push_key_back(ret)
              .unwrap();
        }
      },

      // 建立符号表
      BlockItem::Decl(decl) => match decl {
        Decl::ConstDecl(const_decl) => {
          for const_def in &const_decl.defs {
            let id = &const_def.id;
            let ConstInit::ConstExp(exp) = &const_def.init;
            block.symbols.insert(id.clone(), exp.calc(&block.symbols));
          }
        }
      },
    }
  }
  program
}
