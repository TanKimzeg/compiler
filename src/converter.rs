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
        },
        Stmt::LVal(id, exp) => {
          let val = exp.compile(entry, func_data, &block.symbols);
          if let Some(IdentInfo::Var(var_info)) = block.symbols.get(id) {
            let store = func_data.dfg_mut().new_value().store(val, var_info.dest);
            func_data.layout_mut().bb_mut(entry).insts_mut().push_key_back(store).unwrap();
          } else {
            panic!("Variable '{}' not found in symbol table", id);
          }
        }
      },

      // 建立符号表
      BlockItem::Decl(decl) => match decl {
        Decl::ConstDecl(const_decl) => {
          for const_def in &const_decl.defs {
            let id = &const_def.id;
            let ConstInit::ConstExp(exp) = &const_def.init;
            block.symbols.insert(
              id.clone(),
              IdentInfo::Const(exp.calc(&block.symbols)),
            );
          }
        },
        Decl::VarDecl(varl_decl) => {
          for var_def in &varl_decl.defs {
            let id = &var_def.id;
            let new_var = func_data.dfg_mut().new_value()
                .alloc(Type::get_i32());
            func_data.layout_mut().bb_mut(entry).insts_mut().
              push_key_back(new_var).unwrap();
            match &var_def.init {
              None => {
                block.symbols.insert(
                  id.clone(),
                  IdentInfo::Var(VarInfo { dest: new_var })
                );
              },
              Some(VarInit::VarExp(exp)) => {
                let val = exp.compile(entry, func_data, &block.symbols);
                let store = func_data.dfg_mut().new_value().store(val, new_var);
                func_data.layout_mut().bb_mut(entry).insts_mut().
                  push_key_back(store).unwrap();
                block.symbols.insert(
                  id.clone(),
                  IdentInfo::Var(VarInfo { dest: new_var })
                );
              }
            }
          }
        }
      },
    }
  }
  program
}
