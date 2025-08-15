use crate::ast::CompUnit;
use koopa::ir::*;
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};

/// 第一章的 AST 很简单, 我只需要生成这样的 Koopa IR:
/// fun @main(): i32 {
/// %entry:
///   ret 0;
/// }
pub fn ast2ir(ast: &CompUnit) -> Program {
  let mut program = Program::new();

  let func = program.new_func(FunctionData::with_param_names(
    format!("@{}", ast.func_def.id), 
    Vec::new(), 
    Type::get_i32(),
  ));

  let func_data = program.func_mut(func);
  {
    // entry basic block
    let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
    func_data.layout_mut().bbs_mut().extend([entry]);

    // insert a return statement with value 0
    let ret_val = func_data.dfg_mut().new_value().integer(ast.func_def.block.stmt.num);
    let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
    let _ = func_data.layout_mut().bb_mut(entry).insts_mut().push_key_back(ret);
  }
  program
}