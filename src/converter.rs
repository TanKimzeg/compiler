use crate::ast::*;
use koopa::ir::*;
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};

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
    let ret_val = func_data.dfg_mut().new_value().integer(calc_exp(&ast.func_def.block.stmt.exp));
    let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
    let _ = func_data.layout_mut().bb_mut(entry).insts_mut().push_key_back(ret);
  }
  program
}

fn calc_exp(exp: &Exp) -> i32 {
  match &exp.unary_exp {
    UnaryExp::PExp(pe) => calc_pexp(pe),
    UnaryExp::OpExp(op, ue) => {
      let val = calc_exp(&Exp { unary_exp: (*ue.clone()).clone() });
      match op {
        UnaryOP::Plus => val,
        UnaryOP::Minus => -val,
        UnaryOP::Not => (val == 0) as i32,
      }
    }
  }
}

fn calc_pexp(pexp: &PrimaryExp) -> i32 {
  match pexp {
    PrimaryExp::Exp(exp) => calc_exp(exp),
    PrimaryExp::Number(num) => *num,
  }
}