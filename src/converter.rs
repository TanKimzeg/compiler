use crate::ast::*;
use koopa::ir::*;
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};

pub trait Compile {
    /// 将表达式编译入函数的块中, 并返回生成的值, 为语句执行下一步
    fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value;
}

impl Compile for UnaryExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value {
    match self {
      UnaryExp::OpExp(op, exp) => {
        let val = exp.compile(bb, func_data);
        match op {
          UnaryOP::Plus => val,
          UnaryOP::Minus => {
            let zero = func_data.dfg_mut().new_value().integer(0);
            let sub = func_data.dfg_mut().new_value().binary(BinaryOp::Sub, zero, val);
            func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(sub).unwrap();
            sub
          },
          UnaryOP::Not => {
            let zero = func_data.dfg_mut().new_value().integer(0);
            let eq = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, val, zero);
            func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(eq).unwrap();
            eq
          }
        }
      }
      UnaryExp::PExp(pexp) => pexp.compile(bb, func_data),
    }
  }
}

impl Compile for PrimaryExp {
   fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value {
       match self {
           PrimaryExp::Exp(exp) => exp.compile(bb, func_data),
           PrimaryExp::Number(num) => {
               // can not name constant value, so don't `push_key_back()`
               func_data.dfg_mut().new_value().integer(*num)
           }
       }
   } 
}

impl Compile for Exp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value {
    match self {
      Exp::Single(unary_exp) => unary_exp.compile(bb, func_data),
    }
  }
}

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

    // 编译时执行表达式, 输出结果
    // let ret_val = func_data.dfg_mut().new_value().integer(calc_exp(
    //   match &ast.func_def.block.stmt {
    //       Stmt::Ret(exp) => exp,
    //   }
    // ));
    let ret_val = match &ast.func_def.block.stmt {
      Stmt::Ret(exp) => {
        exp.compile(entry, func_data)
      }
    };
    let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
    let _ = func_data.layout_mut().bb_mut(entry).insts_mut().push_key_back(ret);
  }
  program
}

#[allow(dead_code)]
fn calc_exp(exp: &Exp) -> i32 {
  match &exp {
    Exp::Single(unary_exp) => calc_unary_exp(unary_exp),
  }
}

fn calc_unary_exp(unary_exp: &UnaryExp) -> i32 {
  match unary_exp {
    UnaryExp::PExp(pe) => calc_pexp(pe),
    UnaryExp::OpExp(op, ue) => {
      let val = calc_exp(&Exp::Single(*ue.clone()));
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