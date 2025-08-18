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
      Exp::LOrExp(lor_exp) => lor_exp.compile(bb, func_data),
    }
  }
}

impl Compile for LOrExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value {
      match self {
        LOrExp::Single(land_exp) => land_exp.compile(bb, func_data),
        LOrExp::Binary(binary) => {
          binary.compile(bb, func_data)
        }
      }
  } 
}

impl Compile for LAndExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value {
      match self {
        LAndExp::Single(eq_exp) => eq_exp.compile(bb, func_data),
        LAndExp::Binary(binary) => {
          binary.compile(bb, func_data)
        }
      }
  }  
}

impl Compile for EqExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value {
    match self {
      EqExp::Single(rel_exp) => rel_exp.compile(bb, func_data),
      EqExp::Binary(binary) => {
        binary.compile(bb, func_data)
      }
    }
  }
}

impl Compile for RelExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value {
    match self {
      RelExp::Single(add_exp) => add_exp.compile(bb, func_data),
      RelExp::Binary(binary) => {
        binary.compile(bb, func_data)
      }
    }
  }
}

impl Compile for AddExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value {
    match self {
      AddExp::Single(mul_exp) => mul_exp.compile(bb, func_data),
      AddExp::Binary(binary) => {
        binary.compile(bb, func_data)
      }
    }
  }
}

impl Compile for MulExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value {
    match self {
      MulExp::Single(unary_exp) => unary_exp.compile(bb, func_data),
      MulExp::Binary(binary) => {
        binary.compile(bb, func_data)
      }
    }
  } 
}

impl<T, S> Compile for Binary<T, S> 
where T: Compile, S: Compile {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData) -> Value {
    let lhs = self.lhs.compile(bb, func_data);
    let rhs = self.rhs.compile(bb, func_data);
    match self.op {
      // 未执行短路操作
      BinOp::LOr => {
        let zero = func_data.dfg_mut().new_value().integer(0);
        let l0 = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, lhs, zero);
        func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(l0).unwrap();
        let r0 = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, rhs, zero);
        func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(r0).unwrap();
        let not_or = func_data.dfg_mut().new_value().binary(BinaryOp::And, l0, r0);
        func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(not_or).unwrap();
        let or = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, not_or, zero);
        func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(or).unwrap();
        return or
      },
      BinOp::LAnd => {
        let zero = func_data.dfg_mut().new_value().integer(0);
        let l0 = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, lhs, zero);
        func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(l0).unwrap();
        let r0 = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, rhs, zero);
        func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(r0).unwrap();
        let not_and = func_data.dfg_mut().new_value().binary(BinaryOp::Or, l0, r0);
        func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(not_and).unwrap();
        let and  = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, not_and, zero);
        func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(and).unwrap();
        return and
      },
      _ => {}
    }
    let op = match self.op {
      BinOp::Add => BinaryOp::Add,
      BinOp::Sub => BinaryOp::Sub,
      BinOp::Mul => BinaryOp::Mul,
      BinOp::Div => BinaryOp::Div,
      BinOp::Mod => BinaryOp::Mod,
      BinOp::LT => BinaryOp::Lt,
      BinOp::GT => BinaryOp::Gt,
      BinOp::LE => BinaryOp::Le,
      BinOp::GE => BinaryOp::Ge,
      BinOp::Eq => BinaryOp::Eq,
      BinOp::NEq => BinaryOp::NotEq,
      _ => unreachable!("Logical operators should be handled separately"),
    };
    let bin_op = func_data.dfg_mut().new_value().binary(op, lhs, rhs);
    func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(bin_op).unwrap();
    bin_op
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
  match exp {
    Exp::LOrExp(lor_exp) => calc_lor_exp(lor_exp),
  }
}

fn calc_lor_exp(lor_exp: &LOrExp) -> i32 {
  match lor_exp {
    LOrExp::Single(land_exp) => calc_land_exp(land_exp),
    LOrExp::Binary(binary) => {
      calc_binary(binary)
    }
  }
}

fn calc_land_exp(land_exp: &LAndExp) -> i32 {
  match land_exp {
    LAndExp::Single(eq_exp) => calc_eq_exp(eq_exp),
    LAndExp::Binary(binary) => {
      calc_binary(binary)
    }
  }
}

fn calc_eq_exp(eq_exp: &EqExp) -> i32 {
  match eq_exp {
    EqExp::Single(rel_exp) => calc_rel_exp(rel_exp),
    EqExp::Binary(binary) => {
      calc_binary(binary)
    }
  }
}

fn calc_rel_exp(rel_exp: &RelExp) -> i32 {
  match rel_exp {
    RelExp::Single(add_exp) => calc_add_exp(add_exp),
    RelExp::Binary(binary) => {
      calc_binary(binary)
    }
  }
}

fn calc_add_exp(add_exp: &AddExp) -> i32 {
  match add_exp {
    AddExp::Single(mul_exp) => calc_mul_exp(mul_exp),
    AddExp::Binary(binary) => {
      calc_binary(binary)
    }
  }
}

fn calc_mul_exp(mul_exp: &MulExp) -> i32 {
  match mul_exp {
    MulExp::Single(unary_exp) => calc_unary_exp(unary_exp),
    MulExp::Binary(binary) => {
      calc_binary(binary)
    }
  }
}

fn calc_binary<T, S>(binary: &Binary<T, S>) -> i32
where
    T: Compile, S: Compile,
{
    unimplemented!("Binary expression evaluation is not implemented yet");
    // let lhs = calc_exp(&binary.lhs);
    // let rhs = calc_exp(&binary.rhs);
    // match binary.op {
    //   BinOp::Add => lhs + rhs,
    //   BinOp::Sub => lhs - rhs,
    //   BinOp::Mul => lhs * rhs,
    //   BinOp::Div => lhs / rhs,
    //   BinOp::Mod => lhs % rhs,
    //   BinOp::LT => (lhs < rhs) as i32,
    //   BinOp::GT => (lhs > rhs) as i32,
    //   BinOp::LE => (lhs <= rhs) as i32,
    //   BinOp::GE => (lhs >= rhs) as i32,
    //   BinOp::Eq => (lhs == rhs) as i32,
    //   BinOp::NEq => (lhs != rhs) as i32,
    //   BinOp::And => (lhs != 0 && rhs != 0) as i32,
    //   BinOp::Or => (lhs != 0 || rhs != 0) as i32,
    // }
}

fn calc_unary_exp(unary_exp: &UnaryExp) -> i32 {
  match unary_exp {
    UnaryExp::PExp(pe) => calc_pexp(pe),
    UnaryExp::OpExp(op, ue) => {
      let val = calc_unary_exp(ue);
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