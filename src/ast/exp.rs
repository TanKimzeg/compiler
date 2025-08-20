use koopa::ir::*;
use koopa::ir::builder::{LocalInstBuilder, ValueBuilder};

pub enum Exp {
    LOrExp(LOrExp),
}

pub enum LOrExp {
    Single(LAndExp),
    Binary(Box<Binary<LOrExp, LAndExp>>),
}

pub enum LAndExp {
    Single(EqExp),
    Binary(Box<Binary<LAndExp, EqExp>>),
}


pub enum EqExp {
    Single(RelExp),
    Binary(Box<Binary<EqExp, RelExp>>),
}

pub enum RelExp {
    Single(AddExp),
    Binary(Box<Binary<RelExp, AddExp>>),
}

pub enum AddExp {
    Single(MulExp),
    Binary(Box<Binary<AddExp, MulExp>>),
}

pub enum BinOp {
    Add, Sub,
    Mul, Div, Mod,
    LT, GT, LE, GE,
    Eq, NEq,
    LAnd, LOr,
}

pub enum MulExp {
    Single(UnaryExp),
    Binary(Box<Binary<MulExp, UnaryExp>>),
}

pub struct Binary<T, S> {
    pub lhs: Box<T>,
    pub op: BinOp,
    pub rhs: Box<S>,
}


pub enum UnaryExp {
    PExp(PrimaryExp),
    OpExp(UnaryOP, Box<UnaryExp>),
}

pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
    LVal(String),
}

pub enum UnaryOP {
    Plus,
    Minus,
    Not,
}

// ----------------------- trait Compile -----------------------
use std::collections::HashMap;
type SymbolTable = HashMap<String, i32>;
pub trait Compile {
    /// 将表达式编译入函数的块中, 并返回生成的值, 为语句执行下一步
    fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value;
}

impl Compile for UnaryExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value {
    match self {
      UnaryExp::OpExp(op, exp) => {
        let val = exp.compile(bb, func_data, st);
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
      UnaryExp::PExp(pexp) => pexp.compile(bb, func_data, st),
    }
  }
}

impl Compile for PrimaryExp {
   fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value {
       match self {
           PrimaryExp::Exp(exp) => exp.compile(bb, func_data, st),
           PrimaryExp::Number(num) => {
               // can not name constant value, so don't `push_key_back()`
               func_data.dfg_mut().new_value().integer(*num)
           },
           PrimaryExp::LVal(id) => {
            if let Some(&val) = st.get(id) {
                func_data.dfg_mut().new_value().integer(val)
            } else {
                panic!("Symbol '{}' not found in symbol table", id);
            }
           }
       }
   } 
}

impl Compile for Exp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value {
    match self {
      Exp::LOrExp(lor_exp) => lor_exp.compile(bb, func_data, st),
    }
  }
}

impl Compile for LOrExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value {
      match self {
        LOrExp::Single(land_exp) => land_exp.compile(bb, func_data, st),
        LOrExp::Binary(binary) => {
          binary.compile(bb, func_data,st)
        }
      }
  } 
}

impl Compile for LAndExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value {
      match self {
        LAndExp::Single(eq_exp) => eq_exp.compile(bb, func_data, st),
        LAndExp::Binary(binary) => {
          binary.compile(bb, func_data, st)
        }
      }
  }  
}

impl Compile for EqExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value {
    match self {
      EqExp::Single(rel_exp) => rel_exp.compile(bb, func_data, st),
      EqExp::Binary(binary) => {
        binary.compile(bb, func_data, st)
      }
    }
  }
}

impl Compile for RelExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value {
    match self {
      RelExp::Single(add_exp) => add_exp.compile(bb, func_data,st),
      RelExp::Binary(binary) => {
        binary.compile(bb, func_data, st)
      }
    }
  }
}

impl Compile for AddExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value {
    match self {
      AddExp::Single(mul_exp) => mul_exp.compile(bb, func_data,st),
      AddExp::Binary(binary) => {
        binary.compile(bb, func_data, st)
      }
    }
  }
}

impl Compile for MulExp {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value {
    match self {
      MulExp::Single(unary_exp) => unary_exp.compile(bb, func_data, st),
      MulExp::Binary(binary) => {
        binary.compile(bb, func_data, st)
      }
    }
  } 
}

impl<T, S> Compile for Binary<T, S> 
where T: Compile, S: Compile {
  fn compile(&self, bb: BasicBlock, func_data: &mut FunctionData, st: &SymbolTable) -> Value {
    let lhs = self.lhs.compile(bb, func_data, st);
    let rhs = self.rhs.compile(bb, func_data, st);
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


// ----------------------- trait Calc -----------------------
/// Trait for calculating the value of an expression.
pub trait Calc {
  fn calc(&self, st: &SymbolTable) -> i32;
}

impl Calc for Exp {
  fn calc(&self, st: &SymbolTable) -> i32 {
    match self {
      Exp::LOrExp(lor_exp) => lor_exp.calc(st),
    }
  }
}

impl Calc for LOrExp {
  fn calc(&self, st: &SymbolTable) -> i32 {
    match self {
      LOrExp::Single(land_exp) => land_exp.calc(st),
      LOrExp::Binary(binary) => 
        binary.calc(st)
    }
  } 
}

impl Calc for LAndExp {
  fn calc(&self, st: &SymbolTable) -> i32 {
    match self {
      LAndExp::Single(eq_exp) => eq_exp.calc(st),
      LAndExp::Binary(binary) => 
        binary.calc(st)
    }
  }
}
impl Calc for EqExp {
  fn calc(&self, st: &SymbolTable) -> i32 {
    match self {
      EqExp::Single(rel_exp) => rel_exp.calc(st),
      EqExp::Binary(binary) => 
        binary.calc(st)
    }
  }
}

impl Calc for RelExp {
  fn calc(&self, st: &SymbolTable) -> i32 {
    match self {
      RelExp::Single(add_exp) => add_exp.calc(st),
      RelExp::Binary(binary) => 
        binary.calc(st)
    }
  }
}

impl Calc for AddExp {
  fn calc(&self, st: &SymbolTable) -> i32 {
    match self {
      AddExp::Single(mul_exp) => mul_exp.calc(st),
      AddExp::Binary(binary) => 
        binary.calc(st)
    }
  } 
}

impl Calc for MulExp {
  fn calc(&self, st: &SymbolTable) -> i32 {
    match self {
      MulExp::Single(unary_exp) => unary_exp.calc(st),
      MulExp::Binary(binary) => 
        binary.calc(st)
    }
  } 
}

impl Calc for UnaryExp {
  fn calc(&self, st: &SymbolTable) -> i32 {
    match self {
      UnaryExp::PExp(pexp) => pexp.calc(st),
      UnaryExp::OpExp(op, exp) => {
        let val = exp.calc(st);
        match op {
          UnaryOP::Plus => val,
          UnaryOP::Minus => -val,
          UnaryOP::Not => (val == 0) as i32,
        }
      }
    }
  }
}

impl Calc for PrimaryExp {
  fn calc(&self, st: &SymbolTable) -> i32 {
    match self {
      PrimaryExp::Exp(exp) => exp.calc(st),
      PrimaryExp::Number(num) => *num,
      PrimaryExp::LVal(id) => {
        if let Some(&val) = st.get(id) {
            val
        } else {
            panic!("Symbol '{}' not found in symbol table", id);
        }
      }
    }
  }
}

impl<T, S> Calc for Binary<T, S>
where T: Calc, S: Calc {
  fn calc(&self, st: &SymbolTable) -> i32 {
      let lhs = self.lhs.calc(st);
      let rhs = self.rhs.calc(st);
      match self.op {
        BinOp::Add => lhs + rhs,
        BinOp::Sub => lhs - rhs,
        BinOp::Mul => lhs * rhs,
        BinOp::Div => lhs / rhs,
        BinOp::Mod => lhs % rhs,
        BinOp::LT => (lhs < rhs) as i32,
        BinOp::GT => (lhs > rhs) as i32,
        BinOp::LE => (lhs <= rhs) as i32,
        BinOp::GE => (lhs >= rhs) as i32,
        BinOp::Eq => (lhs == rhs) as i32,
        BinOp::NEq => (lhs != rhs) as i32,
        BinOp::LAnd => (lhs != 0 && rhs != 0) as i32,
        BinOp::LOr => (lhs != 0 || rhs != 0) as i32,
      }
  }
}