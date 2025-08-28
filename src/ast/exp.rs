use core::panic;

use koopa::ir::*;
use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use crate::ast::{Symbols, SymbolsExt};
use crate::ast::IdentInfo;


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
    FuncCall(String, Vec<Exp>),
}

pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
    LVal(LVal)
}

pub struct LVal {
  pub id: String,
  pub indices: Vec<Exp>,
}

pub enum UnaryOP {
    Plus,
    Minus,
    Not,
}

// ----------------------- trait Compile -----------------------
pub struct Context<'a> {
  pub bb: BasicBlock,
  pub program: &'a mut Program,
  pub curr_func: Function,
  pub symbols: Symbols, 
  pub while_ctx: Vec<WhileContext>,
}
#[derive(Clone)]
pub struct WhileContext {
  pub entry: BasicBlock,
  pub end: BasicBlock,
}
pub trait Compile {
    /// 将表达式编译入函数的块中, 并返回生成的值, 为语句执行下一步
    fn compile(&self, c: &mut Context) -> Value;
}

impl Compile for UnaryExp {
  fn compile(&self, c: &mut Context) -> Value {
    match self {
      UnaryExp::OpExp(op, exp) => {
        let val = exp.compile(c);
        match op {
          UnaryOP::Plus => val,
          UnaryOP::Minus => {
            let zero = c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(0);
            let sub = c.program.func_mut(c.curr_func).dfg_mut().new_value().binary(BinaryOp::Sub, zero, val);
            c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(sub).unwrap();
            sub
          },
          UnaryOP::Not => {
            let zero = c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(0);
            let eq = c.program.func_mut(c.curr_func).dfg_mut().new_value().binary(BinaryOp::Eq, val, zero);
            c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(eq).unwrap();
            eq
          }
        }
      }
      UnaryExp::PExp(pexp) => pexp.compile(c),
      UnaryExp::FuncCall(id, args) => {
        let func_info = c.symbols.borrow().get(id).expect(format!("Function '{}' not found", id).as_str());
        if let IdentInfo::Func(func) = func_info.as_ref() {
          let arg_vals: Vec<Value> = args.iter().map(|arg| arg.compile(c)).collect();
          // 检查参数数量,类型检查未实现
          {
            assert_eq!(arg_vals.len(), c.program.func(*func).params().len(),
                "Function '{}' expects {} arguments, but {} were provided", id, c.program.func_mut(*func).params().len(), arg_vals.len());
            // assert_eq!(c.program.func(*func).params().iter().map(
            //   |(_, ty)| ty).collect() == arg_vals.iter().map(
            //     |v| Type::get(c.program.func(c.curr_func).dfg().value(v).kind())
            //   ).collect(),
            //   "Function '{}' argument types do not match", id);
          }
          let call = c.program.func_mut(c.curr_func).dfg_mut().new_value().call(*func, arg_vals);
          c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(call).unwrap();
          call
        } else {
          panic!("Identifier '{}' is not a function", id);
        }

      }
    }
  }
}

impl Compile for PrimaryExp {
   fn compile(&self, c: &mut Context) -> Value {
       match self {
           PrimaryExp::Exp(exp) => exp.compile(c),
           PrimaryExp::Number(num) => {
               // can not name constant value, so don't `push_key_back()`
               c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(*num)
           },
           PrimaryExp::LVal(lval) => {
            match lval.indices.len() {
              0 => {
                if let Some(info) =  c.symbols.borrow().get(&lval.id) {
                    match info.as_ref() {
                      IdentInfo::Const(val) => {
                        c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(*val)
                      }
                      IdentInfo::Var(var) => {
                        // 变量值是一个指针, 需要加载
                        let load = c.program.func_mut(c.curr_func).dfg_mut().new_value().load(*var);
                        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(load).unwrap();
                        load
                      }
                      IdentInfo::Func(_) => panic!("FuncInfo should not appear here"),
                      IdentInfo::MutArray(_) => panic!("{} is a mutable array, need index to access", lval.id),
                      IdentInfo::ConstArray(_) => panic!("{} is a constant array, need index to access", lval.id),
                    }
                } else {
                    panic!("Symbol '{}' not found in symbol table", lval.id);
                }
              }
              _ => {
                let arr = {
                  if let Some(info) =  c.symbols.borrow().get(&lval.id) {
                      match info.as_ref() {
                        IdentInfo::MutArray(arr) => *arr,
                        IdentInfo::ConstArray(arr) => *arr,
                        _ => panic!("{} is not an array", lval.id),
                      }
                  } else {
                      panic!("Symbol '{}' not found in symbol table", lval.id);
                  }
                };
                let index: Vec<Value> = lval.indices.iter().map(
                  |e| e.compile(c)
                ).collect();
                let mut gep = arr;
                for idx in index.iter() {
                  gep = c.program.func_mut(c.curr_func).dfg_mut().new_value().get_elem_ptr(gep, *idx);
                  c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(gep).unwrap();
                }
                let load =  c.program.func_mut(c.curr_func).dfg_mut().new_value().load(gep);
                c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(load).unwrap();
                load
              }
            }
          }
       }
   } 
}

impl Compile for Exp {
  fn compile(&self, c: &mut Context) -> Value {
    match self {
      Exp::LOrExp(lor_exp) => lor_exp.compile(c),
    }
  }
}

impl Compile for LOrExp {
  fn compile(&self, c: &mut Context) -> Value {
      match self {
        LOrExp::Single(land_exp) => land_exp.compile(c),
        LOrExp::Binary(binary) => {
          binary.compile(c)
        }
      }
  } 
}

impl Compile for LAndExp {
  fn compile(&self, c: &mut Context) -> Value {
      match self {
        LAndExp::Single(eq_exp) => eq_exp.compile(c),
        LAndExp::Binary(binary) => {
          binary.compile(c)
        }
      }
  }  
}

impl Compile for EqExp {
  fn compile(&self, c: &mut Context) -> Value {
    match self {
      EqExp::Single(rel_exp) => rel_exp.compile(c),
      EqExp::Binary(binary) => {
        binary.compile(c)
      }
    }
  }
}

impl Compile for RelExp {
  fn compile(&self, c: &mut Context) -> Value {
    match self {
      RelExp::Single(add_exp) => add_exp.compile(c),
      RelExp::Binary(binary) => {
        binary.compile(c)
      }
    }
  }
}

impl Compile for AddExp {
  fn compile(&self, c: &mut Context) -> Value {
    match self {
      AddExp::Single(mul_exp) => mul_exp.compile(c),
      AddExp::Binary(binary) => {
        binary.compile(c)
      }
    }
  }
}

impl Compile for MulExp {
  fn compile(&self, c: &mut Context) -> Value {
    match self {
      MulExp::Single(unary_exp) => unary_exp.compile(c),
      MulExp::Binary(binary) => {
        binary.compile(c)
      }
    }
  } 
}

impl<T, S> Compile for Binary<T, S> 
where T: Compile, S: Compile {
  fn compile(&self, c: &mut Context) -> Value {
    use crate::converter::generate_bb_name;
    let lhs = self.lhs.compile(c);
    match self.op {
      // 执行短路操作
      BinOp::LOr => {
        // result = 1
        // if (lhs == 0) {
        //  result = (rhs != 0)
        // }
        let one = c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(1);
        let result = c.program.func_mut(c.curr_func).dfg_mut().new_value().alloc(Type::get_i32());
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(result).unwrap();
        let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(one, result);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();

        let zero = c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(0);
        let l0 = c.program.func_mut(c.curr_func).dfg_mut().new_value().binary(BinaryOp::Eq, lhs, zero);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(l0).unwrap();
        let then_bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
        let end_bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
        c.program.func_mut(c.curr_func).layout_mut().bbs_mut().extend([then_bb, end_bb]);
        let br = c.program.func_mut(c.curr_func).dfg_mut().new_value().branch(l0, then_bb, end_bb);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(br).unwrap();
        // then 分支
        {
          c.bb = then_bb;
          let rhs = self.rhs.compile(c);
          let res = c.program.func_mut(c.curr_func).dfg_mut().new_value().binary(BinaryOp::NotEq, zero, rhs);
          c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(res).unwrap();
          let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(res, result);
          c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();
          
          let jump = c.program.func_mut(c.curr_func).dfg_mut().new_value().jump(end_bb);
          c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(jump).unwrap();
        }
        c.bb = end_bb;
        let result = c.program.func_mut(c.curr_func).dfg_mut().new_value().load(result);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(result).unwrap();
        return result;
        
        // let zero = func_data.dfg_mut().new_value().integer(0);
        // let l0 = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, lhs, zero);
        // func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(l0).unwrap();
        // let r0 = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, rhs, zero);
        // func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(r0).unwrap();
        // let not_or = func_data.dfg_mut().new_value().binary(BinaryOp::And, l0, r0);
        // func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(not_or).unwrap();
        // let or = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, not_or, zero);
        // func_data.layout_mut().bb_mut(bb).insts_mut().push_key_back(or).unwrap();
        // return or
      },
      BinOp::LAnd => {
        // result = 0
        // if (lhs != 0) {
        //  result = (rhs != 0)
        // }
        let zero = c.program.func_mut(c.curr_func).dfg_mut().new_value().integer(0);
        let result = c.program.func_mut(c.curr_func).dfg_mut().new_value().alloc(Type::get_i32());
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(result).unwrap();
        let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(zero, result);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(store).unwrap();

        let l0 = c.program.func_mut(c.curr_func).dfg_mut().new_value().binary(BinaryOp::NotEq, lhs, zero);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(l0).unwrap();
        let then_bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
        let end_bb = c.program.func_mut(c.curr_func).dfg_mut().new_bb().basic_block(Some(generate_bb_name()));
        c.program.func_mut(c.curr_func).layout_mut().bbs_mut().extend([then_bb, end_bb]);
        let br = c.program.func_mut(c.curr_func).dfg_mut().new_value().branch(l0, then_bb, end_bb);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(br).unwrap();
        // then 分支
        {
          c.bb = then_bb;
          let rhs = self.rhs.compile(c);
          let res = c.program.func_mut(c.curr_func).dfg_mut().new_value().binary(BinaryOp::NotEq, zero, rhs);
          c.program.func_mut(c.curr_func).layout_mut().bb_mut(then_bb).insts_mut().push_key_back(res).unwrap();
          let store = c.program.func_mut(c.curr_func).dfg_mut().new_value().store(res, result);
          c.program.func_mut(c.curr_func).layout_mut().bb_mut(then_bb).insts_mut().push_key_back(store).unwrap();
          
          let jump = c.program.func_mut(c.curr_func).dfg_mut().new_value().jump(end_bb);
          c.program.func_mut(c.curr_func).layout_mut().bb_mut(then_bb).insts_mut().push_key_back(jump).unwrap();
        }
        c.bb = end_bb;
        let result = c.program.func_mut(c.curr_func).dfg_mut().new_value().load(result);
        c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(result).unwrap();
        return result;

        // let zero = func_data.dfg_mut().new_value().integer(0);
        // let l0 = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, lhs, zero);
        // func_data.layout_mut().bb_mut(*bb).insts_mut().push_key_back(l0).unwrap();
        // let r0 = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, rhs, zero);
        // func_data.layout_mut().bb_mut(*bb).insts_mut().push_key_back(r0).unwrap();
        // let not_and = func_data.dfg_mut().new_value().binary(BinaryOp::Or, l0, r0);
        // func_data.layout_mut().bb_mut(*bb).insts_mut().push_key_back(not_and).unwrap();
        // let and  = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, not_and, zero);
        // func_data.layout_mut().bb_mut(*bb).insts_mut().push_key_back(and).unwrap();
        // return and
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
    let rhs = self.rhs.compile(c);
    let bin_op = c.program.func_mut(c.curr_func).dfg_mut().new_value().binary(op, lhs, rhs);
    c.program.func_mut(c.curr_func).layout_mut().bb_mut(c.bb).insts_mut().push_key_back(bin_op).unwrap();
    bin_op
  }
}


// ----------------------- trait Calc -----------------------
/// Trait for calculating the value of an expression.
pub trait Calc {
  fn calc(&self, st: &Symbols) -> i32;
}

impl Calc for Exp {
  fn calc(&self, st: &Symbols) -> i32 {
    match self {
      Exp::LOrExp(lor_exp) => lor_exp.calc(st),
    }
  }
}

impl Calc for LOrExp {
  fn calc(&self, st: &Symbols) -> i32 {
    match self {
      LOrExp::Single(land_exp) => land_exp.calc(st),
      LOrExp::Binary(binary) => 
        binary.calc(st)
    }
  } 
}

impl Calc for LAndExp {
  fn calc(&self, st: &Symbols) -> i32 {
    match self {
      LAndExp::Single(eq_exp) => eq_exp.calc(st),
      LAndExp::Binary(binary) => 
        binary.calc(st)
    }
  }
}
impl Calc for EqExp {
  fn calc(&self, st: &Symbols) -> i32 {
    match self {
      EqExp::Single(rel_exp) => rel_exp.calc(st),
      EqExp::Binary(binary) => 
        binary.calc(st)
    }
  }
}

impl Calc for RelExp {
  fn calc(&self, st: &Symbols) -> i32 {
    match self {
      RelExp::Single(add_exp) => add_exp.calc(st),
      RelExp::Binary(binary) => 
        binary.calc(st)
    }
  }
}

impl Calc for AddExp {
  fn calc(&self, st: &Symbols) -> i32 {
    match self {
      AddExp::Single(mul_exp) => mul_exp.calc(st),
      AddExp::Binary(binary) => 
        binary.calc(st)
    }
  } 
}

impl Calc for MulExp {
  fn calc(&self, st: &Symbols) -> i32 {
    match self {
      MulExp::Single(unary_exp) => unary_exp.calc(st),
      MulExp::Binary(binary) => 
        binary.calc(st)
    }
  } 
}

impl Calc for UnaryExp {
  fn calc(&self, st: &Symbols) -> i32 {
    match self {
      UnaryExp::PExp(pexp) => pexp.calc(st),
      UnaryExp::OpExp(op, exp) => {
        let val = exp.calc(st);
        match op {
          UnaryOP::Plus => val,
          UnaryOP::Minus => -val,
          UnaryOP::Not => (val == 0) as i32,
        }
      },
      UnaryExp::FuncCall(..) => {
        unreachable!("Function calls are not supported in constant expressions");
      }
    }
  }
}

impl Calc for PrimaryExp {
  fn calc(&self, st: &Symbols) -> i32 {
    match self {
      PrimaryExp::Exp(exp) => exp.calc(st),
      PrimaryExp::Number(num) => *num,
      PrimaryExp::LVal(lval) => {
        match lval.indices.len() {
          0 => {
            if let Some(val) = st.get(&lval.id) {
              if let IdentInfo::Const(v) = val.as_ref() {
                *v
              } else {
                panic!("Expected a constant value for identifier '{}'", lval.id);
              }
            } else {
                panic!("Symbol '{}' not found in symbol table", lval.id);
            }
          }
          _ => {
            unreachable!("Array elements are not supported in constant expressions");
          }
        }
      }
    }
  }
}

impl<T, S> Calc for Binary<T, S>
where T: Calc, S: Calc {
  fn calc(&self, st: &Symbols) -> i32 {
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