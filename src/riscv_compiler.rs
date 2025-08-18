use std::{collections::HashMap, fmt::Display};

use koopa::ir::{entities::ValueData, values, *};

trait CodeGen {
    fn to_riscv(&self, func_data: &FunctionData, regs: &mut RegAllocator) -> String;
}

trait ValueKindExt {
    fn to_riscv(&self, func_data: &FunctionData, prev_reg: Option<Reg>, 
                regs: &mut RegAllocator) -> (Reg, String);
}

pub fn riscv_text(program: Program) -> String {
  let mut asm_text = String::new();
  asm_text.push_str("\t.text\n\t.globl main\n");
  let mut regs = RegAllocator::new();

  for &func in program.func_layout() {
    let func_data = program.func(func);
    asm_text.push_str(format!("{}:\n", &func_data.name()[1..]).as_str());
    for (&_bb, node) in func_data.layout().bbs() {
        
      for &inst in node.insts().keys() {

        let val_data = func_data.dfg().value(inst);
        match val_data.kind() {
          // 目标代码生成本身递归, 
          // 而指令的 ID 会按照顺序存放在基本块的指令 layout 中,
          // 因此只匹配 Return 指令, 在 Return 指令中会递归展开执行
          ValueKind::Return(..) => asm_text.push_str(
            &val_data.kind().to_riscv(func_data, &mut regs)
          ),
          _ => { }
        }
      }
    }
  }
  asm_text
}

fn parse_value(val_data: &ValueData, func_data: &FunctionData) -> Option<i32> {
    match val_data.kind() {
        ValueKind::Integer(i) => Some(i.value()),
        ValueKind::Return(ret) => {
            if let Some(rv) = ret.value() {
                let rv_data = func_data.dfg().value(rv);
                parse_value(rv_data, func_data)
            } else {
                None
            }
        },
        _ => unimplemented!(),
    }
}


impl CodeGen for ValueKind {
  fn to_riscv(&self, func_data: &FunctionData, regs: &mut RegAllocator) -> String {
      let mut  asm_text = String::new();
      let (_reg, lines) = match self {
        ValueKind::Return(ret) => {
          ret.to_riscv(&func_data, None, regs)
        },
        ValueKind::Binary(bin) => {
          bin.to_riscv(&func_data, None, regs)
        }
        _ => { unreachable!("{}",format!("Unsupported expr: {:?}", self)) }
      };
      asm_text.push_str(lines.as_str());
      asm_text
  } 
}
impl ValueKindExt for values::Return {
  fn to_riscv(&self, func_data: &FunctionData, prev_reg: Option<Reg>, 
              regs: &mut RegAllocator) -> (Reg, String) {
    let mut asm_text = String::new();
    if let Some(rv) = self.value() {
      let rv_data = func_data.dfg().value(rv);
      match rv_data.kind() {
        ValueKind::Integer(i) => {
          let line = format!("\tli a0, {}\n", i.value());
          asm_text.push_str(&line);
        },
        ValueKind::Binary(bin) => {
          let (reg, lines) = bin.to_riscv(func_data, prev_reg, regs);
          asm_text.push_str(lines.as_str());
          if reg != Reg::A0 {
            asm_text.push_str(format!("\tmv a0, {}\n", reg).as_str());
            regs.free(reg);
          }
        }
        _ => unimplemented!("{}", format!("Unsupported return value type: {:?}", rv_data.kind())),
      }
    }
    asm_text.push_str("\tret\n");
    (Reg::A0, asm_text)
  }
}
impl ValueKindExt for values::Binary {
  fn to_riscv(&self, func_data: &FunctionData, prev_res: Option<Reg>, 
              regs: &mut RegAllocator) -> (Reg, String) {
    let mut asm_text = String::new();
    let op = self.op();
    let lhs_data = func_data.dfg().value(self.lhs());
    let rhs_data = func_data.dfg().value(self.rhs());
    
    let lhs_val = match lhs_data.kind() {
      ValueKind::Integer(i) => {
        match i.value() {
          0 => Reg::Zero,
          _ => {
            let reg = regs.allocate().expect("No available registers");
            asm_text.push_str(format!("\tli {}, {}\n", reg, i.value()).as_str());
            reg
          }
        }
      },
      ValueKind::Binary(bin) => {
        let (reg, lines) = bin.to_riscv(func_data, prev_res, regs);
        asm_text.push_str(lines.as_str());
        reg
      },
      _ => unimplemented!("{}", format!("Unsupported left-hand side type: {:?}", lhs_data.kind())),
    };
    let rhs_val = match rhs_data.kind() {
      ValueKind::Integer(i) => {
        match i.value() {
          0 => {
            Reg::Zero
          },
          _ => {
            let reg = regs.allocate().expect("No available registers");
            asm_text.push_str(format!("\tli {}, {}\n", reg, i.value()).as_str());
            reg
          }
        }
      },
      ValueKind::Binary(bin) => {
        let (reg, lines) = bin.to_riscv(func_data, Some(Reg::T1), regs);
        asm_text.push_str(lines.as_str());
        reg
      },
      _ => unimplemented!("{}", format!("Unsupported right-hand side type: {:?}", rhs_data.kind())),
    };
    let target_reg = prev_res.unwrap_or_else(|| regs.allocate().expect("No available registers"));
    match op {
      BinaryOp::Add => {
        asm_text.push_str(format!("\tadd {}, {}, {}\n", 
                          target_reg.to_string(), lhs_val, rhs_val).as_str());
      },
      BinaryOp::Sub => {
        asm_text.push_str(format!("\tsub {}, {}, {}\n", 
                          target_reg.to_string(), lhs_val, rhs_val).as_str());
      },
      BinaryOp::Eq => {
        asm_text.push_str(format!("\txor {}, {}, {}\n", 
                          target_reg.to_string(), lhs_val, rhs_val).as_str());
        asm_text.push_str(format!("\tseqz {}, {}\n", 
                          target_reg.to_string(), target_reg).as_str());
      },
      BinaryOp::Mul => {
        asm_text.push_str(format!("\tmul {}, {}, {}\n", 
                          target_reg.to_string(), lhs_val, rhs_val).as_str());
      },
      BinaryOp::Div => {
        asm_text.push_str(format!("\tdiv {}, {}, {}\n", 
                          target_reg.to_string(), lhs_val, rhs_val).as_str());
      },
      BinaryOp::Mod => {
        asm_text.push_str(format!("\trem {}, {}, {}\n", 
                          target_reg.to_string(), lhs_val, rhs_val).as_str());
      },
      _ => unimplemented!("{}", format!("Unsupported binary operation: {:?}", op)),

    }
    if lhs_val != Reg::Zero && lhs_val != target_reg {
      regs.free(lhs_val);
    }
    if rhs_val != Reg::Zero && rhs_val != target_reg {
      regs.free(rhs_val);
    }
    (target_reg, asm_text)
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Reg {
    A0, A1, A2, A3, A4, A5, A6, A7,
    S0, S1, S2, S3, S4, S5, S6, S7,
    T0, T1, T2, T3, T4, T5, T6, Zero,
}

impl Display for Reg {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reg_str = match self {
            Reg::A0 => "a0", Reg::A1 => "a1", Reg::A2 => "a2", Reg::A3 => "a3",
            Reg::A4 => "a4", Reg::A5 => "a5", Reg::A6 => "a6", Reg::A7 => "a7",
            Reg::S0 => "s0", Reg::S1 => "s1", Reg::S2 => "s2", Reg::S3 => "s3",
            Reg::S4 => "s4", Reg::S5 => "s5", Reg::S6 => "s6", Reg::S7 => "s7",
            Reg::T0 => "t0", Reg::T1 => "t1", Reg::T2 => "t2", Reg::T3 => "t3",
            Reg::T4 => "t4", Reg::T5 => "t5", Reg::T6 => "t6", Reg::Zero => "x0",
        };
        write!(f, "{}", reg_str)
   } 
}

impl Reg {
  pub fn from_str(s: &str) -> Self {
    match s {
      "a0" => Reg::A0, "a1" => Reg::A1, "a2" => Reg::A2, "a3" => Reg::A3,
      "a4" => Reg::A4, "a5" => Reg::A5, "a6" => Reg::A6, "a7" => Reg::A7,
      "s0" => Reg::S0, "s1" => Reg::S1, "s2" => Reg::S2, "s3" => Reg::S3,
      "s4" => Reg::S4, "s5" => Reg::S5, "s6" => Reg::S6, "s7" => Reg::S7,
      "t0" => Reg::T0, "t1" => Reg::T1, "t2" => Reg::T2, "t3" => Reg::T3,
      "t4" => Reg::T4, "t5" => Reg::T5, "t6" => Reg::T6, "x0" => Reg::Zero,
      _ => panic!("Unknown register: {}", s),
    }
  }
}

type RegsUsed = HashMap<Reg, bool>;
struct RegAllocator {
    regs_used: RegsUsed,
}
impl RegAllocator {
  pub fn new() -> Self {
    let mut regs_used = RegsUsed::new();
    for reg in [
      Reg::A0, Reg::A1, Reg::A2, Reg::A3, Reg::A4, Reg::A5, Reg::A6, Reg::A7,
      Reg::S0, Reg::S1, Reg::S2, Reg::S3, Reg::S4, Reg::S5, Reg::S6, Reg::S7,
      Reg::T0, Reg::T1, Reg::T2, Reg::T3, Reg::T4, Reg::T5, Reg::T6,
    ] {
      regs_used.insert(reg, false);
    }
    Self { regs_used }
  } 

  pub fn allocate(&mut self) -> Option<Reg> {
    for (reg, used) in &mut self.regs_used {
      if !*used {
        *used = true;
        return Some(*reg);
      }
    }
    None
  }

  pub fn free(&mut self, reg: Reg) {
    if let Some(used) = self.regs_used.get_mut(&reg) {
      *used = false;
    }
  }
}