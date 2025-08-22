use std::{collections::HashMap, fmt::Display};

use koopa::ir::{ values, *};

struct Context<'a> {
  pub func_data: &'a FunctionData,
  pub offset: HashMap<Value, u32>,
  pub stack_size: u32,
  pub inst: Option<Value>,
}

impl<'a> Context<'a> {
  pub fn new(func_data: &'a FunctionData) -> Self {
    let (stack_size, offset) = Context::stack_alloc(func_data);
    Self {
      func_data,
      offset: offset,
      stack_size,
      inst: None,
    }
  }
  fn stack_alloc(func_data: &FunctionData) -> (u32, HashMap<Value, u32>) {
    let mut size = 0;
    let mut offset = HashMap::new();
    for (&_bb, node) in func_data.layout().bbs() {
      for &inst in node.insts().keys() {
        let val_data = func_data.dfg().value(inst);
        let ty = val_data.ty();
        if !ty.is_unit() {
          let type_kind = ty.kind();
          match type_kind {
            TypeKind::Int32 => { offset.insert(inst, size); size += 4;},
            TypeKind::Pointer(_) => { offset.insert(inst, size);size += 4; },
            _ => unimplemented!("Unsupported type for stack allocation: {}", type_kind.to_string()),
          }
        }
      }
    }
    ((size + 15) / 16 * 16, offset)
  }
  pub fn get_offset(&self, val: Value) -> u32 {
    *self.offset.get(&val).expect(
      format!("Value {:?} not found in offset map", val).as_str()
    )
  }
  /// `addi sp, sp, 立即数`要求立即数的范围是$[-2048,2047]$,
  /// 对于超过这个范围的立即数,我将其存放在`t0`寄存器中,
  /// 然后使用`sub sp, sp, t0`来完成栈指令的调整.
  /// 如果立即数在范围内,则直接使用`addi sp, sp, 立即数`指令.
  /// 在执行 `prologue` 时,函数未开始执行,所以`t0`寄存器没有被占用.
  fn prologue(&self) -> String{
    let mut asm_text = String::new();
    let stack_size = self.stack_size;
    if stack_size > 2048 {
      asm_text.push_str(format!("\tli t0, {}\n", stack_size).as_str());
      asm_text.push_str("\tsub sp, sp, t0\n");
    } else {
      asm_text.push_str(format!("\taddi sp, sp, -{}\n", stack_size).as_str());
    }
    asm_text
  }

  /// `addi sp, sp, 立即数`要求立即数的范围是$[-2048,2047]$,
  /// 对于超过这个范围的立即数,我将其存放在`t0`寄存器中,
  /// 然后使用`add sp, sp, t0`来完成栈指令的调整.
  /// 如果立即数在范围内,则直接使用`addi sp, sp, 立即数`指令.
  /// 在执行 `epilogue` 时,函数已经执行完毕,所以`t0`寄存器没有被占用.
  fn epilogue(&self) -> String {
    let mut asm_text = String::new();
    let stack_size = self.stack_size;
    if stack_size > 2047 {
      asm_text.push_str(format!("\tli t0, {}\n", stack_size).as_str());
      asm_text.push_str("\tadd sp, sp, t0\n");
    } else {
      asm_text.push_str(format!("\taddi sp, sp, {}\n", stack_size).as_str());
    }
    asm_text.push_str("\tret\n");
    asm_text
  }

}

trait CodeGen {
    fn to_riscv(&self, context: &mut Context) -> String;
}

/// 执行完一种 `ValueKind` 的指令后, 返回存放最终结果的寄存器和生成的汇编代码
trait ValueKindExt {
    fn to_riscv(&self, context: &mut Context) -> String;
}

pub fn riscv_text(program: Program) -> String {
  let mut asm_text = String::new();
  asm_text.push_str("\t.text\n\t.globl main\n");

  for &func in program.func_layout() {
    let func_data = program.func(func);
    asm_text.push_str(format!("{}:\n", &func_data.name()[1..]).as_str());
    let mut context = Context::new(func_data);
    // 函数的 prologue
    asm_text.push_str(context.prologue().as_str());
    for (&bb, _node) in func_data.layout().bbs() {
      asm_text.push_str(bb.to_riscv(&mut context).as_str());
    }
  }
  asm_text
}
impl CodeGen for BasicBlock {
  fn to_riscv(&self, context: &mut Context) -> String {
    let mut asm_text = String::new();
    let node = context.func_data.layout().bbs()
      .node(self)
      .expect(format!("BasicBlock {:?} not found in layout", self).as_str());
    asm_text.push_str(format!("{}:\n", context.func_data.dfg()
      .bb(*self)
      .name().as_deref()
      .expect(format!("BasicBlock {:?} has no name", self).as_str())
      .strip_prefix('%').unwrap()
    ).as_str());
    for &inst in node.insts().keys() {
      let val_data = context.func_data.dfg().value(inst);
      let val_kind = val_data.kind();
      context.inst = Some(inst);
      asm_text.push_str(val_kind.to_riscv(context).as_str());
    }
    asm_text
  }
}

impl ValueKindExt for ValueKind {
  fn to_riscv(&self, context: &mut Context) -> String {
      let mut  asm_text = String::new();
      let lines = match self {
        ValueKind::Return(ret) => {
          ret.to_riscv(context)
        },
        ValueKind::Binary(bin) => {
          bin.to_riscv(context)
        },
        ValueKind::Alloc(..) => { String::new() }, // 分配内存不需要生成代码
        ValueKind::Load(load) => {
          load.to_riscv(context)
        },
        ValueKind::Store(store) => {
          store.to_riscv(context)
        },
        ValueKind::Branch(br) => {
          br.to_riscv(context)
        },
        ValueKind::Jump(j) => {
          j.to_riscv(context)
        }
        _ => { unimplemented!("{}",format!("Unsupported expr: {:?}", self)) }
      }; 
      asm_text.push_str(lines.as_str());
      asm_text
  } 
}
impl ValueKindExt for values::Return {
  fn to_riscv(&self, context: &mut Context) -> String {
    let mut asm_text = String::new();
    match self.value() {
    Some(rv) => {
      let rv_data = context.func_data.dfg().value(rv);
      match rv_data.kind() {
        ValueKind::Integer(i) => {
          asm_text.push_str(format!("\tli {}, {}\n", Reg::A0.to_string(), i.value()).as_str());
        },
        _ => {
          asm_text.push_str(format!("\tlw {}, {}(sp)\n", Reg::A0.to_string(), context.get_offset(rv)).as_str());
        }
      }
    },
    None => {
      asm_text.push_str(format!("\tli {}, {}\n", Reg::A0.to_string(), 0).as_str());
    }
    }
    // 函数的 epilogue
    asm_text.push_str(context.epilogue().as_str());
    asm_text
  }
}
impl ValueKindExt for values::Binary {
  /// 我约定: lhs存放在`t1`寄存器中, rhs存放在`t2`寄存器中,
  /// 最终结果存放在`t0`寄存器中.
  fn to_riscv(&self, context: &mut Context) -> String {
    let mut asm_text = String::new();
    let op = self.op();
    let lhs_data = context.func_data.dfg().value(self.lhs());
    let rhs_data = context.func_data.dfg().value(self.rhs());
    let target_reg = Reg::T0;
    let lhs_reg = Reg::T1;
    let rhs_reg = Reg::T2;
    match lhs_data.kind() {
      ValueKind::Integer(i) => {
        asm_text.push_str(format!("\tli {}, {}\n", 
        lhs_reg.to_string(), i.value()).as_str());
      },
      _ => {
        asm_text.push_str(format!("\tlw {}, {}(sp)\n", 
        lhs_reg.to_string(), context.get_offset(self.lhs())).as_str());
      }
    }
    match rhs_data.kind() {
      ValueKind::Integer(i) => {
        asm_text.push_str(format!("\tli {}, {}\n", 
        rhs_reg.to_string(), i.value()).as_str());
      },
      _ => {
        asm_text.push_str(format!("\tlw {}, {}(sp)\n", 
        rhs_reg.to_string(), context.get_offset(self.rhs())).as_str());
      }
    }
    match op {
      BinaryOp::Add => {
        asm_text.push_str(format!("\tadd {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
      },
      BinaryOp::Sub => {
        asm_text.push_str(format!("\tsub {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
      },
      BinaryOp::Eq => {
        asm_text.push_str(format!("\txor {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
        asm_text.push_str(format!("\tseqz {}, {}\n", 
                          target_reg.to_string(), target_reg).as_str());
      },
      BinaryOp::Mul => {
        asm_text.push_str(format!("\tmul {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
      },
      BinaryOp::Div => {
        asm_text.push_str(format!("\tdiv {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
      },
      BinaryOp::Mod => {
        asm_text.push_str(format!("\trem {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
      },
      BinaryOp::Or => {
        asm_text.push_str(format!("\tor {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
      },
      BinaryOp::And => {
        asm_text.push_str(format!("\tand {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
      },
      BinaryOp::NotEq => {
        asm_text.push_str(format!("\txor {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
        asm_text.push_str(format!("\tsnez {}, {}\n", 
                          target_reg.to_string(), target_reg).as_str());
      },
      BinaryOp::Lt => {
        asm_text.push_str(format!("\tslt {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
      },
      BinaryOp::Le => {
        asm_text.push_str(format!("\tsgt {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
        asm_text.push_str(format!("\tseqz {}, {}\n", 
                          target_reg.to_string(), target_reg).as_str());
      },
      BinaryOp::Gt => {
        asm_text.push_str(format!("\tsgt {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
      },
      BinaryOp::Ge => {
        asm_text.push_str(format!("\tslt {}, {}, {}\n", 
                          target_reg.to_string(), lhs_reg, rhs_reg).as_str());
        asm_text.push_str(format!("\tseqz {}, {}\n", 
                          target_reg.to_string(), target_reg).as_str());
      },
      _ => unimplemented!("{}", format!("Unsupported binary operation: {:?}", op)),

    }
    asm_text.push_str(format!("\tsw {}, {}(sp)\n", 
                          target_reg.to_string(), context.get_offset(context.inst.unwrap())).as_str());
    asm_text
  }
}
impl ValueKindExt for values::Integer {
  /// 将整数值存放在 `t0`寄存器中.
  fn to_riscv(&self, _context: &mut Context) -> String {
    let mut asm_text = String::new();
    match self.value() {
      0 => {
        asm_text.push_str(format!("\tli {}, {}\n", Reg::T0.to_string(), 0).as_str());
      },
      _ => {
        asm_text.push_str(format!("\tli {}, {}\n", Reg::T0.to_string(), self.value()).as_str());
      }
    }
    asm_text
}
}
impl ValueKindExt for values::Load {
  fn to_riscv(&self, context: &mut Context) -> String {
    let mut asm_text = String::new();
    asm_text.push_str(format!("\tlw {}, {}(sp)\n", Reg::T0.to_string(), 
                      context.get_offset(self.src())).as_str());
    asm_text.push_str(format!("\tsw {}, {}(sp)\n", Reg::T0.to_string(), 
                      context.get_offset(context.inst.unwrap())).as_str());
    asm_text
  }
}
impl ValueKindExt for values::Store {
  fn to_riscv(&self, context: &mut Context) -> String {
    let mut asm_text = String::new();
    let val_data = context.func_data.dfg().value(self.value());
    match val_data.kind() {
      ValueKind::Integer(i) => {
        asm_text.push_str(format!("\tli {}, {}\n", Reg::T0.to_string(), i.value()).as_str());
      },
      _ => {
        asm_text.push_str(format!("\tlw {}, {}(sp)\n", Reg::T0.to_string(), 
                              context.get_offset(self.value())).as_str());
      }
    }
    asm_text.push_str(format!("\tsw {}, {}(sp)\n", Reg::T0.to_string(), 
                          context.get_offset(self.dest())).as_str());
    asm_text
  }    
}
impl ValueKindExt for values::Branch {
  fn to_riscv(&self, context: &mut Context) -> String {
    let mut asm_text = String::new();
    let cond_data = context.func_data.dfg().value(self.cond());
    match cond_data.kind() {
      ValueKind::Integer(i) => {
        asm_text.push_str(format!("\tli {}, {}\n", Reg::T0.to_string(), i.value()).as_str());
      },
      _ => {
        asm_text.push_str(format!("\tlw {}, {}(sp)\n", Reg::T0.to_string(), 
                              context.get_offset(self.cond())).as_str());
      }
    }
    let then_bb_name = context.func_data.dfg().bb(self.true_bb())
                      .name().as_deref()
                      .expect(format!("BasicBlock {:?} has no name", self.true_bb()).as_str())
                      .strip_prefix('%').unwrap();

    asm_text.push_str(format!("\tbnez {}, {}\n", Reg::T0.to_string(), then_bb_name).as_str());
    let else_bb_name = context.func_data.dfg().bb(self.false_bb())
                      .name().as_deref()
                      .expect(format!("BasicBlock {:?} has no name", self.false_bb()).as_str())
                      .strip_prefix('%').unwrap();
    asm_text.push_str(format!("\tj {}\n", else_bb_name).as_str());
    
    // asm_text.push_str(self.true_bb().to_riscv(context).as_str());
    // asm_text.push_str(self.false_bb().to_riscv(context).as_str());
    asm_text
  }
}
impl ValueKindExt for values::Jump {
  fn to_riscv(&self, context: &mut Context) -> String {
    let mut asm_text = String::new();
    let target_bb_name = context.func_data.dfg().bb(self.target())
                      .name().as_deref()
                      .expect(format!("BasicBlock {:?} has no name", self.target()).as_str())
                      .strip_prefix('%').unwrap();
    asm_text.push_str(format!("\tj {}\n", target_bb_name).as_str());
    // asm_text.push_str(self.target().to_riscv(context).as_str());
    asm_text
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

#[allow(dead_code)]
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
#[allow(dead_code)]
struct RegAllocator {
    regs_used: RegsUsed,
}
#[allow(dead_code)]
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