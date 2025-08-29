use std::{cmp::max, collections::HashMap, fmt::Display};

use koopa::ir::{ values, *};

struct Context<'a> {
  pub program: &'a Program,
  pub curr_func: Function,
  pub offset: HashMap<Value, Position>,
  pub stack_size: usize,
  pub need_ra: bool,
  pub inst: Option<Value>,
}
#[derive(Clone)]
enum Position {
  Reg(Reg),
  Stack(usize),
}

impl<'a> Context<'a> {
  pub fn new(program: &'a Program, curr_func: Function) -> Self {
    let (stack_size, need_ra, offset) = 
    Context::stack_alloc(program.func(curr_func));
    Self {
      program,
      curr_func,
      offset: offset,
      stack_size,
      need_ra,
      inst: None,
    }
  }
  fn stack_alloc(func_data: &FunctionData) -> (usize, bool, HashMap<Value, Position>) {
    let mut s: usize = 0;
    let mut r: usize = 0;
    let mut a: usize = 0;
    let mut offset = HashMap::new();
    for (&_bb, node) in func_data.layout().bbs() {
      for &inst in node.insts().keys() {
        let val_data = func_data.dfg().value(inst);
        let val_kind = val_data.kind();
        if let ValueKind::Call(callee) = val_kind {
          r = 4;
          a = max(a as i32, (callee.args().len() as i32 - 8)*4) as usize;
        }
      }
    }
    s += a; // 调用超过8个参数的函数,为其参数预留空间.本函数的局部变量在此之上
    Type::set_ptr_size(4);
    for (&_bb, node) in func_data.layout().bbs() {
      for &inst in node.insts().keys() {
        let ty = func_data.dfg().value(inst).ty();
        // offset.insert(inst, ty.size());
        if !ty.is_unit() {
          let type_kind = ty.kind();
          match type_kind {
            TypeKind::Int32 => { 
              offset.insert(inst, Position::Stack(s)); 
              s += 4;
            },
            TypeKind::Pointer(_) => { 
              offset.insert(inst, Position::Stack(s)); 
              s += 4; 
            },
            _ => unimplemented!("Unsupported type for stack allocation: {}", type_kind.to_string()),
          }
        }
      }
    }
    s = (s + r + 15) /16 * 16; // 16字节对齐
    for (i, &arg) in func_data.params().iter().enumerate() {
      if i > 7 {
        // 超过8个参数,在调用者的栈上，所以超出了本函数的栈帧
        offset.insert(arg, Position::Stack(s+(i-8)*4));
      } else {
        offset.insert(arg, Position::Reg(Reg::from_str(format!("a{}", i).as_str())));
      }
    }
    for (&_bb, node) in func_data.layout().bbs() {
      for &inst in node.insts().keys() {
        let val_data = func_data.dfg().value(inst);
        let val_kind = val_data.kind();
        if let ValueKind::Call(call) = val_kind {
          let mut a_ = 0;
          for (i, &arg) in call.args().iter().enumerate() {
            if i > 7 {
              // 超过8个参数,在调用者的栈上，所以超出了本函数的栈帧
              offset.insert(arg, Position::Stack(a_));
              a_ += 4;
            } else {
              offset.insert(arg, Position::Reg(Reg::from_str(format!("a{}", i).as_str())));
            }
          }
          assert!(a_ <= a, "Argument stack space overflow");
        }
      }
    }
    (s, r != 0, offset)
  }
  pub fn get_offset(&self, val: Value) -> Position {
    self.offset.get(&val).expect(
      format!("Value {:?} not found in offset map", val).as_str()
    ).clone()
  }
  /// `addi sp, sp, 立即数`要求立即数的范围是$[-2048,2047]$,
  /// 对于超过这个范围的立即数,我将其存放在`t0`寄存器中,
  /// 然后使用`sub sp, sp, t0`来完成栈指令的调整.
  /// 如果立即数在范围内,则直接使用`addi sp, sp, 立即数`指令.
  /// 在执行 `prologue` 时,函数未开始执行,所以`t0`寄存器没有被占用.
  fn prologue(&self) -> String{
    let mut asm_text = String::new();
    let stack_size = self.stack_size;
    if self.need_ra {
      asm_text.push_str("\tsw ra, -4(sp)\n");
    }
    asm_text.push_str(&Reg::add_sp(-(stack_size as i32), Reg::SP));
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
    asm_text.push_str(&Reg::add_sp(stack_size as i32, Reg::SP));
    if self.need_ra {
      asm_text.push_str("\tlw ra, -4(sp)\n");
    }
    asm_text.push_str("\tret\n");
    asm_text
  }

  fn local_bb_label(&self, bb: &BasicBlock) -> String {
    let fn_name = self.program.func(self.curr_func).name().strip_prefix('@').unwrap();
    let bb_name = self.program.func(self.curr_func).dfg()
          .bb(*bb)
          .name().as_deref()
          .expect(format!("BasicBlock {:?} has no name", bb).as_str())
          .strip_prefix('%').unwrap();
    format!(".L{}_{}", fn_name, bb_name)
  }
}

trait CodeGen {
    fn to_riscv(&self, c: &mut Context) -> String;
}

/// 执行完一种 `ValueKind` 的指令后, 返回存放最终结果的寄存器和生成的汇编代码
trait ValueKindExt {
    fn to_riscv(&self, c: &mut Context) -> String;
}

pub fn riscv_text(program: Program) -> String {
  let mut asm_text = String::new();
  asm_text.push_str("\t.data\n");
  for &inst in program.inst_layout() {
    if inst.is_global() {
      let global_val_data = program.borrow_value(inst);
      if let ValueKind::GlobalAlloc(global_alloc) = global_val_data.kind() {
        let global_name = global_val_data.name().as_ref().unwrap()
          .strip_prefix('@').unwrap();
        let init_val = global_alloc.init();
        asm_text.push_str(&format!("\t.globl {}\n", global_name));
        asm_text.push_str(&format!("{}:\n", global_name));
        fn global_to_riscv(value_kind: &ValueKind, init_size: usize,p: &Program) -> String {
          let mut asm_text = String::new();
          match value_kind {
            ValueKind::Integer(i) => {
              asm_text.push_str(&format!("\t.word {}\n", i.value()));
            }
            ValueKind::ZeroInit(_) => {
              asm_text.push_str(&format!("\t.zero {}\n", init_size));
            }
            ValueKind::Aggregate(agg) => {
              for &elem in agg.elems().iter() {
                let elem_data = p.borrow_value(elem);
                let elem_kind = elem_data.kind();
                asm_text.push_str(&global_to_riscv(elem_kind, init_size,p));
              }
            }
            _ => unimplemented!("Unsupported global init value: {:?}", value_kind),
          }
          asm_text
        }
        asm_text.push_str(&global_to_riscv(
          &program.borrow_value(init_val).kind(), 
          program.borrow_value(init_val).ty().size(), 
          &program
        ));
        
      } else {
        unreachable!("Global value is not GlobalAlloc: {:?}", global_val_data.kind());
      }
    }
    }
  for &func in program.func_layout() {
    let func_data = program.func(func);
    if func_data.layout().bbs().is_empty() {
      continue;
    }
    asm_text.push_str(format!("\t.text\n\t.globl {}\n", &func_data.name()[1..]).as_str());
    asm_text.push_str(format!("{}:\n", &func_data.name()[1..]).as_str());
    let mut c = Context::new(&program, func);
    // 函数的 prologue
    asm_text.push_str(c.prologue().as_str());
    for (&bb, _node) in func_data.layout().bbs() {
      asm_text.push_str(bb.to_riscv(&mut c).as_str());
    }
  }
  asm_text
}
impl CodeGen for BasicBlock {
  fn to_riscv(&self, c: &mut Context) -> String {
    let mut asm_text = String::new();
    let node = c.program.func(c.curr_func).layout().bbs()
      .node(self)
      .expect(format!("BasicBlock {:?} not found in layout", self).as_str());
    asm_text.push_str(format!("{}:\n", c.local_bb_label(self)).as_str());
    for &inst in node.insts().keys() {
      let val_data = c.program.func(c.curr_func).dfg().value(inst);
      let val_kind = val_data.kind();
      c.inst = Some(inst);
      asm_text.push_str(val_kind.to_riscv(c).as_str());
    }
    asm_text
  }
}

impl ValueKindExt for ValueKind {
  fn to_riscv(&self, c: &mut Context) -> String {
      let mut asm_text = String::new();
      let lines = match self {
        ValueKind::Return(ret) => ret.to_riscv(c),
        ValueKind::Binary(bin) => bin.to_riscv(c),
        ValueKind::Alloc(..) => String::new(), // 分配内存不需要生成代码
        ValueKind::Load(load) => load.to_riscv(c),
        ValueKind::Store(store) =>  store.to_riscv(c),
        ValueKind::Branch(br) => br.to_riscv(c),
        ValueKind::Jump(j) => j.to_riscv(c),
        ValueKind::Call(call) => call.to_riscv(c),
        ValueKind::GetElemPtr(gep) => gep.to_riscv(c),
        _ => { unimplemented!("{}",format!("Unsupported expr: {:?}", self)) }
      }; 
      asm_text.push_str(lines.as_str());
      asm_text
  } 
}
impl ValueKindExt for values::Return {
  fn to_riscv(&self, c: &mut Context) -> String {
    let mut asm_text = String::new();
    match self.value() {
    Some(rv) => {
      asm_text.push_str(&Reg::load_val2reg(c, rv, Reg::A0));
    },
    None => { }
    }
    // 函数的 epilogue
    asm_text.push_str(c.epilogue().as_str());
    asm_text
  }
}
impl ValueKindExt for values::Binary {
  /// 我约定: lhs存放在`t1`寄存器中, rhs存放在`t2`寄存器中,
  /// 最终结果存放在`t0`寄存器中.
  fn to_riscv(&self, c: &mut Context) -> String {
    let mut asm_text = String::new();
    let op = self.op();
    let target_reg = Reg::T0;
    let lhs_reg = Reg::T1;
    let rhs_reg = Reg::T2;
    asm_text.push_str(&Reg::load_val2reg(c, self.lhs(), lhs_reg));
    asm_text.push_str(&Reg::load_val2reg(c, self.rhs(), rhs_reg));
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
    asm_text.push_str(&Reg::store_reg2stack(c, target_reg, c.inst.unwrap()));
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
  fn to_riscv(&self, c: &mut Context) -> String {
    let mut asm_text = String::new();
    let dest_kind = c.program.func(c.curr_func).dfg().value(self.src()).kind();
    match dest_kind {
      ValueKind::GetElemPtr(_) => {
        asm_text.push_str(&Reg::arr_load2reg(c, self.src(), Reg::T0));
      }
      _ => {
        asm_text.push_str(&Reg::load_val2reg(c, self.src(), Reg::T0));
      }
    }
    asm_text.push_str(&Reg::store_reg2stack(c, Reg::T0, c.inst.unwrap()));
    asm_text
  }
}
impl ValueKindExt for values::Store {
  fn to_riscv(&self, c: &mut Context) -> String {
    let mut asm_text = String::new();
    asm_text.push_str(&Reg::load_val2reg(c, self.value(), Reg::T0));
    let dest_kind = c.program.func(c.curr_func).dfg().value(self.dest()).kind();
    match dest_kind {
      ValueKind::GetElemPtr(_) => {
        asm_text.push_str(&Reg::arr_store2stack(c, Reg::T0, self.dest()));
      }
      _ => {
        asm_text.push_str(&Reg::store_reg2stack(c, Reg::T0, self.dest()));
      }
    }
    asm_text
  }
}
impl ValueKindExt for values::Branch {
  fn to_riscv(&self, c: &mut Context) -> String {
    let mut asm_text = String::new();
    asm_text.push_str(&Reg::load_val2reg(c, self.cond(), Reg::T0));
    let then_bb_name = c.local_bb_label(&self.true_bb());
    asm_text.push_str(format!("\tbnez {}, {}\n", Reg::T0.to_string(), then_bb_name).as_str());
    let else_bb_name = c.local_bb_label(&self.false_bb());
    asm_text.push_str(format!("\tj {}\n", else_bb_name).as_str());
    asm_text
  }
}
impl ValueKindExt for values::Jump {
  fn to_riscv(&self, c: &mut Context) -> String {
    let mut asm_text = String::new();
    let target_bb_name = c.local_bb_label(&self.target());
    asm_text.push_str(format!("\tj {}\n", target_bb_name).as_str());
    asm_text
  }
}
impl ValueKindExt for values::Call {
  fn to_riscv(&self, c: &mut Context) -> String {
    let mut asm_text = String::new();
    let func = self.callee();
    let args = self.args();
    for (i, arg) in args.iter().enumerate() {
      if i > 7 {
        asm_text.push_str(&Reg::load_val2reg(c, *arg, Reg::T0));
        asm_text.push_str(&Reg::store_reg2stack(c, Reg::T0, *arg));
      } else {
        asm_text.push_str(&Reg::load_val2reg(c, *arg, Reg::from_str(&format!("a{}", i))));
      }
    }
    asm_text.push_str(format!("\tcall {}\n", c.program.func(func).name()
    .strip_prefix('@').unwrap()).as_str());
    let func_type = c.program.func(func).ty().kind();
    match func_type {
      TypeKind::Function(_, ret_ty) => {
        if !ret_ty.is_unit() {
          asm_text.push_str(&Reg::store_reg2stack(c, Reg::A0, c.inst.unwrap()));
        }
      }
      _ => unreachable!("Callee is not a function, but {}", func_type.to_string()),
    }
    asm_text
  }
}
impl ValueKindExt for values::GetElemPtr {
  fn to_riscv(&self, c: &mut Context) -> String {
    let mut asm_text = String::new();
    asm_text.push_str(&Reg::load_arr2reg(c, self.src(), Reg::T0)); // 数组首地址
    asm_text.push_str(&Reg::load_val2reg(c, self.index(), Reg::T1));
    if self.src().is_global() {
      let elem_size = {
        let val_data = c.program.borrow_value(self.src());
        match val_data.ty().kind() {
          TypeKind::Array(t, _) => t.size(),
          TypeKind::Pointer(ptr) => {
            match ptr.kind() {
              TypeKind::Array(t, _) => t.size(),
              _ => unreachable!("GetElemPtr source is pointer but not to array: {}", ptr.kind()),
            }
          }
          _ => panic!("GetElemPtr source is not array or pointer: {}", val_data.ty().kind()),
        }
      };
    asm_text.push_str(&format!("\tli t2, {}\n", 
      elem_size
    ));
    } else {
      let elem_size = {
        let val_data = c.program.func(c.curr_func).dfg().value(self.src());
        match val_data.ty().kind() {
          TypeKind::Array(t, _) => t.size(),
          TypeKind::Pointer(ptr) => {
            match ptr.kind() {
              TypeKind::Array(t, _) => t.size(),
              _ => unreachable!("GetElemPtr source is pointer but not to array: {}", ptr.kind()),
            }
          }
          _ => panic!("GetElemPtr source is not array or pointer: {}", val_data.ty().kind()),
        }
      };
      asm_text.push_str(&format!("\tli t2, {}\n", 
        elem_size
      ));
    }
    asm_text.push_str(&format!("\tmul t1, t1, t2\n")); // 计算偏移量
    asm_text.push_str(&format!("\tadd t0, t0, t1\n")); // 计算元素地址
    asm_text.push_str(&Reg::store_reg2stack(c, Reg::T0, c.inst.unwrap())); // 保存结果
    asm_text
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Reg {
    A0, A1, A2, A3, A4, A5, A6, A7,
    S0, S1, S2, S3, S4, S5, S6, S7,
    T0, T1, T2, T3, T4, T5, T6, Zero,
    SP,
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
            Reg::SP => "sp",
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
      "sp" => Reg::SP,
      _ => panic!("Unknown register: {}", s),
    }
  }
  pub fn load_val2reg(c: &mut Context, val: Value, reg: Reg) -> String {
    let mut asm_text = String::new();
    if val.is_global() {
      let val_data = c.program.borrow_value(val);
      match val_data.kind() {
        ValueKind::GlobalAlloc(global_alloc) => {
          let alloc_val =  c.program.borrow_value(global_alloc.init());
          let kind = alloc_val.kind();
          let name = val_data.name().as_ref().unwrap()
            .strip_prefix('@').unwrap();
          match kind {
            ValueKind::Integer(_) | ValueKind::ZeroInit(_) => {
              asm_text.push_str(&format!(
                "\tlui {}, %hi({})\n", reg, name
              ));
              asm_text.push_str(&format!(
                "\tlw {}, %lo({})({})\n", reg, name, reg
              ));
            }
            _ => unimplemented!("Unsupported global init value kind: {:?}", kind),
          }
        }
        _ => { unimplemented!("Unsupported global value kind: {:?}", val_data.kind()) },
      }
    } else {
      let val_data = c.program.func(c.curr_func).dfg().value(val);
      match val_data.kind() {
        ValueKind::Integer(i) => {
          asm_text.push_str(format!("\tli {}, {}\n", reg, i.value()).as_str());
        },
        _ => {
          let pos = c.get_offset(val);
          match pos {
            Position::Reg(r) => {
              if r != reg {
                asm_text.push_str(format!("\tmv {}, {}\n", reg, r).as_str());
              }
            }
            Position::Stack(offset) => {
              asm_text.push_str(&Reg::lw_sp(offset as i32, reg));
            }
          }
        }
      }
    }
    asm_text
  }
  pub fn load_arr2reg(c: &mut Context, val: Value, reg: Reg) -> String {
    let mut asm_text = String::new();
    if val.is_global() {
      let val_data = c.program.borrow_value(val);
      match val_data.kind() {
        ValueKind::GlobalAlloc(global_alloc) => {
          let alloc_val =  c.program.borrow_value(global_alloc.init());
          let kind = alloc_val.kind();
          let name = val_data.name().as_ref().unwrap()
            .strip_prefix('@').unwrap();
          match kind {
            ValueKind::Aggregate(_) | ValueKind::ZeroInit(_) => {
              asm_text.push_str(&format!(
                "\tlui {}, %hi({})\n", reg, name
              )); // 全局变量的地址存放在 reg 寄存器中
              asm_text.push_str(&format!(
                "\taddi {}, {}, %lo({})\n", reg, reg, name
              )); // 计算全局变量的地址
            }
            _ => unimplemented!("Unsupported global init value kind: {:?}", kind),
          }
        }
        _ => { unimplemented!("Unsupported global value kind: {:?}", val_data.kind()) },
      }
    } else {
      let pos = c.get_offset(val);
      match pos {
        Position::Reg(r) => {
          if r != reg {
            asm_text.push_str(format!("\tmv {}, {}\n", reg, r).as_str());
          }
        }
        Position::Stack(offset) => {
          let src_kind = c.program.func(c.curr_func).dfg().value(val).kind();
          match src_kind {
            ValueKind::Alloc(_) => {
              asm_text.push_str(&Reg::add_sp(offset as i32, reg));
            }
            ValueKind::GetElemPtr(_) => {
              asm_text.push_str(&Reg::lw_sp(offset as i32, Reg::A0));
            }
            _ => unimplemented!("Unsupported source kind for array load: {:?}", src_kind),
          }
        }
      }
    }
    asm_text
  }
  pub fn store_reg2stack(c: &mut Context, reg: Reg, val: Value) -> String {
    let mut asm_text = String::new();
    if val.is_global() {
      let val_data = c.program.borrow_value(val);
      match val_data.kind() {
        ValueKind::GlobalAlloc(global_alloc) => {
          let alloc_val =  c.program.borrow_value(global_alloc.init());
          let kind = alloc_val.kind();
          let name = val_data.name().as_ref().unwrap()
            .strip_prefix('@').unwrap();
          match kind {
            ValueKind::Integer(_) | ValueKind::ZeroInit(_)=> {
              asm_text.push_str(&format!(
                "\tlui {}, %hi({})\n", Reg::A0, name
              )); // 全局变量的地址存放在 a0 寄存器中
              asm_text.push_str(&format!(
                "\tsw {}, %lo({})({})\n", reg, name, Reg::A0
              ));
            }
            _ => unimplemented!("Unsupported global init value kind: {:?}", val_data.kind()),
          }
        }
        _ => { unimplemented!("Unsupported global value kind: {:?}", val_data.kind()) },
      }
    } else {
      let pos = c.get_offset(val);
      match pos {
        Position::Reg(r) => {
          asm_text.push_str(&format!("\tmv {}, {}\n", r, reg));
        }
        Position::Stack(offset) => {
          asm_text.push_str(&Reg::sw_sp(offset as i32, reg));
        }
      }
    }
    asm_text
  }

  pub fn add_sp(offset: i32, target: Reg) -> String {
    let mut asm_text = String::new();
    if offset > 2047 || offset < -2048 {
      asm_text.push_str(&format!("\tli {}, {}\n", Reg::A1, offset));
      asm_text.push_str(&format!("\tadd {}, sp, {}\n", target, Reg::A1));
    } else {
      asm_text.push_str(&format!("\taddi {}, sp, {}\n", target, offset));
    }
    asm_text
  }

  pub fn lw_sp(offset: i32, target: Reg) -> String {
    let mut asm_text = String::new();
    if offset > 2047 || offset < -2048 {
      asm_text.push_str(&format!("\tli {}, {}\n", Reg::A1, offset));
      asm_text.push_str(&format!("\tadd {}, sp, {}\n", Reg::A1, Reg::A1));
      asm_text.push_str(&format!("\tlw {}, 0({})\n", target, Reg::A1));
    } else {
      asm_text.push_str(&format!("\tlw {}, {}(sp)\n", target, offset));
    }
    asm_text
  }

  pub fn sw_sp(offset: i32, target: Reg) -> String {
    let mut asm_text = String::new();
    if offset > 2047 || offset < -2048 {
      asm_text.push_str(&format!("\tli {}, {}\n", Reg::A1, offset));
      asm_text.push_str(&format!("\tadd {}, sp, {}\n", Reg::A1, Reg::A1));
      asm_text.push_str(&format!("\tsw {}, 0({})\n", target, Reg::A1));
    } else {
      asm_text.push_str(&format!("\tsw {}, {}(sp)\n", target, offset));
    }
    asm_text
  }
  pub fn arr_load2reg(c: &mut Context, src: Value, target: Reg) -> String {
    let mut asm_text = String::new();
    if src.is_global() {
      let val_data = c.program.borrow_value(src);
      match val_data.kind() {
        ValueKind::GlobalAlloc(global_alloc) => {
          let alloc_val =  c.program.borrow_value(global_alloc.init());
          let kind = alloc_val.kind();
          let name = val_data.name().as_ref().unwrap()
            .strip_prefix('@').unwrap();
          match kind {
            ValueKind::Aggregate(_) => {
              asm_text.push_str(&format!(
                "\tlui {}, %hi({})\n", Reg::A0, name
              )); // 全局变量的地址存放在 a0 寄存器中
              asm_text.push_str(&format!(
                "\taddi {}, {}, %lo({})\n", target, Reg::A0, name
              )); // 计算全局变量的地址
            }
            _ => unimplemented!("Unsupported global init value kind: {:?}", kind),
          }
        }
        _ => { unimplemented!("Unsupported global value kind: {:?}", val_data.kind()) },
      }
    } else {
      let pos = c.get_offset(src);
      match pos {
        Position::Reg(r) => {
          if r != target {
            asm_text.push_str(format!("\tmv {}, {}\n", r, target).as_str());
          }
        }
        Position::Stack(offset) => {
          asm_text.push_str(&Reg::lw_sp(offset as i32, Reg::A0));
          asm_text.push_str(&format!("\tlw {}, 0({})\n", target, Reg::A0));
        }
      }
    }
    asm_text
  }

  pub fn arr_store2stack(c: &mut Context, src: Reg, val: Value) -> String {
    let mut asm_text = String::new();
    if val.is_global() {
      let val_data = c.program.borrow_value(val);
      match val_data.kind() {
        ValueKind::GlobalAlloc(_) => {
          let name = val_data.name().as_ref().unwrap()
            .strip_prefix('@').unwrap();
          asm_text.push_str(&format!(
            "\tlui {}, %hi({})\n", Reg::A0, name
          )); // 全局变量的地址存放在 a0 寄存器中
          asm_text.push_str(&format!(
            "\taddi {}, {}, %lo({})\n", Reg::A0, Reg::A0, name
          )); // 计算全局变量的地址
          asm_text.push_str(&format!(
            "\tsw {}, 0({})\n", src, Reg::A0
          ));
        }
        _ => { unimplemented!("Unsupported global value kind: {:?}", val_data.kind()) },
      }
    } else {
      let pos = c.get_offset(val);
      match pos {
        Position::Reg(r) => {
          asm_text.push_str(&format!("\tlw {}, 0({})\n", Reg::A0, r));
          asm_text.push_str(&format!("\tmv {}, {}\n", r, Reg::A0));
        }
        Position::Stack(offset) => {
          asm_text.push_str(&Reg::lw_sp(offset as i32, Reg::A0));
          asm_text.push_str(&format!("\tsw {}, 0({})\n", src, Reg::A0));
        }
      }
    }
    asm_text
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