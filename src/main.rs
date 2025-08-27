use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
use std::io::Result;
use compiler::{ast::*, ast2ir};
use compiler::{koopa_text, riscv_text};

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

fn main() -> Result<()> {
  // 解析命令行参数
  let mut args = args();
  args.next();
  let mode = args.next().unwrap();
  let input = args.next().unwrap();
  args.next();
  let output = args.next().unwrap();

  // 读取输入文件
  let input = read_to_string(input)?;

  // 调用 lalrpop 生成的 parser 解析输入文件
  let mut ast: Module = sysy::ModuleParser::new().parse(&input).unwrap();

  let program = ast2ir(&mut ast);

  // 根据 mode 选择不同的处理方式
  let text = match mode.as_str() {
    "-koopa" => {
      // 生成 Koopa IR
      koopa_text(program)
    }

    "-riscv" => {
      // 生成 RISC-V 汇编代码
      riscv_text(program)
    }

    _ => {
      panic!("Unknown mode: {}", mode);
    }
  };
  std::fs::write(output, text)
}
