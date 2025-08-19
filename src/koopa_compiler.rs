use std::str;
use koopa::ir::Program;
use koopa::back::KoopaGenerator;

pub fn koopa_text(program: Program) -> String {
  let mut generator = KoopaGenerator::new(Vec::new());
  generator.generate_on(&program).unwrap();

  // 将生成的 IR 写入输出文件
  let text_form_ir = str::from_utf8(&generator.writer()).unwrap().to_string();
  text_form_ir
}
