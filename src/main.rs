use koopa::ir::builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder};
use lalrpop_util::lalrpop_mod;
use koopa::ir::{self, Program};
use koopa::back::KoopaGenerator;
use std::env::args;
use std::fs::read_to_string;
use std::str;
use std::io::Result;
use compiler::ast::*;

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
  let ast: CompUnit = sysy::CompUnitParser::new().parse(&input).unwrap();

  // 输出解析得到的 AST
  println!("{:#?}", ast);

  // 将 AST 转换为 Koopa IR
  let program = convert_ast_to_ir(&ast);
  let mut generator = KoopaGenerator::new(Vec::new());
  generator.generate_on(&program).unwrap();

  // 将生成的 IR 写入输出文件
  let text_form_ir = str::from_utf8(&generator.writer()).unwrap().to_string();
  println!("{}", text_form_ir);
  std::fs::write(output, text_form_ir)?;
  Ok(())
}

/// 第一章的 AST 很简单, 我只需要生成这样的 Koopa IR:
/// fun @main(): i32 {
/// %entry:
///   ret 0;
/// }
fn convert_ast_to_ir(ast: &CompUnit) -> ir::Program {
  let mut program = ir::Program::new();

  let func = program.new_func(ir::FunctionData::with_param_names(
    format!("@{}", ast.func_def.id), 
    Vec::new(), 
    ir::Type::get_i32(),
  ));

  let func_data = program.func_mut(func);
  {
    // entry basic block
    let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
    func_data.layout_mut().bbs_mut().extend([entry]);

    // insert a return statement with value 0
    let ret_val = func_data.dfg_mut().new_value().integer(ast.func_def.block.stmt.num);
    let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
    func_data.layout_mut().bb_mut(entry).insts_mut().push_key_back(ret);
  }
  program
}

