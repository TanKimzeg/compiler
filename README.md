# 北京大学编译原理实践 —— SysY 编译器

PKU 编译原理实践课程工程代码, 目标语言为 Koopa IR 与 RISC-V。工程使用 Rust 实现，语法前端基于 [lalrpop](https://docs.rs/crate/lalrpop/latest) .

![测试通过](assets/passed.png)

[课程文档](https://pku-minic.github.io/online-doc/)

## 构建与运行

- 构建:

```shell
cargo build
```

- 运行编译器:

```shell
# 生成 Koopa IR 到输出文件
cargo run -- -koopa <source.c> -o <out.S>

# 生成 RISC-V 汇编到输出文件
cargo run -- -riscv <source.c> -o <out.S>

# RISC-V 性能测试模式
cargo run -- -perf <source.c> -o <out.S>
```

## 目录结构

- `src/`:编译器源码
  - `sysy.lalrpop`: 语法定义（由 `build.rs` 在构建时生成解析器）
  - `ast/`: 抽象语法树定义与处理
  - `converter.rs`、`koopa_compiler.rs`、`riscv_compiler.rs` 等: 中间表示与后端生成
  - `main.rs`: 程序入口
- `build.rs`:构建时生成解析器等步骤
- `Cargo.toml`:Rust 包配置
