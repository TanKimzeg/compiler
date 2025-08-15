use koopa::ir::{entities::ValueData, *};


pub fn riscv_text(program: Program) -> String {
    let mut asm_text = String::new();
    asm_text.push_str("\t.text\n\t.globl main\n");

    for &func in program.func_layout() {
        let func_data = program.func(func);
        println!("function {}:", func_data.name());
        asm_text.push_str(format!("{}:\n", &func_data.name()[1..]).as_str());
        for (&bb, node) in func_data.layout().bbs() {
            
            for &inst in node.insts().keys() {

                let val_data = func_data.dfg().value(inst);
                match val_data.kind() {
                    ValueKind::Return(ret) => {
                        if let Some(rv) = ret.value() {
                            let rv_data = func_data.dfg().value(rv);
                            if let Some(imm) = parse_value(rv_data, &func_data) {
                                asm_text.push_str(format!("\tli a0, {}\n", imm).as_str());
                            } else {
                                unimplemented!("Return value is not an integer");
                            }
                        }
                        asm_text.push_str("\tret\n");
                    }
                
                    _ => { unreachable!("Not supported type") }
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

