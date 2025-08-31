# Usage:
# make SRC=tests/lisp.c MODE=koopa
# make SRC=tests/lisp.c MODE=riscv

SHELL := /usr/bin/env bash
.SHELLFLAGS := -eu -o pipefail -c

BUILD_DIR ?= build

BASE := $(basename $(notdir $(SRC)))
# 根据模式选择中间文件后缀：koopa -> .koopa，riscv -> .S
ifeq ($(MODE),koopa)
OUT_EXT := .koopa
else
OUT_EXT := .S
endif
OUT   := $(BUILD_DIR)/$(BASE)$(OUT_EXT)
OBJ   := $(BUILD_DIR)/$(BASE).o
EXE   := $(BUILD_DIR)/$(BASE)

MODE_OPT := -$(MODE)

all: $(EXE)
	@echo "Built: $(EXE)"

$(BUILD_DIR):
	mkdir -p "$(BUILD_DIR)"

# 由编译器生成中间产物（Koopa IR 或 RISC-V 汇编）
$(OUT): $(SRC) | $(BUILD_DIR)
	cargo run --release -q -- $(MODE_OPT) "$(SRC)" -o "$(OUT)"

ifeq ($(MODE),koopa)
# Koopa 模式：从 .koopa 通过 koopac | llc 生成目标文件
$(OBJ): $(OUT) | $(BUILD_DIR)
	koopac "$(OUT)" | llc --filetype=obj -o "$(OBJ)"
$(EXE): $(OBJ)
	clang "$(OBJ)" -L"$(CDE_LIBRARY_PATH)/native" -lsysy -o "$(EXE)"
endif

ifeq ($(MODE),riscv)
# RISC-V 模式：从 .S 生成目标文件并链接为 riscv32 ELF
$(OBJ): $(OUT) | $(BUILD_DIR)
	clang "$(OUT)" -c -o "$(OBJ)" -target riscv32-unknown-linux-elf -march=rv32im -mabi=ilp32
$(EXE): $(OBJ)
	ld.lld "$(OBJ)" -L"$(CDE_LIBRARY_PATH)/riscv32" -lsysy -o "$(EXE)"
endif

.PHONY: clean
clean:
	rm -rf "$(BUILD_DIR)"