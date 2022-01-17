// NOTES TO SELF
// input ptr: rdi
// output ptr: rsi
// input_len: rdx

// The lowest XMM registers are reserved for variables.
// E.g for x+y, XMM0 is reserved for x and XMM1 reserved for y 
use expr_parse::{Operation, Variable, Expression};
use crate::assemble::{LabelsSSE, LabelsAVX};

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Inst {
    MoveFReg {dest: FloatReg, source: FloatReg}, 
    MoveFLit {dest: FloatReg, literal: LiteralIndex}, 
    Negate(FloatReg),
    AddF {dest: FloatReg, op: FloatReg},
    SubF {dest: FloatReg, op: FloatReg},
    MulF {dest: FloatReg, op: FloatReg},
    DivF {dest: FloatReg, op: FloatReg}
}

type LiteralIndex = u8;
type VariableIndex = u8;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FloatRegState {
    pub lowest: usize
}

impl FloatRegState {
    fn low_reg(self) -> FloatReg { FloatReg(self.lowest as u8) }
    fn incremented(self) -> FloatRegState {
        FloatRegState { lowest: self.lowest + 1 }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FloatReg(pub u8);

impl FloatReg {
    pub const fn to_native_reg(self) -> iced_x86::code_asm::AsmRegisterXmm {
        use iced_x86::code_asm::registers::xmm::*;
        match self.0 {
            0  => xmm0,
            1  => xmm1,
            2  => xmm2,
            3  => xmm3,
            4  => xmm4,
            5  => xmm5,
            6  => xmm6,
            7  => xmm7,
            8  => xmm8,
            9  => xmm9,
            10 => xmm10,
            11 => xmm11,
            12 => xmm12,
            13 => xmm13,
            14 => xmm14,
            15 => xmm15,
            _ => unimplemented!(),
        }
    }

    pub const fn to_avx_reg(self) -> iced_x86::code_asm::AsmRegisterYmm {
        use iced_x86::code_asm::registers::ymm::*;
        match self.0 {
            0  => ymm0,
            1  => ymm1,
            2  => ymm2,
            3  => ymm3,
            4  => ymm4,
            5  => ymm5,
            6  => ymm6,
            7  => ymm7,
            8  => ymm8,
            9  => ymm9,
            10 => ymm10,
            11 => ymm11,
            12 => ymm12,
            13 => ymm13,
            14 => ymm14,
            15 => ymm15,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub struct CompileInfo<'a> {
    pub insts: Vec<Inst>,
    pub literals: Vec<f32>,
    pub vars: &'a [Variable],
}

pub fn compile_expression(expr: &Expression) -> CompileInfo {
    let insts = Vec::new();
    let literals = Vec::new();

    let mut info = CompileInfo {
        insts,
        literals,
        vars: &expr.variables,
    };

    compile_operation(&expr.operation, FloatRegState { lowest: expr.variables.len() }, &mut info);
    
    info
}

fn variable_index(vars: &[Variable], v: &Variable) -> VariableIndex {
    vars.iter().position(|p| *p == *v).unwrap() as u8
}

fn literal_index(literals: &mut Vec<f32>, lit: f32) -> LiteralIndex {
    literals.iter().position(|n| *n == lit)
        .unwrap_or_else(|| {literals.push(lit); literals.len() - 1 }) as u8
}

pub fn compile_operation(
    op: &Operation,
    reg_state: FloatRegState,
    info: &mut CompileInfo,
) {
    match op {
        Operation::Literal(n) => {
            let lit_index = literal_index(&mut info.literals, *n);
            info.insts.push(
                Inst::MoveFLit { dest: reg_state.low_reg(), literal: lit_index}
            );
        },
        Operation::Variable(v) => {
            let v_i = variable_index(info.vars, v);
            info.insts.push(
                Inst::MoveFReg { dest: reg_state.low_reg(), source: FloatReg(v_i) }
            );
        },
        Operation::Neg(op) => {
            compile_operation(op.as_ref(), reg_state, info);
            info.insts.push(Inst::Negate(reg_state.low_reg()));
        },
        Operation::Add(ops) => {
            assert!(ops.len() > 0);
            compile_operation(&ops[0], reg_state, info);
            let inc_reg_state = reg_state.incremented();
            for op in ops[1..].iter() {
                match op {
                    Operation::Neg(next_op) => {
                        if let Operation::Variable(v) = &**next_op {
                            let op = FloatReg(variable_index(info.vars, &v));
                            info.insts.push(Inst::SubF { dest: reg_state.low_reg(), op });
                        } else {
                            compile_operation(next_op, inc_reg_state, info);
                            let op = inc_reg_state.low_reg();
                            info.insts.push(Inst::SubF { dest: reg_state.low_reg(), op } );
                        }
                    },
                    Operation::Variable(v) => {
                        let op = FloatReg(variable_index(info.vars, v));
                        info.insts.push(Inst::AddF { dest: reg_state.low_reg(), op });
                    },
                    op => {
                        compile_operation(op, inc_reg_state, info);
                        let op = inc_reg_state.low_reg();
                        info.insts.push(Inst::AddF { dest: reg_state.low_reg(), op } );
                    }
                }
            }
        },
        Operation::Mul(ops) => {
            assert!(ops.len() > 0);
            compile_operation(&ops[0], reg_state, info);
            for op in ops[1..].iter() {
                match op {
                    Operation::Variable(v) => {
                        let op = FloatReg(variable_index(info.vars, v));
                        info.insts.push(Inst::MulF { dest: reg_state.low_reg(), op });
                    },
                    op => {
                        let inc_reg_state = reg_state.incremented();
                        compile_operation(op, inc_reg_state, info);
                        let op = inc_reg_state.low_reg();
                        info.insts.push(Inst::MulF { dest: reg_state.low_reg(), op } );
                    }
                }
            }
        }
        Operation::Div(ops) => {
            compile_operation(&ops.0, reg_state, info);
            match &ops.1 {
                Operation::Variable(v) => {
                    let op = FloatReg(variable_index(info.vars, v));
                    info.insts.push(Inst::DivF { dest: reg_state.low_reg(), op });
                }
                op => {
                    let inc_reg_state = reg_state.incremented();
                    compile_operation(op, inc_reg_state, info);
                    let op = inc_reg_state.low_reg();
                    info.insts.push(Inst::DivF { dest: reg_state.low_reg(), op } );
                }
            }
        }
    }
}

impl Inst {
    pub fn add_to_inst_stream_single_sse(
        self,
        assembler: &mut iced_x86::code_asm::CodeAssembler,
        labels: &LabelsSSE,
    ) -> Result<(), iced_x86::code_asm::IcedError> {
        match self {
            Inst::Negate(low) => {
                use iced_x86::code_asm::xmmword_ptr;
                assembler.xorps(low.to_native_reg(), xmmword_ptr(labels.negation_literal))?;
            },
            Inst::MoveFReg {dest, source} if dest == source => {},
            Inst::MoveFReg {dest, source} => {
                assembler.movss(dest.to_native_reg(), source.to_native_reg())?;
            },
            Inst::MoveFLit {dest, literal: lit_index} => {
                use iced_x86::code_asm::dword_ptr;
                assembler.movd(
                    dest.to_native_reg(),
                    dword_ptr(labels.expression_literals[lit_index as usize])
                )?;
            },
            Inst::AddF {dest, op} => {
                assembler.addss(dest.to_native_reg(), op.to_native_reg())?;
            },
            Inst::SubF {dest, op} => {
                assembler.subss(dest.to_native_reg(), op.to_native_reg())?;
            },
            Inst::MulF {dest, op} => {
                assembler.mulss(dest.to_native_reg(), op.to_native_reg())?;
            },
            Inst::DivF {dest, op} => {
                assembler.divss(dest.to_native_reg(), op.to_native_reg())?;
            },
        }

        Ok(())
    }

    pub fn add_to_inst_stream_ps_sse(
        self,
        assembler: &mut iced_x86::code_asm::CodeAssembler,
        labels: &LabelsSSE,
    ) -> Result<(), iced_x86::code_asm::IcedError> {
        match self {
            Inst::Negate(low) => {
                use iced_x86::code_asm::xmmword_ptr;
                assembler.xorps(low.to_native_reg(), xmmword_ptr(labels.negation_literal))?;
            },
            Inst::MoveFReg {dest, source} if dest == source => {},
            Inst::MoveFReg {dest, source} => {
                assembler.movaps(dest.to_native_reg(), source.to_native_reg())?;
            },
            Inst::MoveFLit {dest, literal: lit_index} => {
                use iced_x86::code_asm::dword_ptr;
                assembler.movaps(
                    dest.to_native_reg(),
                    dword_ptr(labels.expression_literals[lit_index as usize])
                )?;
            },
            Inst::AddF {dest, op} => {
                assembler.addps(dest.to_native_reg(), op.to_native_reg())?;
            },
            Inst::SubF {dest, op} => {
                assembler.subps(dest.to_native_reg(), op.to_native_reg())?;
            },
            Inst::MulF {dest, op} => {
                assembler.mulps(dest.to_native_reg(), op.to_native_reg())?;
            },
            Inst::DivF {dest, op} => {
                assembler.divps(dest.to_native_reg(), op.to_native_reg())?;
            },
        }

        Ok(())
    }

    pub fn add_to_inst_stream_single_avx(
        self,
        assembler: &mut iced_x86::code_asm::CodeAssembler,
        labels: &LabelsAVX,
    ) -> Result<(), iced_x86::code_asm::IcedError> {
        match self {
            Inst::Negate(low) => {
                use iced_x86::code_asm::dword_ptr;
                let next_low = FloatReg(low.0 + 1).to_native_reg();
                assembler.vmovd(next_low, dword_ptr(labels.negation_literal))?;
                assembler.vxorps(low.to_native_reg(), low.to_native_reg(), next_low)?;
            },
            Inst::MoveFReg {dest, source} if dest == source => {},
            Inst::MoveFReg {dest, source} => {
                assembler.vmovaps(dest.to_native_reg(), source.to_native_reg())?;
            },
            Inst::MoveFLit {dest, literal: lit_index} => {
                use iced_x86::code_asm::dword_ptr;
                assembler.vmovd(
                    dest.to_native_reg(),
                    dword_ptr(labels.expression_literals[lit_index as usize])
                )?;
            },
            Inst::AddF {dest, op} => {
                assembler.vaddss(dest.to_native_reg(), dest.to_native_reg(), op.to_native_reg())?;
            },
            Inst::SubF {dest, op} => {
                assembler.vsubss(dest.to_native_reg(), dest.to_native_reg(), op.to_native_reg())?;
            },
            Inst::MulF {dest, op} => {
                assembler.vmulss(dest.to_native_reg(), dest.to_native_reg(), op.to_native_reg())?;
            },
            Inst::DivF {dest, op} => {
                assembler.vdivss(dest.to_native_reg(), dest.to_native_reg(), op.to_native_reg())?;
            },
        }

        Ok(())
    }

    pub fn add_to_inst_stream_ps_avx(
        self,
        assembler: &mut iced_x86::code_asm::CodeAssembler,
        labels: &LabelsAVX,
    ) -> Result<(), iced_x86::code_asm::IcedError> {
        match self {
            Inst::Negate(low) => {
                use iced_x86::code_asm::dword_ptr;
                let next_low = FloatReg(low.0 + 1).to_avx_reg();
                assembler.vbroadcastss(next_low, dword_ptr(labels.negation_literal))?;
                assembler.vxorps(low.to_avx_reg(), low.to_avx_reg(), next_low)?;
            },
            Inst::MoveFReg {dest, source} if dest == source => {},
            Inst::MoveFReg {dest, source} => {
                assembler.vmovaps(dest.to_avx_reg(), source.to_avx_reg())?;
            },
            Inst::MoveFLit {dest, literal: lit_index} => {
                use iced_x86::code_asm::dword_ptr;
                assembler.vbroadcastss(dest.to_avx_reg(), dword_ptr(labels.expression_literals[lit_index as usize]))?;
            },
            Inst::AddF {dest, op} => {
                assembler.vaddps(dest.to_avx_reg(), dest.to_avx_reg(), op.to_avx_reg())?;
            },
            Inst::SubF {dest, op} => {
                assembler.vsubps(dest.to_avx_reg(), dest.to_avx_reg(), op.to_avx_reg())?;
            },
            Inst::MulF {dest, op} => {
                assembler.vmulps(dest.to_avx_reg(), dest.to_avx_reg(), op.to_avx_reg())?;
            },
            Inst::DivF {dest, op} => {
                assembler.vdivps(dest.to_avx_reg(), dest.to_avx_reg(), op.to_avx_reg())?;
            },
        }

        Ok(())
    }
}
