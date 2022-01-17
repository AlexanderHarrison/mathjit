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
    MoveF {dest: FloatReg, source: FloatSource}, 
    Negate(FloatReg),
    AddF {dest: FloatReg, source: FloatSource},
    SubF {dest: FloatReg, source: FloatSource},
    MulF {dest: FloatReg, source: FloatSource},
    DivF {dest: FloatReg, source: FloatSource}
}

type LiteralIndex = u8;
type VariableIndex = u8;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FloatSource {
    Register(FloatReg),
    Literal(LiteralIndex),
}

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
                Inst::MoveF { dest: reg_state.low_reg(), source: FloatSource::Literal(lit_index)}
            );
        },
        Operation::Variable(v) => {
            let v_i = variable_index(info.vars, v);
            info.insts.push(
                Inst::MoveF { dest: reg_state.low_reg(), source: FloatSource::Register(FloatReg(v_i)) }
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
                        let source = if let Operation::Variable(v) = &**next_op {
                            FloatSource::Register(FloatReg(variable_index(info.vars, &v)))
                        } else {
                            compile_operation(next_op, inc_reg_state, info);
                            FloatSource::Register(inc_reg_state.low_reg())
                        };
                        info.insts.push(Inst::SubF { dest: reg_state.low_reg(), source });
                    },
                    Operation::Variable(v) => {
                        let source = FloatSource::Register(FloatReg(variable_index(info.vars, v)));
                        info.insts.push(Inst::AddF { dest: reg_state.low_reg(), source });
                    },
                    op => {
                        compile_operation(op, inc_reg_state, info);
                        let source = FloatSource::Register(reg_state.low_reg());
                        info.insts.push(Inst::AddF { dest: reg_state.low_reg(), source });
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
                        let source = FloatSource::Register(FloatReg(variable_index(info.vars, v)));
                        info.insts.push(Inst::MulF { dest: reg_state.low_reg(), source });
                    },
                    op => {
                        let inc_reg_state = reg_state.incremented();
                        compile_operation(op, inc_reg_state, info);
                        let source = FloatSource::Register(inc_reg_state.low_reg());
                        info.insts.push(Inst::MulF { dest: reg_state.low_reg(), source } );
                    }
                }
            }
        }
        Operation::Div(ops) => {
            compile_operation(&ops.0, reg_state, info);
            match &ops.1 {
                Operation::Variable(v) => {
                    let source = FloatSource::Register(FloatReg(variable_index(info.vars, v)));
                    info.insts.push(Inst::DivF { dest: reg_state.low_reg(), source });
                }
                op => {
                    let inc_reg_state = reg_state.incremented();
                    compile_operation(op, inc_reg_state, info);
                    let source = FloatSource::Register(inc_reg_state.low_reg());
                    info.insts.push(Inst::DivF { dest: reg_state.low_reg(), source } );
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
            Inst::MoveF {dest, source: FloatSource::Register(source_reg) } if source_reg == dest => (),
            Inst::MoveF {dest, source: FloatSource::Register(source_reg) } => {
                assembler.movss(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::MoveF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                assembler.movd(
                    dest.to_native_reg(),
                    dword_ptr(labels.expression_literals[lit_index as usize])
                )?;
            },
            Inst::AddF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.addss(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::AddF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                let lit_ptr = dword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.addss(dest.to_native_reg(), lit_ptr)?;
            },
            Inst::SubF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.subss(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::SubF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                let lit_ptr = dword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.subss(dest.to_native_reg(), lit_ptr)?;
            },
            Inst::MulF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.mulss(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::MulF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                let lit_ptr = dword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.mulss(dest.to_native_reg(), lit_ptr)?;
            },
            Inst::DivF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.divss(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::DivF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                let lit_ptr = dword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.divss(dest.to_native_reg(), lit_ptr)?;
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
            Inst::MoveF {dest, source: FloatSource::Register(source_reg) } if source_reg == dest => (),
            Inst::MoveF {dest, source: FloatSource::Register(source_reg) } => {
                assembler.movaps(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::MoveF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::xmmword_ptr;
                assembler.movaps(
                    dest.to_native_reg(),
                    xmmword_ptr(labels.expression_literals[lit_index as usize])
                )?;
            },
            Inst::AddF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.addps(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::AddF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                let lit_ptr = dword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.addps(dest.to_native_reg(), lit_ptr)?;
            },
            Inst::SubF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.subps(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::SubF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::xmmword_ptr;
                let lit_ptr = xmmword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.subps(dest.to_native_reg(), lit_ptr)?;
            },
            Inst::MulF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.mulps(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::MulF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::xmmword_ptr;
                let lit_ptr = xmmword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.mulps(dest.to_native_reg(), lit_ptr)?;
            },
            Inst::DivF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.divps(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::DivF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::xmmword_ptr;
                let lit_ptr = xmmword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.divps(dest.to_native_reg(), lit_ptr)?;
            },
        }

        Ok(())
    }

    pub fn add_to_inst_stream_single_avx(
        self,
        assembler: &mut iced_x86::code_asm::CodeAssembler,
        labels: &LabelsAVX,
    ) -> Result<(), iced_x86::code_asm::IcedError> {
        use iced_x86::code_asm::xmmword_ptr;
        match self {
            Inst::Negate(low) => {
                let neg_lit_ptr = xmmword_ptr(labels.negation_literal);
                assembler.vxorps(low.to_native_reg(), low.to_native_reg(), neg_lit_ptr)?;
            },
            Inst::MoveF {dest, source: FloatSource::Register(source_reg) } if source_reg == dest => (),
            Inst::MoveF {dest, source: FloatSource::Register(source_reg) } => {
                assembler.vmovaps(dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::MoveF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                assembler.vmovd(
                    dest.to_native_reg(),
                    dword_ptr(labels.expression_literals[lit_index as usize])
                )?;
            },
            Inst::AddF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.vaddss(dest.to_native_reg(), dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::AddF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                let lit_ptr = dword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.vaddss(dest.to_native_reg(), dest.to_native_reg(), lit_ptr)?;
            },
            Inst::SubF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.vsubss(dest.to_native_reg(), dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::SubF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                let lit_ptr = dword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.vsubss(dest.to_native_reg(), dest.to_native_reg(), lit_ptr)?;
            },
            Inst::MulF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.vmulss(dest.to_native_reg(), dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::MulF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                let lit_ptr = dword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.vmulss(dest.to_native_reg(), dest.to_native_reg(), lit_ptr)?;
            },
            Inst::DivF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.vdivss(dest.to_native_reg(), dest.to_native_reg(), source_reg.to_native_reg())?;
            },
            Inst::DivF {dest, source: FloatSource::Literal(lit_index)} => {
                use iced_x86::code_asm::dword_ptr;
                let lit_ptr = dword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.vdivss(dest.to_native_reg(), dest.to_native_reg(), lit_ptr)?;
            },
        }

        Ok(())
    }

    pub fn add_to_inst_stream_ps_avx(
        self,
        assembler: &mut iced_x86::code_asm::CodeAssembler,
        labels: &LabelsAVX,
    ) -> Result<(), iced_x86::code_asm::IcedError> {
        use iced_x86::code_asm::ymmword_ptr;
        match self {
            Inst::Negate(low) => {
                let neg_lit_ptr = ymmword_ptr(labels.negation_literal);
                assembler.vxorps(low.to_avx_reg(), low.to_avx_reg(), neg_lit_ptr)?;
            },
            Inst::MoveF {dest, source: FloatSource::Register(source_reg) } if source_reg == dest => (),
            Inst::MoveF {dest, source: FloatSource::Register(source_reg) } => {
                assembler.vmovaps(dest.to_avx_reg(), source_reg.to_avx_reg())?;
            },
            Inst::MoveF {dest, source: FloatSource::Literal(lit_index)} => {
                let lit_ptr = ymmword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.vmovaps(dest.to_avx_reg(), lit_ptr)?;
            },
            Inst::AddF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.vaddps(dest.to_avx_reg(), dest.to_avx_reg(), source_reg.to_avx_reg())?;
            },
            Inst::AddF {dest, source: FloatSource::Literal(lit_index)} => {
                let lit_ptr = ymmword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.vaddps(dest.to_avx_reg(), dest.to_avx_reg(), lit_ptr)?;
            },
            Inst::SubF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.vsubps(dest.to_avx_reg(), dest.to_avx_reg(), source_reg.to_avx_reg())?;
            },
            Inst::SubF {dest, source: FloatSource::Literal(lit_index)} => {
                let lit_ptr = ymmword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.vsubps(dest.to_avx_reg(), dest.to_avx_reg(), lit_ptr)?;
            },
            Inst::MulF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.vmulps(dest.to_avx_reg(), dest.to_avx_reg(), source_reg.to_avx_reg())?;
            },
            Inst::MulF {dest, source: FloatSource::Literal(lit_index)} => {
                let lit_ptr = ymmword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.vmulps(dest.to_avx_reg(), dest.to_avx_reg(), lit_ptr)?;
            },
            Inst::DivF {dest, source: FloatSource::Register(source_reg)} => {
                assembler.vdivps(dest.to_avx_reg(), dest.to_avx_reg(), source_reg.to_avx_reg())?;
            },
            Inst::DivF {dest, source: FloatSource::Literal(lit_index)} => {
                let lit_ptr = ymmword_ptr(labels.expression_literals[lit_index as usize]);
                assembler.vdivps(dest.to_avx_reg(), dest.to_avx_reg(), lit_ptr)?;
            },
        }

        Ok(())
    }
}
