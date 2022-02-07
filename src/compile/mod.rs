// NOTES TO SELF
// input ptr: rdi
// output ptr: rsi
// input_len: rdx

// The lowest XMM registers are reserved for variables.
// E.g for x+y, XMM0 is reserved for x and XMM1 reserved for y 
use crate::expr_parse::{Operation, Variable, Expression};

pub mod optimise;
pub use optimise::{compile_expression_optimised, Optimizations};
use crate::assemble::Inst;


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
pub(crate) struct FloatRegisterOverflow();

impl FloatReg {
    pub(crate) const fn to_native_reg(self) -> Result<iced_x86::code_asm::AsmRegisterXmm, FloatRegisterOverflow> {
        use iced_x86::code_asm::registers::xmm::*;
        Ok(match self.0 {
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
            _ => return Err(FloatRegisterOverflow()),
        })
    }

    pub(crate) const fn to_avx_reg(self) -> Result<iced_x86::code_asm::AsmRegisterYmm, FloatRegisterOverflow> {
        use iced_x86::code_asm::registers::ymm::*;
        Ok(match self.0 {
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
            _ => return Err(FloatRegisterOverflow()),
        })
    }
}

#[derive(Debug)]
pub struct CompileInfo<'a> {
    pub insts: Vec<Inst>,
    pub literals: Vec<f32>,
    pub vars: &'a [Variable],
}

/// Some functions supported by `expr_parse` are not supported to compile.
/// Currently unsupported functions: `ln` and `exp`.
/// 
/// Sufficiently complicated expressions of many variables
/// may use all available floating point xmm or ymm registers.
/// This will be fixed in the future.
///
/// The underlying library for assembling and compilation, iced_x86,
/// may also occasionally error.
#[derive(Clone, Debug)]
pub enum CodeGenError {
    UnsupportedOperation(Box<str>),
    FloatRegisterOverflow,
    IcedError(iced_x86::code_asm::IcedError),
}

/// Compile an expression. 
/// The returned `CompileInfo` needs to be assembled before it can be run.
/// Usually you want to call `CompiledExpression::new()` instead of this function.
pub fn compile_expression(expr: &Expression) -> Result<CompileInfo, CodeGenError> {
    let insts = Vec::new();
    let literals = Vec::new();

    let mut info = CompileInfo {
        insts,
        literals,
        vars: &expr.variables,
    };

    compile_operation(&expr.operation, FloatRegState { lowest: expr.variables.len() }, &mut info)?;
    
    Ok(info)
}

fn variable_index(vars: &[Variable], v: &Variable) -> VariableIndex {
    vars.iter().position(|p| *p == *v).unwrap() as u8
}

fn literal_index(literals: &mut Vec<f32>, lit: f32) -> LiteralIndex {
    literals.iter().position(|n| *n == lit)
        .unwrap_or_else(|| {literals.push(lit); literals.len() - 1 }) as u8
}

/// Recursivly compile a specific operation.
/// Usually you want to call `CompiledExpression::new()` instead of this function.
pub fn compile_operation(
    op: &Operation,
    reg_state: FloatRegState,
    info: &mut CompileInfo,
) -> Result<(), CodeGenError> {
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
            compile_operation(op.as_ref(), reg_state, info)?;
            info.insts.push(Inst::Negate(reg_state.low_reg()));
        },
        Operation::Sqrt(op) => {
            compile_operation(op.as_ref(), reg_state, info)?;
            info.insts.push(Inst::Sqrt(reg_state.low_reg()));
        },
        Operation::Ln(_) => {
            return Err(CodeGenError::UnsupportedOperation(
                box_str("Ln")
            ));
        },
        Operation::Exp(_) => {
            return Err(CodeGenError::UnsupportedOperation(
                box_str("Exp")
            ));
        },
        Operation::Add(ops) => {
            assert!(ops.len() > 0);
            compile_operation(&ops[0], reg_state, info)?;
            let inc_reg_state = reg_state.incremented();
            for op in ops[1..].iter() {
                match op {
                    Operation::Neg(next_op) => {
                        let source = if let Operation::Variable(v) = &**next_op {
                            FloatSource::Register(FloatReg(variable_index(info.vars, &v)))
                        } else {
                            compile_operation(next_op, inc_reg_state, info)?;
                            FloatSource::Register(inc_reg_state.low_reg())
                        };
                        info.insts.push(Inst::SubF { dest: reg_state.low_reg(), source });
                    },
                    Operation::Variable(v) => {
                        let source = FloatSource::Register(FloatReg(variable_index(info.vars, v)));
                        info.insts.push(Inst::AddF { dest: reg_state.low_reg(), source });
                    },
                    op => {
                        compile_operation(op, inc_reg_state, info)?;
                        let source = FloatSource::Register(inc_reg_state.low_reg());
                        info.insts.push(Inst::AddF { dest: reg_state.low_reg(), source });
                    }
                }
            }
        },
        Operation::Mul(ops) => {
            assert!(ops.len() > 0);
            compile_operation(&ops[0], reg_state, info)?;
            for op in ops[1..].iter() {
                match op {
                    Operation::Variable(v) => {
                        let source = FloatSource::Register(FloatReg(variable_index(info.vars, v)));
                        info.insts.push(Inst::MulF { dest: reg_state.low_reg(), source });
                    },
                    op => {
                        let inc_reg_state = reg_state.incremented();
                        compile_operation(op, inc_reg_state, info)?;
                        let source = FloatSource::Register(inc_reg_state.low_reg());
                        info.insts.push(Inst::MulF { dest: reg_state.low_reg(), source } );
                    }
                }
            }
        }
        Operation::Div(ops) => {
            compile_operation(&ops.0, reg_state, info)?;
            match &ops.1 {
                Operation::Variable(v) => {
                    let source = FloatSource::Register(FloatReg(variable_index(info.vars, v)));
                    info.insts.push(Inst::DivF { dest: reg_state.low_reg(), source });
                }
                op => {
                    let inc_reg_state = reg_state.incremented();
                    compile_operation(op, inc_reg_state, info)?;
                    let source = FloatSource::Register(inc_reg_state.low_reg());
                    info.insts.push(Inst::DivF { dest: reg_state.low_reg(), source } );
                }
            }
        }
    }

    Ok(())
}


fn box_str(s: &str) -> Box<str> {
    s.to_string().into_boxed_str()
}

impl From<iced_x86::code_asm::IcedError> for CodeGenError {
    fn from(e: iced_x86::code_asm::IcedError) -> Self {
        CodeGenError::IcedError(e)
    }
}

impl From<FloatRegisterOverflow> for CodeGenError {
    fn from(_: FloatRegisterOverflow) -> Self {
        CodeGenError::FloatRegisterOverflow
    }
}
