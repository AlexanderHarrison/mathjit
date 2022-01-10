// NOTES TO SELF
// input ptr: rdi
// output ptr: rsi
// len: rdx

// The lowest XMM registers are reserved for variables.
// E.g for x+y, XMM0 is reserved for x and XMM1 reserved for y 
use eq_parse::{Operation, Variable, Equation};

// Each Oper results in a single float in XMM0
#[derive(Copy, Clone, PartialEq, Debug)]
#[allow(dead_code)]
pub enum Inst {
    MoveFReg {dest: FloatReg, source: FloatReg}, 
    MoveFLit {dest: FloatReg, literal: f32}, 
    //MoveFMem {dest: FloatReg, source: Address}, 
    Negate(FloatReg),
    AddF {dest: FloatReg, op: FloatReg},
    SubF {dest: FloatReg, op: FloatReg},
    MulF {dest: FloatReg, op: FloatReg},
}

type LiteralIndex = u8;
type VariableIndex = u8;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FloatSource {
    Register(FloatReg),
    Variable(VariableIndex),
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
}

#[derive(Debug)]
pub struct CompileInfo<'a> {
    insts: Vec<Inst>,
    literals: Vec<f32>,
    vars: &'a [Variable],
}

pub fn compile_equation(eq: &Equation) -> Box<[Inst]> {
    let insts = Vec::new();
    let literals = Vec::new();

    let mut info = CompileInfo {
        insts,
        literals,
        vars: &eq.variables,
    };

    compile_operation(&eq.operation, FloatRegState { lowest: eq.variables.len() }, &mut info);
    
    info.insts.into_boxed_slice()
}

fn variable_index(vars: &[Variable], v: Variable) -> VariableIndex {
    vars.iter().position(|&p| p == v).unwrap() as u8
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
            //let lit_index = literal_index(&mut info.literals, *n);
            info.insts.push(
                Inst::MoveFLit { dest: reg_state.low_reg(), literal: *n}
            );
        },
        Operation::Variable(v) => {
            let v_i = variable_index(info.vars, *v);
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
                        if let Operation::Variable(v) = **next_op {
                            let op = FloatReg(variable_index(info.vars, v));
                            info.insts.push(Inst::SubF { dest: reg_state.low_reg(), op });
                        } else {
                            compile_operation(next_op, inc_reg_state, info);
                            let op = inc_reg_state.low_reg();
                            info.insts.push(Inst::SubF { dest: reg_state.low_reg(), op } );
                        }
                    },
                    Operation::Variable(v) => {
                        let op = FloatReg(variable_index(info.vars, *v));
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
            let inc_reg_state = reg_state.incremented();
            for op in ops[1..].iter() {
                match op {
                    Operation::Variable(v) => {
                        let op = FloatReg(variable_index(info.vars, *v));
                        info.insts.push(Inst::MulF { dest: reg_state.low_reg(), op });
                    },
                    op => {
                        compile_operation(op, inc_reg_state, info);
                        let op = inc_reg_state.low_reg();
                        info.insts.push(Inst::MulF { dest: reg_state.low_reg(), op } );
                    }
                }
            }
        }
    }
}

impl Inst {
    pub fn add_to_inst_stream(
        self,
        assembler: &mut iced_x86::code_asm::CodeAssembler
) -> Result<(), iced_x86::code_asm::IcedError> {
        match self {
            Inst::Negate(low) => {
                let next_low = FloatReg(low.0 + 1).to_native_reg();
                let low = low.to_native_reg();
                assembler.movss(next_low, low)?;
                assembler.pxor(low, low)?;
                assembler.subss(low, next_low)?;
            },
            Inst::MoveFReg {dest, source} if dest == source => {},
            Inst::MoveFReg {dest, source} => {
                assembler.movss(dest.to_native_reg(), source.to_native_reg())?;
            },
            Inst::MoveFLit {dest, literal} => {
                use iced_x86::code_asm::registers::gpr32::*;
                let lit_bytes: u32 = unsafe { std::mem::transmute(literal.to_ne_bytes()) };
                assembler.mov(eax, lit_bytes)?;
                assembler.movd(dest.to_native_reg(), eax)?;
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
        }

        Ok(())
    }
}
