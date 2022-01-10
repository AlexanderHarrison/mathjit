// NOTES TO SELF
// input ptr: rdi
// output ptr: rsi
// len: rdx

// The lowest XMM registers are reserved for variables.
// E.g for x+y, XMM0 is reserved for x and XMM1 reserved for y 
use eq_parse::{Operation, Variable, Equation};

use macro_find_and_replace::replace_token_sequence;

// Each Oper results in a single float in XMM0
#[derive(Copy, Clone, PartialEq, Debug)]
#[allow(dead_code)]
pub enum Inst {
    MoveF {dest: FloatReg, source: FloatSource},
    Negate(FloatReg),
    AddF {dest: FloatReg, op: FloatSource},
    SubF {dest: FloatReg, op: FloatSource},
    MulF {dest: FloatReg, op: FloatSource},
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

#[derive(Debug)]
struct CompileInfo<'a> {
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
    
    insts.into_boxed_slice()
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
            let lit_index = literal_index(&mut info.literals, *n);
            info.insts.push(
                Inst::MoveF { dest: reg_state.low_reg(), source: FloatSource::Literal(lit_index) }
            );
        },
        Operation::Variable(v) => {
            let v_i = variable_index(info.vars, *v);
            info.insts.push(
                Inst::MoveF { dest: reg_state.low_reg(), source: FloatSource::Variable(v_i) }
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
                            let op = FloatSource::Variable(variable_index(info.vars, v));
                            info.insts.push(Inst::SubF { dest: reg_state.low_reg(), op });
                        } else {
                            compile_operation(next_op, inc_reg_state, info);
                            let op = FloatSource::Register(inc_reg_state.low_reg());
                            info.insts.push(Inst::SubF { dest: reg_state.low_reg(), op } );
                        }
                    },
                    Operation::Variable(v) => {
                        let op = FloatSource::Variable(variable_index(info.vars, *v));
                        info.insts.push(Inst::AddF { dest: reg_state.low_reg(), op });
                    },
                    op => {
                        compile_operation(op, inc_reg_state, info);
                        let op = FloatSource::Register(inc_reg_state.low_reg());
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
                        let op = FloatSource::Variable(variable_index(info.vars, *v));
                        info.insts.push(Inst::MulF { dest: reg_state.low_reg(), op });
                    },
                    op => {
                        compile_operation(op, inc_reg_state, info);
                        let op = FloatSource::Register(inc_reg_state.low_reg());
                        info.insts.push(Inst::MulF { dest: reg_state.low_reg(), op } );
                    }
                }
            }
        }
    }
}

#[macro_export]
macro_rules! _recurse_expand {
    ($ops:ident () ($(($alias:ident $reg:ident))*) $($t:tt)*) => (
        dynasm!($ops
            ; .arch x64
            $(; .alias $alias, $reg)*
            $($t)*
        )
    );
    ($ops:ident ($a:ident $($to_alias:ident)*) ($(($alias:ident $reg:ident))*) $($t:tt)*) => (
        match $a {
            FloatReg(0) =>  _recurse_expand!($ops ($($to_alias)*) (($a xmm0)  $(($alias $reg))*) $($t)*),
            FloatReg(1) =>  _recurse_expand!($ops ($($to_alias)*) (($a xmm1)  $(($alias $reg))*) $($t)*),
            FloatReg(2) =>  _recurse_expand!($ops ($($to_alias)*) (($a xmm2)  $(($alias $reg))*) $($t)*),
            FloatReg(3) =>  _recurse_expand!($ops ($($to_alias)*) (($a xmm3)  $(($alias $reg))*) $($t)*),
            FloatReg(4) =>  _recurse_expand!($ops ($($to_alias)*) (($a xmm4)  $(($alias $reg))*) $($t)*),
            FloatReg(5) =>  _recurse_expand!($ops ($($to_alias)*) (($a xmm5)  $(($alias $reg))*) $($t)*),
            FloatReg(6) =>  _recurse_expand!($ops ($($to_alias)*) (($a xmm6)  $(($alias $reg))*) $($t)*),
            FloatReg(7) =>  _recurse_expand!($ops ($($to_alias)*) (($a xmm7)  $(($alias $reg))*) $($t)*),
            FloatReg(8) =>  _recurse_expand!($ops ($($to_alias)*) (($a xmm8)  $(($alias $reg))*) $($t)*),
            FloatReg(9) =>  _recurse_expand!($ops ($($to_alias)*) (($a xmm9)  $(($alias $reg))*) $($t)*),
            FloatReg(10) => _recurse_expand!($ops ($($to_alias)*) (($a xmm10) $(($alias $reg))*) $($t)*),
            FloatReg(11) => _recurse_expand!($ops ($($to_alias)*) (($a xmm11) $(($alias $reg))*) $($t)*),
            FloatReg(12) => _recurse_expand!($ops ($($to_alias)*) (($a xmm12) $(($alias $reg))*) $($t)*),
            FloatReg(13) => _recurse_expand!($ops ($($to_alias)*) (($a xmm13) $(($alias $reg))*) $($t)*),
            FloatReg(14) => _recurse_expand!($ops ($($to_alias)*) (($a xmm14) $(($alias $reg))*) $($t)*),
            FloatReg(15) => _recurse_expand!($ops ($($to_alias)*) (($a xmm15) $(($alias $reg))*) $($t)*),
            _ => unimplemented!()
        }
    )
}

#[macro_export]
macro_rules! dyn_reg {
    ($ops:ident, ($($reg:ident),+) $($t:tt)*) => (
        _recurse_expand!($ops ($($reg)*) () $($t)*)
    )
}

macro_rules! dyn_float_source {
    ($ops:ident, $s:ident, ($($reg:ident),*) $($t:tt)*) => {
        match $s {
            FloatSource::Register(FloatReg(source_index)) 
                | FloatSource::Variable(source_index) => 
            {
                let $s = FloatReg(source_index);
                dyn_reg!($ops, ($s, $($reg)*) $($t)*);
            }
            FloatSource::Literal(lit_index) => {
                let offset = lit_index as isize * 4;
                dyn_reg!($ops, ($($reg)*) 
                    ;; replace_token_sequence!([$s], [[->literal+offset]], $($t)*)
                );
            }
        }
    }
}

impl Inst {
    pub fn add_to_inst_stream(self, assembler: &mut dynasmrt::x64::Assembler) {
        use dynasmrt::dynasm;
        match self {
            Inst::Negate(low) => {
                let next_low = FloatReg(low.0 + 1);
                dyn_reg!(assembler, (low, next_low)
                    ; movss next_low, low
                    ; pxor low, low
                    ; subss low, next_low
                );
            },
            Inst::MoveF {dest, source: FloatSource::Register(source_reg)} if dest == source_reg => {},
            Inst::MoveF {dest, source} => {
                dyn_float_source!(assembler, source, (dest)
                    ; movss dest, source
                );
            },
            Inst::AddF {dest, op} => {

            },
            Inst::SubF {dest, op} => {
                //dyn_reg!(assembler, (dest, op)
                //    ; subss dest, op
                //);
            },
            Inst::MulF {dest, op} => {
                //dyn_reg!(assembler, (dest, op)
                //    ; mulss dest, op
                //);
            },
        }
    }
}
