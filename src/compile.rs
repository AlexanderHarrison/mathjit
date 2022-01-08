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
    MoveF {dest: FloatReg, source: FloatReg},
    MoveVar {dest: FloatReg, source: VariableIndex},
    MoveFConst {dest: FloatReg, source: f32},
    Negate(FloatReg),
    AddF {dest: FloatReg, op: FloatReg},
    MulF {dest: FloatReg, op: FloatReg},
}

pub type VariableIndex = u32;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FloatRegState {
    pub lowest: usize
}

impl FloatRegState {
    fn low_reg(self) -> FloatReg { FloatReg(self.lowest) }
    fn incremented(self) -> FloatRegState {
        FloatRegState { lowest: self.lowest + 1 }
    }
}

//#[derive(Copy, Clone, PartialEq, Eq, Debug)]
//pub enum FloatSource {
//    Reg(FloatReg),
//    Var(VariableIndex),
//}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FloatReg(pub usize);

pub fn compile_equation(eq: &Equation) -> Box<[Inst]> {
    let mut insts = Vec::new();

    compile_operation(&eq.operation, &mut insts, &eq.variables, FloatRegState { lowest: eq.variables.len() });
    
    insts.into_boxed_slice()
}

pub fn compile_operation(op: &Operation, insts: &mut Vec<Inst>, vars: &[Variable], reg_state: FloatRegState) {
    match op {
        Operation::Literal(n) => {
            insts.push(Inst::MoveFConst { dest: reg_state.low_reg(), source: *n });
        },
        Operation::Variable(v) => {
            let var = vars.iter().position(|&p| p == *v).unwrap() as VariableIndex;
            insts.push(Inst::MoveVar { dest: reg_state.low_reg(), source: var });
        },
        Operation::Neg(op) => {
            compile_operation(op.as_ref(), insts, vars, reg_state);
            insts.push(Inst::Negate(reg_state.low_reg()));
        },
        Operation::Add(ops) => {
            assert!(ops.len() > 0);
            compile_operation(&ops[0], insts, vars, reg_state);
            let incremented_reg_state = reg_state.incremented();
            for op in ops[1..].iter() {
                compile_operation(op, insts, vars, incremented_reg_state);
                insts.push(Inst::AddF { dest: reg_state.low_reg(), op: incremented_reg_state.low_reg() } );
            }
        },
        Operation::Mul(ops) => {
            assert!(ops.len() > 0);
            compile_operation(&ops[0], insts, vars, reg_state);
            let incremented_reg_state = reg_state.incremented();
            for op in ops[1..].iter() {
                compile_operation(op, insts, vars, incremented_reg_state);
                insts.push(Inst::MulF { dest: reg_state.low_reg(), op: incremented_reg_state.low_reg() } );
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

impl Inst {
    pub fn add_to_inst_stream(self, assembler: &mut dynasmrt::x64::Assembler) {
        use dynasmrt::{dynasm, DynasmApi};
        match self {
            Inst::Negate(low) => {
                let next_low = FloatReg(low.0 + 1);
                dyn_reg!(assembler, (low, next_low)
                    ; movss next_low, low
                    ; pxor low, low
                    ; subss low, next_low
                );
            },
            Inst::MoveF {dest, source} if dest == source => {},
            Inst::MoveF {dest, source} => {
                dyn_reg!(assembler, (dest, source)
                    ; movss dest, source
                );
            },
            Inst::MoveVar { dest, source: v } => {
                let var_reg = FloatReg(v as usize);
                dyn_reg!(assembler, (dest, var_reg)
                    ; movss dest, var_reg
                );
            },
            Inst::MoveFConst {dest, source} => {
                let source_bits = source.to_ne_bytes();
                let source_bits: u32 = unsafe { std::mem::transmute(source_bits) };
                dynasm!(assembler
                    ; mov r0d, DWORD source_bits as _
                );
                dyn_reg!(assembler, (dest)
                    ; movd dest, r0d
                );
            },
            Inst::AddF {dest, op} => {
                dyn_reg!(assembler, (dest, op)
                    ; addss dest, op
                );
            },
            Inst::MulF {dest, op} => {
                dyn_reg!(assembler, (dest, op)
                    ; mulss dest, op
                );
            },
        }
    }
}
