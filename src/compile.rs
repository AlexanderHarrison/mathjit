// NOTES TO SELF
// input ptr: rdi
// output ptr: rsi
// len: rdx

// XMM0 is the target register of all operations
// XMM15 is reserved for var 1
use eq_parse::{Operation, Variable};

// Each Oper results in a single float in XMM0
#[derive(Copy, Clone, PartialEq, Debug)]
#[allow(dead_code)]
pub enum Inst {
    PushF(FloatReg),
    PopF(FloatReg),
    MoveF {dest: FloatReg, source: FloatReg},
    MoveVar {dest: FloatReg, source: Variable},
    MoveFConst {dest: FloatReg, source: f32},
    Negate(FloatReg),
    AddF {dest: FloatReg, op: FloatReg},
    MulF {dest: FloatReg, op: FloatReg},
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FloatReg {
    XMM0,
    XMM1,
}

//#[derive(Copy, Clone, PartialEq)]
//enum RegularReg {
//    RDI,
//    RSI,
//    RDX,
//}

pub fn compile_operation(op: &Operation, insts: &mut Vec<Inst>) {
    match op {
        Operation::Literal(n) => {
            insts.push(Inst::MoveFConst { dest: FloatReg::XMM0, source: *n });
        },
        Operation::Variable(v) => {
            insts.push(Inst::MoveVar { dest: FloatReg::XMM0, source: *v });
        },
        Operation::Neg(op) => {
            compile_operation(op.as_ref(), insts);
            insts.push(Inst::Negate(FloatReg::XMM0));
        },
        Operation::Add(ops) => {
            assert!(ops.len() > 0);
            eval_all_to_stack(ops, insts);
            insts.pop(); // want last result in xmm0, not stack
    
            for _ in 1..ops.len() {
                insts.push(Inst::PopF(FloatReg::XMM1));
                insts.push(Inst::AddF { dest: FloatReg::XMM0, op: FloatReg::XMM1 });
            }
        },
        Operation::Mul(ops) => {
            assert!(ops.len() > 0);
            eval_all_to_stack(ops, insts);
            insts.pop(); // want last result in xmm0, not stack

            for _ in 1..ops.len() {
                insts.push(Inst::PopF(FloatReg::XMM1));
                insts.push(Inst::MulF { dest: FloatReg::XMM0, op: FloatReg::XMM1 });
            }
        }
    }
}

fn eval_all_to_stack(ops: &[Operation], insts: &mut Vec<Inst>) {
    for op in ops {
        compile_operation(op, insts);
        insts.push(Inst::PushF(FloatReg::XMM0));
    }
}

impl Inst {
    pub fn add_to_inst_stream(self, assembler: &mut dynasmrt::x64::Assembler) {
        use dynasmrt::{dynasm, DynasmApi};
        match self {
            Inst::PushF(FloatReg::XMM0) => {
                dynasm!(assembler
                    ; .arch x64
                    ; movd r0d, xmm0
                    ; push r0
                );
            }
            Inst::PushF(FloatReg::XMM1) => {
                dynasm!(assembler
                    ; .arch x64
                    ; movd r0d, xmm1
                    ; push r0
                );
            }
            Inst::PopF(FloatReg::XMM0) => {
                dynasm!(assembler
                    ; .arch x64
                    ; pop r0
                    ; movd xmm0, r0d
                );
            }
            Inst::PopF(FloatReg::XMM1) => {
                dynasm!(assembler
                    ; .arch x64
                    ; pop r0
                    ; movd xmm1, r0d
                );
            }
            Inst::MoveF {dest: FloatReg::XMM0, source: FloatReg::XMM1} => {
                dynasm!(assembler
                    ; .arch x64
                    ; movss xmm0, xmm1
                );
            },
            Inst::MoveF {dest: FloatReg::XMM1, source: FloatReg::XMM0} => {
                dynasm!(assembler
                    ; .arch x64
                    ; movss xmm1, xmm0
                );
            },
            Inst::MoveF {dest: _, source: _} => {},
            Inst::MoveVar { dest: FloatReg::XMM0, source: Variable(_) } => {
                dynasm!(assembler
                    ; .arch x64
                    ; movss xmm0, xmm15
                );
            },
            Inst::MoveVar { dest: FloatReg::XMM1, source: Variable(_) } => {
                dynasm!(assembler
                    ; .arch x64
                    ; movss xmm1, xmm15
                );
            },
            Inst::MoveFConst {dest: FloatReg::XMM0, source} => {
                let source_bits = source.to_ne_bytes();
                let source_bits: u32 = unsafe { std::mem::transmute(source_bits) };
                dynasm!(assembler
                    ; .arch x64
                    ; mov r0d, DWORD source_bits as _
                    ; movd xmm0, r0d
                );
            },
            Inst::MoveFConst {dest: FloatReg::XMM1, source} => {
                let source_bits = source.to_ne_bytes();
                let source_bits: u32 = unsafe { std::mem::transmute(source_bits) };
                dynasm!(assembler
                    ; .arch x64
                    ; mov r0d, DWORD source_bits as _
                    ; movd xmm1, r0d
                );
            },
            Inst::Negate(FloatReg::XMM0) => {
                dynasm!(assembler
                    ; .arch x64
                    ; movss xmm1, xmm0
                    ; pxor xmm0, xmm0
                    ; subss xmm0, xmm1
                );
            },
            Inst::Negate(FloatReg::XMM1) => {
                dynasm!(assembler
                    ; .arch x64
                    ; movss xmm2, xmm1
                    ; pxor xmm1, xmm1
                    ; subss xmm1, xmm2
                );
            },
            Inst::AddF {dest: FloatReg::XMM0, op: FloatReg::XMM0} => {
                dynasm!(assembler
                    ; .arch x64
                    ; addss xmm0, xmm0
                );
            },
            Inst::AddF {dest: FloatReg::XMM0, op: FloatReg::XMM1} => {
                dynasm!(assembler
                    ; .arch x64
                    ; addss xmm0, xmm1
                );
            },
            Inst::AddF {dest: FloatReg::XMM1, op: FloatReg::XMM0} => {
                dynasm!(assembler
                    ; .arch x64
                    ; addss xmm1, xmm0
                );
            },
            Inst::AddF {dest: FloatReg::XMM1, op: FloatReg::XMM1} => {
                dynasm!(assembler
                    ; .arch x64
                    ; addss xmm1, xmm1
                );
            },
            Inst::MulF {dest: FloatReg::XMM0, op: FloatReg::XMM0} => {
                dynasm!(assembler
                    ; .arch x64
                    ; mulss xmm0, xmm0
                );
            },
            Inst::MulF {dest: FloatReg::XMM0, op: FloatReg::XMM1} => {
                dynasm!(assembler
                    ; .arch x64
                    ; mulss xmm0, xmm1
                );
            },
            Inst::MulF {dest: FloatReg::XMM1, op: FloatReg::XMM0} => {
                dynasm!(assembler
                    ; .arch x64
                    ; mulss xmm1, xmm0
                );
            },
            Inst::MulF {dest: FloatReg::XMM1, op: FloatReg::XMM1} => {
                dynasm!(assembler
                    ; .arch x64
                    ; mulss xmm1, xmm1
                );
            },
            //_ => ()
        }
    }
}
