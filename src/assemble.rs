use crate::{RetPtr, dyn_reg, _recurse_expand, compile::{Inst, FloatReg}};
use dynasmrt::{dynasm, DynasmApi};

// RDI is the ptr to input variables
pub fn assemble(insts: Box<[Inst]>, var_count: usize) -> (dynasmrt::mmap::ExecutableBuffer, RetPtr) {
    let mut assembler = dynasmrt::x64::Assembler::new().unwrap();

    let start = assembler.offset();

    // move variables into low registers
    for i in 0..var_count {
        let reg = FloatReg(i);
        dyn_reg!(assembler, (reg)
            ; movd reg, [rdi+i as i32*4]
        );
    }

    for inst in insts.into_iter() {
        inst.add_to_inst_stream(&mut assembler);
    }

    // move result into return register
    let low_non_var_reg = FloatReg(var_count);
    dyn_reg!(assembler, (low_non_var_reg)
        ; movss xmm0, low_non_var_reg
    );

    dynasm!(assembler
        ; .arch x64
        ; ret
    );

    let code = assembler.finalize().unwrap();

    let ptr = unsafe {
        std::mem::transmute(code.ptr(start))
    };
    (code, ptr)
}
