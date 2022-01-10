use crate::{RetPtr, dyn_reg, _recurse_expand, compile::{Inst, FloatReg}};
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};

// RDI is the ptr to input variables array
// RSI is the ptr to output array
// RDX is the length of the output array - decremented each loop
pub fn assemble(insts: Box<[Inst]>, var_count: usize) -> (dynasmrt::mmap::ExecutableBuffer, RetPtr) {
    let mut assembler = dynasmrt::x64::Assembler::new().unwrap();

    let start = assembler.offset();

    dynasm!(assembler
        ; .arch x64
        ; test rdx, rdx
        ; jle->exit
        ; ->loop_start:
    );

    // move variables into low registers
    for j in 0..var_count {
        let reg = FloatReg(j as u8);
        let offset = j as i32 * 4;
        dyn_reg!(assembler, (reg)
            ; movd reg, [rdi+offset]
        );
    }

    for inst in insts.into_iter() {
        inst.add_to_inst_stream(&mut assembler);
    }

    // move result into output array
    let low_non_var_reg = FloatReg(var_count as u8);
    dyn_reg!(assembler, (low_non_var_reg)
        ; movd eax, low_non_var_reg
    );

    dynasm!(assembler
        ; .arch x64
        ; mov [rsi], eax
        ; add rsi, 4
        ; add rdi, var_count as i32 * 4
        ; sub rdx, 1
        ; jne->loop_start
        ; ->exit:
        ; ret
    );

    let code = assembler.finalize().unwrap();

    let ptr = unsafe {
        std::mem::transmute(code.ptr(start))
    };
    (code, ptr)
}
