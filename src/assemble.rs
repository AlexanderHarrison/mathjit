use crate::{RetPtr, compile::Inst};
use dynasmrt::{dynasm, DynasmApi};

pub fn assemble(insts: Vec<Inst>) -> (dynasmrt::mmap::ExecutableBuffer, RetPtr) {
    let mut assembler = dynasmrt::x64::Assembler::new().unwrap();

    let start = assembler.offset();

    dynasm!(assembler
        ; .arch x64
        ; movss xmm15, xmm0
    );

    for inst in insts.into_iter() {
        inst.add_to_inst_stream(&mut assembler);
    }


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
