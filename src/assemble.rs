use crate::{RetPtr, compile::{Inst, FloatReg}};

pub struct FuncBuffer {
    _mmap: memmap2::Mmap
}

// RDI is the ptr to input variables array
// RSI is the ptr to output array
// RDX is the length of the output array - decremented each loop
pub fn assemble(insts: Box<[Inst]>, var_count: usize) -> (RetPtr, FuncBuffer) {
    println!("{:?}", insts);
    const BUFFER_SIZE: usize = 2048;
    use iced_x86::code_asm::registers::{gpr32::*, gpr64::*};
    use iced_x86::code_asm::{CodeAssembler, ptr, IcedError};

    let mut assembler = CodeAssembler::new(64).unwrap();

    //let start = assembler.offset();

    fn add_instructions(assembler: &mut CodeAssembler, insts: Box<[Inst]>, var_count: usize) -> Result<(), IcedError> {
        assembler.test(rdx, rdx)?;
        let mut exit_label = assembler.create_label();
        assembler.jle(exit_label)?;
        let mut loop_start_label = assembler.create_label();
        assembler.set_label(&mut loop_start_label)?;

        // move variables into low registers
        for j in 0..var_count {
            let reg = FloatReg(j as u8);
            let offset = j as i32 * 4;

            assembler.movd(reg.to_native_reg(), ptr(rdi) + offset)?;
        }

        for inst in insts.into_iter() {
            inst.add_to_inst_stream(assembler)?;
        }

        // move result into output array
        let low_non_var_reg = FloatReg(var_count as u8);
        assembler.movd(eax, low_non_var_reg.to_native_reg())?;

        assembler.mov(ptr(rsi), eax)?;
        assembler.add(rsi, 4)?;
        assembler.add(rdi, var_count as i32 * 4)?;
        assembler.sub(rdx, 1)?;
        assembler.jne(loop_start_label)?;
        assembler.set_label(&mut exit_label)?;
        assembler.ret()?;

        Ok(())
    }

    add_instructions(&mut assembler, insts, var_count).unwrap();

    let mut mmap = memmap2::MmapMut::map_anon(BUFFER_SIZE).unwrap();
    let instruction_ptr = mmap.as_ptr() as u64;

    let code_bytes = assembler.assemble(instruction_ptr).unwrap();
    mmap[..code_bytes.len()].copy_from_slice(&code_bytes);

    let mmap = mmap.make_exec().unwrap();

    let ptr = unsafe {
        std::mem::transmute::<_, RetPtr>(instruction_ptr)
    };
    (ptr, FuncBuffer { _mmap: mmap })
}
