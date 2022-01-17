use crate::{RetPtr, compile::{FloatReg, CompileInfo}};
use iced_x86::code_asm::{CodeLabel, CodeAssembler, IcedError, AsmRegister64};

const PRINT_ASM: bool = false;

pub struct FuncBuffer {
    _mmap: memmap2::Mmap
}

pub enum Labels {
    SSE(LabelsSSE),
    AVX(LabelsAVX),
}

impl Labels {
    pub fn data_len(self) -> usize {
        match self {
            Labels::SSE(sse) => sse.data_len(),
            Labels::AVX(avx) => avx.data_len(),
        }
    }
}

#[derive(Clone, Debug)]
// Requires pre-broadcasted values
pub struct LabelsSSE {
    pub expression_literals: Vec<CodeLabel>,
    pub negation_literal: CodeLabel,
}

#[derive(Clone, Debug)]
// Does not require pre-broadcasted values
pub struct LabelsAVX {
    pub expression_literals: Vec<CodeLabel>,
    pub negation_literal: CodeLabel,
}

impl LabelsSSE {
    pub fn new(assembler: &mut CodeAssembler, literals: &[f32]) -> Result<Self, IcedError> {
        let mut expression_literals = literals.iter()
            .map(|_| assembler.create_label())
            .collect::<Vec<CodeLabel>>();
        let mut negation_literal = assembler.create_label();

        // Since alignment isn't yet supported in iced, we need to initiate
        // the mandatory aligned data first (assuming the initial mem pointer is aligned).
        assert!(assembler.instructions().len() == 0);
        assembler.set_label(&mut negation_literal)?;
        assembler.dd_f32(&[-0.0, -0.0, -0.0, -0.0])?;

        for (lit, label) in literals.iter().zip(expression_literals.iter_mut()) {
            assembler.set_label(label)?;
            assembler.dd_f32(&[*lit; 4])?;
        }

        Ok(Self {
            expression_literals,
            negation_literal,
        })
    }

    pub fn data_len(&self) -> usize {
        (self.expression_literals.len() + 1) * 4 * 4
    }
}

impl LabelsAVX {
    pub fn new(assembler: &mut CodeAssembler, literals: &[f32]) -> Result<Self, IcedError> {
        let mut expression_literals = literals.iter()
            .map(|_| assembler.create_label())
            .collect::<Vec<CodeLabel>>();
        let mut negation_literal = assembler.create_label();

        // Since alignment isn't yet supported in iced, we need to initiate
        // the mandatory aligned data first (assuming the initial mem pointer is aligned).
        assert!(assembler.instructions().len() == 0);
        assembler.set_label(&mut negation_literal)?;
        assembler.dd_f32(&[-0.0, 0.0])?;

        for (lit, label) in literals.iter().zip(expression_literals.iter_mut()) {
            assembler.set_label(label)?;
            assembler.dd_f32(&[*lit, 0.0])?;
        }

        Ok(Self {
            expression_literals,
            negation_literal,
        })
    }

    pub fn data_len(&self) -> usize {
        (self.expression_literals.len() + 1) * 4 * 2
    }
}

/// Only uses SSE for now. AVX is slightly slower for some reason.
pub fn assemble(compile_info: &CompileInfo) -> (RetPtr, FuncBuffer) {
    unsafe { assemble_with_force_feature(compile_info, Some(false)) }
}

// Autodetect if has_avx is None
pub unsafe fn assemble_with_force_feature(compile_info: &CompileInfo, has_avx: Option<bool>) -> (RetPtr, FuncBuffer) {
    const BUFFER_SIZE: usize = 2048;

    let mut assembler = CodeAssembler::new(64).unwrap();
    let mut labels = if has_avx.unwrap_or_else(|| is_x86_feature_detected!("avx")) {
        Labels::AVX(LabelsAVX::new(&mut assembler, &compile_info.literals).unwrap())
    } else {
        Labels::SSE(LabelsSSE::new(&mut assembler, &compile_info.literals).unwrap())
    };

    assemble_eval(&mut assembler, &mut labels, &compile_info).unwrap();

    if PRINT_ASM {
        use iced_x86::Formatter;
        let mut formatter = iced_x86::NasmFormatter::new();
        for inst in assembler.instructions() {
            let mut s = String::new();
            formatter.format(inst, &mut s);
            println!("{}", s);
        }
    }

    let mut mmap = memmap2::MmapMut::map_anon(BUFFER_SIZE).unwrap();
    let instruction_ptr = mmap.as_ptr() as u64 + labels.data_len() as u64;

    let code_bytes = assembler.assemble(instruction_ptr).unwrap();
    mmap[..code_bytes.len()].copy_from_slice(&code_bytes);

    let mmap = mmap.make_exec().unwrap();

    let ptr = std::mem::transmute::<_, RetPtr>(instruction_ptr);
    (ptr, FuncBuffer { _mmap: mmap })
}

// RDI is the ptr to input variables array
// RSI is the ptr to output array
// RDX is the length of the output array
// RAX is the length of the input array divided by 4 * var_count (times to execute sse loop)
// RCX is the remainder of the above division (times to execute single float loop)
pub fn assemble_eval(
    assembler: &mut CodeAssembler,
    labels: &mut Labels,
    compile_info: &CompileInfo
) -> Result<(), IcedError> {
    use iced_x86::code_asm::registers::gpr64::*;
    let var_count = compile_info.vars.len();

    let input_ptr_reg = rdi;
    let output_ptr_reg = rsi;
    let output_length_reg = rdx;
    let times_simd_reg = rax;
    let times_single_reg = rcx;

    let has_avx = matches!(labels, Labels::AVX(_));
    let floats_log2 = if has_avx { 3 } else { 2 };

    assembler.mov(times_simd_reg, output_length_reg)?;
    if var_count == 0 {
        todo!();
    } else {
        assembler.mov(times_single_reg, output_length_reg)?;
        assembler.shr(times_simd_reg, floats_log2)?;
        assembler.and(times_single_reg, ((1 << floats_log2) - 1) as i32)?;
    }

    //if FORCE_SINGLE {
    //    assemble_eval_loop_single(
    //        assembler, &compile_info, &labels,
    //        output_length_reg, output_ptr_reg, input_ptr_reg,
    //    )?;
    //} else {
        match labels {
            Labels::AVX(labels) => assemble_eval_loop_avx(
                assembler, &compile_info, &labels,
                times_simd_reg, output_ptr_reg, input_ptr_reg,
            )?,
            Labels::SSE(labels) => assemble_eval_loop_sse(
                assembler, &compile_info, &labels,
                times_simd_reg, output_ptr_reg, input_ptr_reg,
            )?,
        }
        assemble_eval_loop_single(
            assembler, &compile_info, &labels,
            times_single_reg, output_ptr_reg, input_ptr_reg,
        )?;
    //}

    assembler.ret()?;

    Ok(())
}

fn assemble_eval_loop_sse(
    assembler: &mut CodeAssembler,
    compile_info: &CompileInfo,
    labels: &LabelsSSE,
    times_sse_reg: AsmRegister64,
    output_ptr_reg: AsmRegister64,
    input_ptr_reg: AsmRegister64,
) -> Result<(), IcedError> {
    use iced_x86::code_asm::xmmword_ptr;

    let var_count = compile_info.vars.len();

    assembler.test(times_sse_reg, times_sse_reg)?;
    let mut exit_sse_label = assembler.create_label();
    assembler.jle(exit_sse_label)?;
    let mut sse_loop_start_label = assembler.create_label();
    assembler.set_label(&mut sse_loop_start_label)?;

    if var_count == 1 {
        assembler.movups(FloatReg(0).to_native_reg(), xmmword_ptr(input_ptr_reg))?;
    } else if var_count != 0 {
        // Move packed variables into higher float registers - implementation limits max vars to 7
        for i in 0..var_count {
            let offset = i as i32 * 4 * 4;
            let reg = FloatReg(i as u8 + var_count as u8);
            assembler.movups(reg.to_native_reg(), xmmword_ptr(input_ptr_reg + offset))?;
        }

        // Crazy, don't ask
        // var order is x1, y1, x2, y2, x3, y3, x4, y4, ... for any number of vars
        // But we need them in (x1 x2 x3 x4), (y1, y2, y3, y4), ... xmm registers
        let temp_reg = FloatReg(var_count as u8 * 2).to_native_reg();
        for i in 0..var_count {
            let var_reg = FloatReg(i as u8).to_native_reg();
            let b_1_r = FloatReg((i / 4 + var_count) as u8).to_native_reg();
            let b_1_i = i % 4;
            let b_2_r = FloatReg(((i + 1 * var_count) / 4 + var_count) as u8).to_native_reg();
            let b_2_i = (i + 1 * var_count) % 4;
            let b_3_r = FloatReg(((i + 2 * var_count) / 4 + var_count) as u8).to_native_reg();
            let b_3_i = (i + 2 * var_count) % 4;
            let b_4_r = FloatReg(((i + 3 * var_count) / 4 + var_count) as u8).to_native_reg();
            let b_4_i = (i + 3 * var_count) % 4;
            //println!("1:{:?} {:?} 2:{:?} {:?} 3:{:?} {:?} 4:{:?} {:?}", b_1_r, b_1_i, b_2_r, b_2_i, b_3_r, b_3_i, b_4_r, b_4_i);
            assembler.movaps(var_reg, b_1_r)?;
            assembler.movaps(temp_reg, b_3_r)?;
            assembler.shufps(var_reg, b_2_r, ((b_2_i << 4) ^ b_1_i) as u32)?; // byte 1 set in byte 1, byte 2 set in byte 3
            assembler.shufps(temp_reg, b_4_r, ((b_4_i << 4) ^ b_3_i) as u32)?; // byte 3 set in byte 1, byte 4 set in byte 3
            assembler.shufps(var_reg, temp_reg, 0b10001000)?; // all bytes set
        }
    }

    for inst in compile_info.insts.iter() {
        inst.add_to_inst_stream_ps_sse(assembler, &labels)?;
    }

    // move result into output array
    let low_non_var_reg = FloatReg(var_count as u8);
    assembler.movups(xmmword_ptr(output_ptr_reg), low_non_var_reg.to_native_reg())?;
   
    assembler.add(output_ptr_reg, 4 * 4)?;
    assembler.add(input_ptr_reg, var_count as i32 * 4 * 4)?;
    assembler.dec(times_sse_reg)?;
    assembler.jne(sse_loop_start_label)?;
    assembler.set_label(&mut exit_sse_label)?;

    Ok(())
}

fn assemble_eval_loop_avx(
    assembler: &mut CodeAssembler,
    compile_info: &CompileInfo,
    labels: &LabelsAVX,
    times_avx_reg: AsmRegister64,
    output_ptr_reg: AsmRegister64,
    input_ptr_reg: AsmRegister64,
) -> Result<(), IcedError> {
    use iced_x86::code_asm::ymmword_ptr;

    let var_count = compile_info.vars.len();

    assembler.test(times_avx_reg, times_avx_reg)?;
    let mut exit_avx_label = assembler.create_label();
    assembler.jle(exit_avx_label)?;
    let mut avx_loop_start_label = assembler.create_label();
    assembler.set_label(&mut avx_loop_start_label)?;

    // TODO - optimization for 2 var
    if var_count == 1 {
        assembler.vmovups(FloatReg(0).to_avx_reg(), ymmword_ptr(input_ptr_reg))?;
    } else if var_count != 0 {
        // Crazy, don't ask
        // var order is x1, y1, x2, y2, x3, y3, x4, y4, ... for any number of vars
        // But we need them in (x1 x2 ... x8), (y1 y2 ... y8), ... ymm registers
        for i in 0..var_count {
            let offset = |n| ((n * var_count) + i) * 4;
            use iced_x86::code_asm::dword_ptr;
            let var_ymm = FloatReg(i as u8).to_avx_reg();
            let temp_ymm_1 = FloatReg(i as u8 + 1).to_avx_reg();
            let temp_ymm_2 = FloatReg(i as u8 + 2).to_avx_reg();
            let temp_ymm_3 = FloatReg(i as u8 + 3).to_avx_reg();

            let var_xmm = FloatReg(i as u8).to_native_reg();
            let temp_xmm_1 = FloatReg(i as u8 + 1).to_native_reg();
            let temp_xmm_2 = FloatReg(i as u8 + 2).to_native_reg();
            let temp_xmm_3 = FloatReg(i as u8 + 3).to_native_reg();
            let temp_xmm_4 = FloatReg(i as u8 + 4).to_native_reg();
            let temp_xmm_5 = FloatReg(i as u8 + 5).to_native_reg();
            let temp_xmm_6 = FloatReg(i as u8 + 6).to_native_reg();
            let temp_xmm_7 = FloatReg(i as u8 + 7).to_native_reg();
            
            assembler.vmovd(var_xmm, dword_ptr(input_ptr_reg + offset(0)))?;       // b1
            assembler.vmovd(temp_xmm_1, dword_ptr(input_ptr_reg + offset(1)))?;    // b2
            assembler.vmovd(temp_xmm_2, dword_ptr(input_ptr_reg + offset(2)))?;    // b3
            assembler.vmovd(temp_xmm_3, dword_ptr(input_ptr_reg + offset(3)))?;    // b4
            assembler.vmovd(temp_xmm_4, dword_ptr(input_ptr_reg + offset(4)))?;    // b5
            assembler.vmovd(temp_xmm_5, dword_ptr(input_ptr_reg + offset(5)))?;    // b6
            assembler.vmovd(temp_xmm_6, dword_ptr(input_ptr_reg + offset(6)))?;    // b7
            assembler.vmovd(temp_xmm_7, dword_ptr(input_ptr_reg + offset(7)))?;    // b8

            assembler.vinsertf128(var_ymm, var_ymm, temp_xmm_4, 1)?;        // x x x b5 x x x b1
            assembler.vinsertf128(temp_ymm_1, temp_ymm_1, temp_xmm_5, 1)?;  // x x x b6 x x x b2
            assembler.vinsertf128(temp_ymm_2, temp_ymm_2, temp_xmm_6, 1)?;  // x x x b7 x x x b3
            assembler.vinsertf128(temp_ymm_3, temp_ymm_3, temp_xmm_7, 1)?;  // x x x b8 x x x b4
            
            assembler.vunpcklps(var_ymm, var_ymm, temp_ymm_1)?; // x x b6 b5 x x b2 b1
            assembler.vunpcklps(temp_ymm_2, temp_ymm_2, temp_ymm_3)?; // x x b8 b7 x x b4 b3

            assembler.vunpcklpd(var_ymm, var_ymm, temp_ymm_2)?; // b8 b7 b6 b5 b4 b3 b2 b1
        }
    }

    for inst in compile_info.insts.iter() {
        inst.add_to_inst_stream_ps_avx(assembler, &labels)?;
    }

    // move result into output array
    let low_non_var_reg = FloatReg(var_count as u8);
    assembler.vmovups(ymmword_ptr(output_ptr_reg), low_non_var_reg.to_avx_reg())?;
   
    assembler.add(output_ptr_reg, 4 * 8)?;
    assembler.add(input_ptr_reg, var_count as i32 * 4 * 8)?;
    assembler.dec(times_avx_reg)?;
    assembler.jne(avx_loop_start_label)?;
    assembler.set_label(&mut exit_avx_label)?;

    Ok(())
}

fn assemble_eval_loop_single(
    assembler: &mut CodeAssembler,
    compile_info: &CompileInfo,
    labels: &Labels,
    times_single_reg: AsmRegister64,
    output_ptr_reg: AsmRegister64,
    input_ptr_reg: AsmRegister64,
) -> Result<(), IcedError> {
    use iced_x86::code_asm::dword_ptr;

    let var_count = compile_info.vars.len();

    assembler.test(times_single_reg, times_single_reg)?;
    let mut exit_single_label = assembler.create_label();
    assembler.jle(exit_single_label)?;
    let mut single_loop_start_label = assembler.create_label();
    assembler.set_label(&mut single_loop_start_label)?;

    let has_axv = matches!(labels, Labels::AVX(_));

    // move variables into low registers for single
    for j in 0..var_count {
        let reg = FloatReg(j as u8);
        let offset = j as i32 * 4;

        if has_axv {
            assembler.vmovd(reg.to_native_reg(), dword_ptr(input_ptr_reg) + offset)?;
        } else {
            assembler.movd(reg.to_native_reg(), dword_ptr(input_ptr_reg) + offset)?;
        }
    }


    match labels {
        Labels::AVX(labels) => {
            for inst in compile_info.insts.iter() {
                inst.add_to_inst_stream_single_avx(assembler, &labels)?;
            }
        }
        Labels::SSE(labels) => {
            for inst in compile_info.insts.iter() {
                inst.add_to_inst_stream_single_sse(assembler, &labels)?;
            }
        }
    }

    // move result into output array
    let low_non_var_reg = FloatReg(var_count as u8);
    if has_axv {
        assembler.vmovd(dword_ptr(output_ptr_reg), low_non_var_reg.to_native_reg())?;
    } else {
        assembler.movd(dword_ptr(output_ptr_reg), low_non_var_reg.to_native_reg())?;
    }

    assembler.add(output_ptr_reg, 4)?;
    assembler.add(input_ptr_reg, var_count as i32 * 4)?;
    assembler.dec(times_single_reg)?;
    assembler.jnz(single_loop_start_label)?;
    assembler.set_label(&mut exit_single_label)?;
    
    Ok(())
}
