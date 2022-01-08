use eq_parse::Equation;

mod compile;
mod assemble;

pub use eq_parse;

pub type RetPtr = unsafe extern "C" fn(input: *const f32, output: *mut f32, len: u64);
//pub type RetPtr = unsafe extern "C" fn(input: *const f32, output: *mut f32, len: usize);

pub struct CompiledEquation {
    pub eq: Equation,
    pub raw_fn: RetPtr,
    _buf: dynasmrt::mmap::ExecutableBuffer,
}
 
impl CompiledEquation {
    pub fn new(eq: &Equation) -> Self {
        let high_level_instructions = compile::compile_equation(eq);

        let (buf, raw_fn) = assemble::assemble(high_level_instructions, eq.variables.len());

        Self {
            eq: eq.clone(),
            raw_fn,
            _buf: buf
        }
    }

    pub fn temp_eval(&self, vars: &[f32]) -> Box<[f32]> {
        let var_count = self.eq.variables.len();
        assert!(vars.len() % var_count == 0);
        let out_count = vars.len() / var_count;
        let mut output = vec![0.0; out_count].into_boxed_slice();
        unsafe { 
            (self.raw_fn)(vars.as_ptr(), output.as_mut_ptr(), out_count as u64);
        };
        output
    }

    //pub fn evaluate(&self, data: &[f32]) -> Box<[f32]> {
    //    let mut out_data = vec![0.0; data.len()].into_boxed_slice();
    //    self.evaluate_with_out(data, &mut out_data);
    //    out_data
    //}

    //pub fn evaluate_with_out(&self, data: &[f32], out_data: &mut [f32]) {
    //    let input = data.as_ptr();
    //    let output = out_data.as_mut_ptr();
    //    let len = data.len();

    //    //let ret_len = len / var_count always 1 for now.
    //    unsafe {
    //        (self.raw_fn)(input, output, len);
    //    }
    //}
}
