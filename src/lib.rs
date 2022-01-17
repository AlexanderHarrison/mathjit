use expr_parse::Expression;

pub mod compile;
pub mod assemble;

pub use expr_parse;

pub type RetPtr = unsafe extern "C" fn(input: *const f32, output: *mut f32, output_len: u64);

pub struct CompiledExpression {
    pub expr: Expression,
    pub raw_fn: RetPtr,
    _buf: assemble::FuncBuffer,
}
 
impl CompiledExpression {
    pub fn new(expr: &Expression) -> Self {
        let compile_info = compile::compile_expression(expr);

        let (raw_fn, buf) = assemble::assemble(&compile_info);

        Self {
            expr: expr.clone(),
            raw_fn,
            _buf: buf
        }
    }

    /// Autodetect if has_avx is None
    pub unsafe fn with_force_feature(expr: &Expression, has_avx: Option<bool>) -> Self {
        let compile_info = compile::compile_expression(expr);

        let (raw_fn, buf) = assemble::assemble_with_force_feature(&compile_info, has_avx);

        Self {
            expr: expr.clone(),
            raw_fn,
            _buf: buf
        }
    }

    pub fn eval(&self, vars: &[f32]) -> Box<[f32]> {
        let var_count = self.expr.variables.len();
        assert!(vars.len() % var_count == 0);
        let out_count = vars.len() / var_count;
        let mut output = vec![0.0; out_count].into_boxed_slice();
        unsafe { 
            (self.raw_fn)(vars.as_ptr(), output.as_mut_ptr(), out_count as u64);
        };
        output
    }

    pub fn eval_with_out_buf(&self, vars: &[f32], out_buf: &mut [f32]) {
        let var_count = self.expr.variables.len();
        let out_count = vars.len() / var_count;
        assert!(vars.len() % var_count == 0);
        assert!(out_buf.len() == out_count);
        unsafe { 
            (self.raw_fn)(vars.as_ptr(), out_buf.as_mut_ptr(), out_count as u64);
        };
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
