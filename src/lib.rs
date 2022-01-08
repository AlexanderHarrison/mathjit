use eq_parse::Equation;

mod compile;
mod assemble;

pub use eq_parse;

pub type RetPtr = extern "C" fn(f32) -> f32;
//pub type RetPtr = unsafe extern "C" fn(input: *const f32, output: *mut f32, len: usize);

pub struct CompiledEquation {
    pub raw_fn: RetPtr,
    _buf: dynasmrt::mmap::ExecutableBuffer,
}
 
impl CompiledEquation {
    pub fn new(eq: &Equation) -> Self {

        println!("{:?}", eq);
        let mut high_level_instructions = Vec::new();
        compile::compile_operation(&eq.operation, &mut high_level_instructions);
        println!("{:#?}", high_level_instructions);

        let (buf, raw_fn) = assemble::assemble(high_level_instructions);

        Self {
            raw_fn,
            _buf: buf
        }
    }

    pub fn temp_eval(&self, x: f32) -> f32 {
        println!("evaluating");
        (self.raw_fn)(x)
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
