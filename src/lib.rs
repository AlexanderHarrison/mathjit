use expr_parse::Expression;

pub mod compile;
pub mod assemble;
pub mod expr_parse;

pub type ExprFnPtr = unsafe extern "C" fn(input: *const f32, output: *mut f32, output_len: u64);

/// A safe wrapper for creating and running compiled expressions.
///
/// # Examples
///
/// ```
/// use mathjit::expr_parse::{Variable, Expression};
/// use mathjit::CompiledExpression;
///
/// // Single variable example
/// let expression = "2*x+6".parse::<Expression>().unwrap();
/// let compiled_expr = CompiledExpression::new(&expression).unwrap();
/// let inputs = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
/// let output = compiled_expr.eval(&inputs);
/// assert_eq!(*output, [8.0, 10.0, 12.0, 14.0, 16.0, 18.0]);
///
/// // Multi variable example
/// let mut multi_var_expr = "3*x+2*y".parse::<Expression>().unwrap();
/// multi_var_expr.set_variable_order(vec![Variable::new("x"), Variable::new("y")]);
/// let compiled_multi_var_expr = CompiledExpression::new(&multi_var_expr).unwrap();
/// let inputs = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
/// let output = compiled_multi_var_expr.eval(&inputs);
/// assert_eq!(*output, [7.0, 17.0, 27.0]);
/// ```
pub struct CompiledExpression {
    pub expr: Expression,
    pub raw_fn: ExprFnPtr,
    _buf: assemble::FuncBuffer,
}

/// Joint enum for assembler errors and code generation errors.
#[derive(Debug, Clone)]
pub enum CompileError {
    CodeGenError(compile::CodeGenError),
    AssembleError(assemble::AssembleError),
}

impl From<compile::CodeGenError> for CompileError {
    fn from(e: compile::CodeGenError) -> Self {
        CompileError::CodeGenError(e)
    }
}

impl From<assemble::AssembleError> for CompileError {
    fn from(e: assemble::AssembleError) -> Self {
        CompileError::AssembleError(e)
    }
}
 
impl CompiledExpression {
    /// Takes a parsed expression and compiles it.
    ///
    /// Always compiles with SSE vectorized instructions enabled.
    /// AVX instructions are measurably slower as the current implementation stands,
    /// and I do not know why.
    pub fn new(expr: &Expression) -> Result<Self, CompileError> {
        let compile_info = compile::compile_expression(expr)?;

        let (raw_fn, buf) = assemble::assemble(&compile_info)?;

        Ok(Self {
            expr: expr.clone(),
            raw_fn,
            _buf: buf
        })
    }

    /// Takes a parsed expression and compiles it.
    ///
    /// Autodetect features if None is passed for `has_avx`
    pub unsafe fn with_force_feature(expr: &Expression, has_avx: Option<bool>) -> Result<Self, CompileError> {
        let compile_info = compile::compile_expression(expr)?;

        let (raw_fn, buf) = assemble::assemble_with_force_feature(&compile_info, has_avx)?;

        Ok(Self {
            expr: expr.clone(),
            raw_fn,
            _buf: buf
        })
    }

    /// Evaluate a slice of inputs.
    /// For an expression of n vars, each chunk of size n in the input buffer corresponds 
    /// to one value in the output buffer.
    ///
    /// # Panics
    ///
    /// Panics if the input buffer length is not a multiple of the expression's variable count.
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

    /// Evaluate a slice of inputs.
    /// For an expression of n vars, each chunk of size n in the input buffer corresponds 
    /// to one value in the output buffer.
    ///
    /// # Panics
    ///
    /// Panics if the input buffer length is not a multiple of the expression's number of variables,
    /// or if the output buffer is not the proper length (input buf len / number of variables).
    pub fn eval_with_out_buf(&self, vars: &[f32], out_buf: &mut [f32]) {
        let var_count = self.expr.variables.len();
        let out_count = vars.len() / var_count;
        assert!(vars.len() % var_count == 0);
        assert!(out_buf.len() == out_count);
        unsafe { 
            (self.raw_fn)(vars.as_ptr(), out_buf.as_mut_ptr(), out_count as u64);
        };
    }
}
