use super::{Expression, Variable, Operation};

pub fn blanket_eval(
    expression: &Expression,
    ranges: &[std::ops::Range<f32>],
    resolution: usize,
) -> Box<[f32]> {
    let resolution_f32 = resolution as f32;
    assert_eq!(expression.variables.len(), ranges.len());
    let len = ranges.len();
    let data_count = resolution.pow(len as u32);
    let mut data = Vec::with_capacity(data_count);
    let deltas = ranges.clone().iter().map(|r| (r.end - r.start) / resolution_f32).collect::<Vec<f32>>();
    let mut values = ranges.clone().iter().map(|r| r.start).collect::<Vec<f32>>();

    for _ in 0..data_count {
        data.push(eval_operation(&expression.operation, &expression.variables, &values));
        for j in 0..len {
            values[j] += deltas[j];
            if values[j] >= ranges[j].end {
                values[j] = ranges[j].start;
            }
        }
    }

    data.into_boxed_slice()
}

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
pub fn eval(expression: &Expression, variables: &[f32]) -> Box<[f32]> {
    assert!(variables.len() % expression.variables.len() == 0);
    variables
        .chunks_exact(expression.variables.len())
        .map(|values| eval_operation(&expression.operation, &expression.variables, values))
        .collect::<Vec<f32>>()
        .into_boxed_slice()
}

fn eval_operation(operation: &Operation, vars: &[Variable], values: &[f32]) -> f32 {
    use std::ops::Mul;
    match operation {
        Operation::Literal(n) => *n,
        Operation::Variable(var) => values[vars.iter().position(|p| *p == *var).unwrap()],
        Operation::Add(ops) => ops.iter().map(|op| eval_operation(op, vars, values)).sum(),
        Operation::Mul(ops) => ops.iter().map(|op| eval_operation(op, vars, values)).fold(1.0, f32::mul),
        Operation::Div(ops) => { 
            let (dividend, divisor) = &**ops; 
            eval_operation(&dividend, vars, values) / eval_operation(&divisor, vars, values)
        }
        Operation::Neg(op) => -eval_operation(op, vars, values),
        Operation::Sqrt(op) => eval_operation(op, vars, values).sqrt(),
        Operation::Ln(op) => eval_operation(op, vars, values).ln(),
        Operation::Exp(op) => eval_operation(op, vars, values).exp(),
    }
}
