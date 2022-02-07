# MathJIT

This library provides runtime capability to parse and JIT compile
math expressions.

Currently only supports single-precision floating-point math on x86-64.
Supported math operations for compilation:
- Addition, subtraction, multiplication, division
- Square root
More will be added in the near future.

## Example Usage

```rust
use mathjit::expr_parse::{Variable, Expression};
use mathjit::CompiledExpression;

// Single variable example
let expression = "2*x+6".parse::<Expression>().unwrap();
let compiled_expr = CompiledExpression::new(&expression).unwrap();
let inputs = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
let output = compiled_expr.eval(&inputs);
assert_eq!(*output, [8.0, 10.0, 12.0, 14.0, 16.0, 18.0]);

// Multi variable example
let mut multi_var_expr = "3*x+2*y".parse::<Expression>().unwrap();
multi_var_expr.set_variable_order(vec![Variable::new("x"), Variable::new("y")]);
let compiled_multi_var_expr = CompiledExpression::new(&multi_var_expr).unwrap();
let inputs = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
let output = compiled_multi_var_expr.eval(&inputs);
assert_eq!(*output, [7.0, 17.0, 27.0]);
```

# Limitations
This library is a work in progress.

Todo:
- More optimizations,
- Support more architectures,
- Support double precision floating point and integer math,
- Support more mathematical functions (trig functions, exponentials, powers, etc.),
- Fix register overuse with complicated multivariable expressions,
