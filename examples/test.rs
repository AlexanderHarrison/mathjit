use mathjit::expr_parse::Expression;

fn main() {
    let mut expr = "ln(x*(2*x+z+7))*(1 + sqrt(2) + y / 1.0)".parse::<Expression>().unwrap();
    println!("{}", expr.operation);
    //mathjit::compile::optimise::expand_polynomials(&mut expr.operation);
    println!("{}", expr.operation);
    mathjit::compile::optimise::precompute(&mut expr.operation);
    println!("{}", expr.operation);
}
