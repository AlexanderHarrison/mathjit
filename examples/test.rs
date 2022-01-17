use mathjit::{CompiledExpression, expr_parse::Expression};

fn main() {
    let expr_string = "-x*(2*x*(-x+4)*(1+x*x-2*x)-5*(-6*(1+x*(4-2*x))*x*x-(-2.4)*x*x+4.1))".parse::<Expression>().unwrap();
    for _ in 0..100000 {
        criterion::black_box(CompiledExpression::new(&expr_string));
    }
}
