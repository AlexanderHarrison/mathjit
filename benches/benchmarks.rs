use criterion::*;
use expr_parse::{Expression, eval};
use mathjit::CompiledExpression;

fn default_criterion() -> Criterion {
    Criterion::default()
        .warm_up_time(std::time::Duration::from_millis(100))
}

criterion_main!(compile_assemble);

criterion_group! {
    name = baseline_eval;
    config = default_criterion();
    targets = baseline_eval_bench
}

criterion_group! {
    name = compile_assemble;
    config = default_criterion();
    targets = compile_assemble_bench
}

criterion_group! {
    name = sse_vs_avx;
    config = default_criterion();
    targets = sse_vs_avx_bench
}

#[allow(dead_code)]
pub fn compile_assemble_bench(c: &mut Criterion) {
    let expr_string = black_box("-x(2x(-x+4)(1+xx-2x)-5(-6(1+x(4-2x))xx-(-2.4)xx+4.1))").parse::<Expression>().unwrap();
    c.bench_with_input(
        BenchmarkId::new("compile and assemble", ""),
        &expr_string,
        |b, expr| b.iter(|| CompiledExpression::new(expr)),
    );

    c.bench_with_input(
        BenchmarkId::new("compile", ""),
        &expr_string,
        |b, expr| b.iter(|| mathjit::compile::compile_expression(expr)),
    );

    let compiled = mathjit::compile::compile_expression(&expr_string).unwrap();
    c.bench_with_input(
        BenchmarkId::new("assemble start", ""),
        &compiled,
        |b, c| b.iter(|| {
            mathjit::assemble::assemble(&c)
        }),
    );

    c.bench_with_input(
        BenchmarkId::new("assemble total", ""),
        &compiled,
        |b, c| b.iter(|| mathjit::assemble::assemble(&c)),
    );
}

#[allow(dead_code)]
pub fn baseline_eval_bench(c: &mut Criterion) {
    let expr_string = black_box("2x+5");
    let expr: Expression = expr_string.parse().unwrap();

    let vals = [];
    c.bench_with_input(
        BenchmarkId::new("baseline regular", expr_string), 
        &(&expr, &vals), 
        |b, (e, v)| b.iter(
            || v.iter().map(|n| eval(e, &[black_box(*n)])).sum::<f32>()
        )
    );

    let expr_compiled = CompiledExpression::new(&expr).unwrap();
    c.bench_with_input(
        BenchmarkId::new("baseline compiled", expr_string), 
        &(&expr_compiled, &vals), 
        |b, (e, v)| b.iter(
            || e.eval(black_box(*v)).iter().sum::<f32>()
        )
    );
}

#[allow(dead_code)]
pub fn simple_single_var_eval_bench(c: &mut Criterion) {
    let expr_string = black_box("2x+5");
    let expr: Expression = expr_string.parse().unwrap();

    let vals = std::iter::successors(Some(0.0), |n| Some(n + 1.0)).take(256).collect::<Vec<f32>>();
    c.bench_with_input(
        BenchmarkId::new("simple eval regular", expr_string), 
        &(&expr, &vals), 
        |b, (e, v)| b.iter(
            || v.iter().map(|n| eval(e, &[black_box(*n)])).sum::<f32>()
        )
    );

    let expr_compiled = CompiledExpression::new(&expr).unwrap();
    c.bench_with_input(
        BenchmarkId::new("simple eval compiled", expr_string), 
        &(&expr_compiled, &vals), 
        |b, (e, v)| b.iter(
            || e.eval(black_box(*v)).iter().sum::<f32>()
        )
    );
}

#[allow(dead_code)]
pub fn complex_single_var_eval_bench(c: &mut Criterion) {
    let expr_string = black_box("-x(2x(-x+4)(1+xx-2x)-5(-6(1+x(4-2x))xx-(-2.4)xx+4.1))");
    let expr: Expression = expr_string.parse().unwrap();

    let vals = std::iter::successors(Some(0.0), |n| Some(n + 1.0)).take(256).collect::<Vec<f32>>();
    //c.bench_with_input(
    //    BenchmarkId::new("simple eval regular", expr_string), 
    //    &(&expr, &vals), 
    //    |b, (e, v)| b.iter(
    //        || v.iter().map(|n| expr_parse::eval(e, &[Variable('x')], &[black_box(*n)])).sum::<f32>()
    //    )
    //);

    let expr_compiled = CompiledExpression::new(&expr).unwrap();
    c.bench_with_input(
        BenchmarkId::new("complex eval compiled", expr_string), 
        &mut (&expr_compiled, &vals), 
        |b, (e, v)| b.iter(
            || {e.eval(black_box(*v)).iter().sum::<f32>() }
        )
    );
}

#[allow(dead_code)]
pub fn complex_multi_var_eval_bench(c: &mut Criterion) {
    let expr_string = black_box("-xyzz(2xz(-xy+4)z(1+yx-2y)-5z(-6(1+y(4y-2z)y)xz-(-2.4)yx+4.1))");
    let expr: Expression = expr_string.parse().unwrap();

    let vals = std::iter::successors(Some(0.0), |n| Some(n + 1.0)).take(999).collect::<Vec<f32>>();
    c.bench_with_input(
        BenchmarkId::new("simple eval regular", expr_string), 
        &(&expr, &vals), 
        |b, (e, v)| b.iter(
            || v.chunks_exact(e.variables.len()).map(|n| eval(e, black_box(n))).sum::<f32>()
        )
    );

    let expr_compiled = CompiledExpression::new(&expr).unwrap();
    c.bench_with_input(
        BenchmarkId::new("simple eval compiled", expr_string), 
        &(&expr_compiled, &vals), 
        |b, (e, v)| b.iter(
            || e.eval(black_box(*v)).iter().sum::<f32>()
        )
    );
}

#[allow(dead_code)]
pub fn sse_vs_avx_bench(c: &mut Criterion) {
    let simple = "x".parse::<Expression>().unwrap();
    let complex = "x+2(x+5xx+8(6x-2xxx+4)-0.1(x+2xx)(x-3))-3xx".parse::<Expression>().unwrap();
    let (simple_comp_sse, complex_comp_sse, simple_comp_avx, complex_comp_avx) = unsafe {
        let simple_comp_sse = CompiledExpression::with_force_feature(&simple, Some(false)).unwrap();
        let complex_comp_sse = CompiledExpression::with_force_feature(&complex, Some(false)).unwrap();
        let simple_comp_avx = CompiledExpression::with_force_feature(&simple, Some(true)).unwrap();
        let complex_comp_avx = CompiledExpression::with_force_feature(&complex, Some(true)).unwrap();
        (simple_comp_sse, complex_comp_sse, simple_comp_avx, complex_comp_avx)
    };

    let vals = std::iter::successors(Some(0.0), |n| Some(n + 1.0)).take(4096).collect::<Vec<f32>>();

    c.bench_with_input(
        BenchmarkId::new("sse simple expr", ""), 
        &(&simple_comp_sse, &vals), 
        |b, (e, v)| b.iter(
            || e.eval(black_box(*v)).iter().sum::<f32>()
        )
    );

    c.bench_with_input(
        BenchmarkId::new("sse complex expr", ""), 
        &(&complex_comp_sse, &vals), 
        |b, (e, v)| b.iter(
            || e.eval(black_box(*v)).iter().sum::<f32>()
        )
    );

    c.bench_with_input(
        BenchmarkId::new("avx simple expr", ""), 
        &(&simple_comp_avx, &vals), 
        |b, (e, v)| b.iter(
            || e.eval(black_box(*v)).iter().sum::<f32>()
        )
    );

    c.bench_with_input(
        BenchmarkId::new("avx complex expr", ""), 
        &(&complex_comp_avx, &vals), 
        |b, (e, v)| b.iter(
            || e.eval(black_box(*v)).iter().sum::<f32>()
        )
    );
}
