fn main() {
    let eq = eq_parse::parse_equation("x + y").unwrap();
    let compiled = mathjit::CompiledEquation::new(&eq);
    println!("{:?}", compiled.temp_eval(&[1.0, 2.0]));
}
