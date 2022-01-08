fn main() {
    let eq = eq_parse::parse_equation("2xx").unwrap();
    let compiled = mathjit::CompiledEquation::new(&eq);
    println!("{:?}", compiled.temp_eval(2.4));
}
