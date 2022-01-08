use eq_parse::Variable;

fn main() {
    let mut eq = eq_parse::parse_equation("2.0x").unwrap();
    //eq.set_variable_order(vec![Variable('x'), Variable('y'), Variable('z')]).unwrap();
    eq.set_variable_order(vec![Variable('x')]).unwrap();
    let compiled = mathjit::CompiledEquation::new(&eq);
    println!("{:?}", compiled.temp_eval(&[1.0, 2.0, 3.0, 4.0, 5.0, 7.0]));
}
