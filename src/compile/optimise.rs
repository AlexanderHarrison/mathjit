use crate::expr_parse::{Operation, Expression};
use super::{CompileInfo, CodeGenError};

pub struct Optimizations {
    pub precompute: bool,
}

impl Optimizations {
    #![allow(non_snake_case)]
    pub fn O3() -> Self {
        Optimizations {
            precompute: true,
        }
    }

    pub fn unoptimized() -> Self {
        Optimizations {
            precompute: false,
        }
    }
}

pub fn compile_expression_optimised<'a, 'b>(
    expr: &'a mut Expression,
    optimizations: &'b Optimizations
) -> Result<CompileInfo<'a>, CodeGenError> {
    if optimizations.precompute {
        precompute(&mut expr.operation);
    }

    super::compile_expression(expr)
}

pub fn precompute(op: &mut Operation) {
    use Operation::*;
    let new_op = match op {
        Add(ref mut terms) => {
            let ops = std::mem::take(terms);
            let (lits, mut rest) = ops.into_iter()
                .map(|mut t| { precompute(&mut t); t })
                .partition::<Vec<_>, _>(|t| matches!(t, Literal(_)));

            let lit = Literal(lits.into_iter()
                                      .map(|t| if let Literal(n) = t { n } else { unreachable!() } )
                                      .sum());

            if rest.len() != 0 {
                if lit != Literal(0.0) {
                    rest.push(lit);
                }
                *terms = rest;
                None
            } else {
                Some(lit)
            }
        }
        Mul(ref mut terms) => {
            let ops = std::mem::take(terms);
            let (lits, mut rest) = ops.into_iter()
                .map(|mut t| { precompute(&mut t); t })
                .partition::<Vec<_>, _>(|t| matches!(t, Literal(_)));

            let lit = Literal(lits.into_iter()
                                  .map(|t| if let Literal(n) = t { n } else { unreachable!() } )
                                  .fold(1.0, <f32 as std::ops::Mul>::mul));

            if rest.len() != 0 {
                if lit != Literal(1.0) {
                    rest.push(lit);
                }
                *terms = rest;
                None
            } else {
                Some(lit)
            }
        }
        Neg(op) => { precompute(op); if let Literal(n) = **op { Some(Literal(-n)) } else { None }},
        Sqrt(op) => { precompute(op); if let Literal(n) = **op { Some(Literal(n.sqrt())) } else { None }},
        Ln(op) => { precompute(op); if let Literal(n) = **op { Some(Literal(n.ln())) } else { None }},
        Exp(op) => { precompute(op); if let Literal(n) = **op { Some(Literal(n.exp())) } else { None }},
        Div(ops) => {
            let (ref mut op_a, ref mut op_b) = **ops;
            precompute(op_a);
            precompute(op_b);
            match (op_a, op_b) {
                (Literal(n), Literal(m)) => Some(Literal(*n / *m)),
                (op_a, Literal(n)) if *n == 1.0 => Some(op_a.clone()),
                (op_a, Literal(n)) => Some(Mul(vec![op_a.clone(), Literal(n.recip())])),
                _ => None,
            }
        }
        Literal(_) | Variable(_) => None,
    };

    if let Some(n) = new_op {
        *op = n;
    }
}
