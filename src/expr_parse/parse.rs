use super::*;
use meval::tokenizer::Token;
use meval::tokenizer::Operation as MevalOp;

pub type ParseError = ();

pub fn parse_expression(s: &str) -> Result<Expression, ParseError> {
    let tokens = meval::tokenizer::tokenize(s).map_err(|_| ())?;
    let rpn_tokens = meval::shunting_yard::to_rpn(&tokens).map_err(|_| ())?;
    let variables = find_vars(&rpn_tokens);
    let operation = rpn_to_expression(&rpn_tokens);

    Ok(Expression {
        operation,
        variables,
    })
}

fn rpn_to_expression(tokens: &[Token]) -> Operation {
    let mut stack = Vec::new();

    for token in tokens {
        match token {
            Token::Number(n) => stack.push(Operation::Literal(*n as f32)),
            Token::Var(var) => stack.push(Operation::Variable(Variable::new(var))),
            Token::Unary(MevalOp::Plus) => (),
            Token::Unary(MevalOp::Minus) => {
                let negated = stack.pop().unwrap();
                stack.push(Operation::Neg(Box::new(negated)));
            }
            Token::Func(f, Some(1)) if f == "sqrt" => {
                let a = stack.pop().unwrap();
                stack.push(Operation::Sqrt(Box::new(a)));
            }
            Token::Func(f, Some(1)) if f == "ln" => {
                let a = stack.pop().unwrap();
                stack.push(Operation::Ln(Box::new(a)));
            }
            Token::Func(f, Some(1)) if f == "exp" => {
                let a = stack.pop().unwrap();
                stack.push(Operation::Exp(Box::new(a)));
            }
            Token::Binary(MevalOp::Pow) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                match (a, b) {
                    (Operation::Mul(mut ops_a), Operation::Mul(ops_b)) => {
                        ops_a.extend_from_slice(&ops_b);
                        stack.push(Operation::Add(ops_a));
                    }
                    (Operation::Mul(mut ops_a), op_b) => {
                        ops_a.push(op_b);
                        stack.push(Operation::Add(ops_a));
                    }
                    (op_a, Operation::Mul(mut ops_b)) => {
                        ops_b.insert(0, op_a);
                        stack.push(Operation::Add(ops_b));
                    }
                    (op_a, op_b) => stack.push(Operation::Add(vec![op_a, op_b]))
                }
            }
            Token::Binary(MevalOp::Plus) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                match (a, b) {
                    (Operation::Add(mut ops_a), Operation::Add(ops_b)) => {
                        ops_a.extend_from_slice(&ops_b);
                        stack.push(Operation::Add(ops_a));
                    }
                    (Operation::Add(mut ops_a), op_b) => {
                        ops_a.push(op_b);
                        stack.push(Operation::Add(ops_a));
                    }
                    (op_a, Operation::Add(mut ops_b)) => {
                        ops_b.insert(0, op_a);
                        stack.push(Operation::Add(ops_b));
                    }
                    (op_a, op_b) => stack.push(Operation::Add(vec![op_a, op_b]))
                }
            }
            Token::Binary(MevalOp::Minus) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                match (a, b) {
                    (Operation::Add(mut ops_a), op_b) => {
                        ops_a.push(Operation::Neg(Box::new(op_b)));
                        stack.push(Operation::Add(ops_a));
                    }
                    (op_a, op_b) => stack.push(Operation::Add(vec![
                        op_a, 
                        Operation::Neg(Box::new(op_b))
                    ]))
                }
            }
            Token::Binary(MevalOp::Times) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                match (a, b) {
                    (Operation::Mul(mut ops_a), Operation::Mul(ops_b)) => {
                        ops_a.extend_from_slice(&ops_b);
                        stack.push(Operation::Mul(ops_a));
                    }
                    (Operation::Mul(mut ops_a), op_b) => {
                        ops_a.push(op_b);
                        stack.push(Operation::Mul(ops_a));
                    }
                    (op_a, Operation::Mul(mut ops_b)) => {
                        ops_b.insert(0, op_a);
                        stack.push(Operation::Mul(ops_b));
                    }
                    (op_a, op_b) => stack.push(Operation::Mul(vec![op_a, op_b]))
                }
            }
            Token::Binary(MevalOp::Div) => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();

                stack.push(Operation::Div(Box::new((a, b))));
            }
            _ => unimplemented!(),
        }
    }

    stack.into_iter().next().unwrap()
}

fn find_vars(tokens: &[Token]) -> Vec<Variable> {
    tokens.iter()
        .filter_map(|t| if let Token::Var(s) = t { Some(s) } else { None })
        .map(|s| Variable::new(s))
        .collect::<std::collections::HashSet<Variable>>() // remove duplicates
        .into_iter()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::Operation;
    use super::Operation::*;
    use super::parse_expression;
    use super::Variable as Var;

    fn var(s: &str) -> Operation { Variable(Var::new(s)) }

    #[test]
    fn sum() {
        assert_eq!(
            parse_expression("1+2+x-y+(5)").map(|e| e.operation),
            Ok(Add(vec![Literal(1.0), Literal(2.0), var("x"), Neg(Box::new(var("y"))), Literal(5.0)]))
        );
    }

    #[test]
    fn mul() {
        assert_eq!(
            parse_expression("1*(-2)*x*-y").map(|e| e.operation),
            Ok(Mul(vec![Literal(1.0), Neg(Box::new(Literal(2.0))), var("x"), Neg(Box::new(var("y")))]))
        );
    }

    #[test]
    fn neg() {
        assert_eq!(
            parse_expression("---y").map(|e| e.operation),
            Ok(Neg(Box::new(Neg(Box::new(Neg(Box::new(var("y")))))))),
        );
    }

    #[test]
    fn vars() {
        assert_eq!(
            parse_expression("-x*y+c*(d*r-r*y)-u").map(|e| {
                let mut v = e.variables;
                v.sort_unstable();
                v
            }),
            Ok(vec![Var::new("c"), Var::new("d"), Var::new("r"), Var::new("u"), Var::new("x"), Var::new("y")])
        );
    }
}
