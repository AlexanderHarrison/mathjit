mod parse;
mod eval;
pub use parse::*;
pub use eval::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    Add(Vec<Operation>),
    Mul(Vec<Operation>),
    Neg(Box<Operation>),
    Sqrt(Box<Operation>),
    Ln(Box<Operation>),
    Exp(Box<Operation>),
    Div(Box<(Operation, Operation)>),
    Literal(f32),
    Variable(Variable)
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Variable(tinystr::TinyStrAuto);

impl Variable {
    // panics if not ascii
    pub fn new(s: &str) -> Self {
        Self(s.parse().unwrap())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub operation: Operation,
    pub variables: Vec<Variable>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SetVarOrderError {
    LengthsDoNotMatch,
    VariablesDoNotMatch,
}

impl Expression {
    pub fn set_variable_order(&mut self, new_order: Vec<Variable>) -> Result<(), SetVarOrderError> {
        if new_order.len() != self.variables.len() {
            Err(SetVarOrderError::LengthsDoNotMatch)
        } else if self.variables.iter().any(
                |v1| new_order.iter().find(|v2| *v2 == v1).is_none()) {
            Err(SetVarOrderError::VariablesDoNotMatch)
        } else {
            self.variables = new_order;
            Ok(())
        }
    }
}

impl std::str::FromStr for Expression {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_expression(s)
    }
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Operation::*;
        match self {
            Add(terms) => format_op_list(&terms, f, '+'),
            Mul(terms) => format_op_list(&terms, f, '*'),
            Neg(op) => write!(f, "-{}", op),
            Sqrt(op) => write!(f, "√({})", op),
            Ln(op) => write!(f, "ln({})", op),
            Exp(op) => write!(f, "exp({})", op),
            Div(op) => write!(f, "({}) / ({})", op.0, op.1),
            Literal(n) => write!(f, "{}", n),
            Variable(v) => write!(f, "{}", v.0),
        }
    }
}

fn format_op_list(
    ops: &[Operation],
    f: &mut std::fmt::Formatter<'_>,
    sep: char,
) -> Result<(), std::fmt::Error> {
    write!(f, "(")?;
    if ops.len() != 0 { 
        for op in ops[..(ops.len()-1)].iter() {
            write!(f, "{} {} ", op, sep)?;
        }
        write!(f, "{}", ops[ops.len()-1])?;
    }
    write!(f, ")")?;
    Ok(())
}
