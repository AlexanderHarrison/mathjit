use mathjit::expr_parse::*;
use mathjit::*;

const EPSILON: f32 = 0.001;

fn check_evals_equal_nvar(eq_str: &str, n: &[f32]) {
    let eq = eq_str.parse::<Expression>().unwrap();
    assert!(
        (n.chunks_exact(eq.variables.len()).map(|s| eval(&eq, s)).sum::<f32>()
        - CompiledExpression::new(&eq).unwrap().eval(n).iter().sum::<f32>()).abs()
        < EPSILON
    );
}

macro_rules! test_evals_equal {
    ($f:ident, $s:expr, $v:expr) => {
        #[test]
        fn $f () {
            check_evals_equal_nvar($s, $v);
        }
    }
}

test_evals_equal!(t0,"x + 1", &[-1.0, 1.0, -2.0, 2.0, 123123.1, 132.0]);
test_evals_equal!(t1, "x", &[5.0, 0.0]);
test_evals_equal!(t2,"2.0*x", &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]);
test_evals_equal!(t3,"x*4", &[23423.9274; 1000]);
test_evals_equal!(t4,"2.0*x+1", &[-1.0, 1.0, -2.0, 2.0, 123123.1, 132.0]);
test_evals_equal!(t5,"x*(2*x*(-x+4)*(1+x*x-2*x)-5*(6*(1+x*(4-2*x))*x*x-2.4*x*x+4.1))", 
                    &std::iter::successors(Some(-100.0), |n| Some(n+0.41231)).take(1000).collect::<Vec<f32>>()
                  );
test_evals_equal!(t6,"x*(2*y*(-y+4)*(1+y*y-2*x)-5*y*(6*(1+x*(4-y))*x*y-2.4*y*x+4.1))", 
                    &std::iter::successors(Some(-100.0), |n| Some(n+0.41231)).take(1000).collect::<Vec<f32>>()
                  );
test_evals_equal!(t7,"x*(2*z*(-y+4)*(1+z*y-2*x)-5*y*(6*(1+x*(4-z))*z*z-2.4*y*x+4.1))", 
                    &std::iter::successors(Some(-100.0), |n| Some(n+0.41231)).take(999).collect::<Vec<f32>>()
                  );
test_evals_equal!(t8,"2*x / 5*x + 2/5*(1/x)", 
                    &std::iter::successors(Some(-100.0), |n| Some(n+0.41231)).take(999).collect::<Vec<f32>>()
                  );
test_evals_equal!(t9,"sqrt(5*sqrt(x) + 6)", 
                    &std::iter::successors(Some(0.0), |n| Some(n+0.41231)).take(999).collect::<Vec<f32>>()
                  );
test_evals_equal!(t10,"3*x+2*y", &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
