use egg::*;
use crate::types::T;


define_language! {
  pub enum TLanguage {
    Named(String),
    "Arrow" = Arrow([Id; 2]),
    "Tuple" = Tuple(Box<[Id]>),
    "Variant" = Variant(Box<[Id]>),
  }
}

define_language! {
  pub enum ExprLanguage {
      Var(TLanguage),
      "_" = Wildcard,
      "App" = App([Id; 2]),
      "Func" = Func([Id; 2]),
      "Ctor" = Ctor([Id; 2]),
      "Unctor" = Unctor([Id; 2]),
      "Eq" = Eq([Id; 2]),
      "Match" = Match([Id; 3]),
      "Fix" = Fix([Id; 3]),
      "Tuple" = Tuple(Box<[Id]>),
      "Proj" = Proj([Id; 2]),
  }
}

fn make_rules() -> Vec<Rewrite<ExprLanguage, ()>> {
  vec![
      rewrite!("commute-add"; "(+ ?a ?b)" => "(+ ?b ?a)"),
      rewrite!("commute-mul"; "(* ?a ?b)" => "(* ?b ?a)"),
      rewrite!("add-0"; "(+ ?a 0)" => "?a"),
      rewrite!("mul-0"; "(* ?a 0)" => "0"),
      rewrite!("mul-1"; "(* ?a 1)" => "?a"),
  ]
}

fn simplify(s: &str) -> String {
  // parse the expression, the type annotation tells it which Language to use
  let expr: RecExpr<ExprLanguage> = s.parse().unwrap();
  let mut rules: Vec<Rewrite<ExprLanguage, ()>> = Vec::new();
  // simplify the expression using a Runner, which creates an e-graph with
  // the given expression and runs the given rules over it
  let runner = Runner::default().with_expr(&expr).run(&rules);

  // the Runner knows which e-class the expression given with `with_expr` is in
  let root = runner.roots[0];

  // use an Extractor to pick the best element of the root eclass
  let extractor = Extractor::new(&runner.egraph, AstSize);
  let (best_cost, best) = extractor.find_best(root);
  println!("Simplified {} to {} with cost {}", expr, best, best_cost);
  best.to_string()
}

#[test]
fn simple_tests() {
    assert_eq!(simplify("(* 0 42)"), "0");
    assert_eq!(simplify("(+ 0 (* 1 foo))"), "foo");
}
