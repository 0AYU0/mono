static TARGET_FUNC: &'static str = "f";
static TARGET_FUNC_ARG: &'static str = "x";

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone)]
struct Param<T> {
  p_name: String,
  p_type: T,
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone)]
enum ExprT {
  Var(String),
  Wildcard,
  App(Box<ExprT>, Box<ExprT>),
  Func(Param<Box<ExprT>>, Box<ExprT>), //should be Func(Param<Type.T>, T)
  Ctor(String, Box<ExprT>),
  Unctor(String, Box<ExprT>),
  Eq(bool, Box<ExprT>, Box<ExprT>),
  Match(Box<ExprT>, Vec<(Box<ExprT>, Box<ExprT>)>),
  Fix(String, Box<ExprT>, Box<ExprT>), //should be Fix(String, T, T)
  Tuple(Vec<Box<ExprT>>),
  Proj(i32, Box<ExprT>),
}

fn destruct_tuple (e: ExprT) -> Vec<Box<ExprT>> { 
	match e {
    ExprT::Tuple(es) => es,
    _ => Vec::new()
  }
}

fn size_of_expr (expr: ExprT) -> i32 { 
	match expr {
    ExprT::Var(_) => 1,
    ExprT::Wildcard => 1,
    ExprT::App(e1, e2) => size_of_expr(*e1) + size_of_expr(*e2),
    ExprT::Func(_, e1) => size_of_expr(*e1), 
    ExprT::Ctor(_, e1) => size_of_expr(*e1) + 1, 
    ExprT::Unctor(_, e1) => size_of_expr(*e1), 
    ExprT::Eq (_, e1, e2) => size_of_expr (*e1) + size_of_expr (*e2) + 1,
    ExprT::Match (e, patterns) => patterns.iter().fold(size_of_expr (*e) + 1, |acc, (_, e1)| acc + size_of_expr ((**e1).clone()) + 1),
    ExprT::Fix (_, _, e) => size_of_expr(*e),
    ExprT::Tuple (es) => es.iter().fold(0, |acc, e1| acc + size_of_expr((**e1).clone())),
    ExprT::Proj (_, e) => size_of_expr(*e),
  }
}

fn contains_id (i: &String, expr: ExprT) -> bool { 
	match expr { 
    ExprT::Var(j) => i == &j,
    ExprT::App(e1, e2) => contains_id(i, *e1) || contains_id(i, *e2),
    ExprT::Func (_, e) => contains_id(i, *e),
    ExprT::Ctor (_, e) => contains_id(i, *e),
    ExprT::Unctor (_, e) => contains_id(i, *e),
    ExprT::Eq(_, e1, e2) => contains_id(i, *e1) || contains_id(i, *e2),
    ExprT::Match (_, patterns) => patterns.iter().fold(false, |acc, (_, e1)| (acc || contains_id(i, (**e1).clone()))),
    ExprT::Fix (j, _, e) => i == &j || contains_id (i, *e),
    ExprT::Tuple (es) => es.iter().fold(false, |acc, e| acc || contains_id(i, (**e).clone())),
    ExprT::Proj (_, e) => contains_id(i, *e),
    _ => false,
  }
}
	
fn children_of_expr (expr: ExprT) -> Vec<Box<ExprT>> { 
	match expr { 
    ExprT::Var(_) => Vec::new(),
    ExprT::Wildcard => Vec::new(),
    ExprT::App(e1, e2) => vec![e1, e2],
    ExprT::Func (_, e) => vec![e],
    ExprT::Ctor (_, e) => vec![e],
    ExprT::Unctor (_, e) => vec![e],
    ExprT::Eq(_, e1, e2) => vec![e1, e2],
    ExprT::Match (_, patterns) => patterns.iter().map(|(_, e1)| e1.clone()).collect(),
    //ExprT::Match (_, patterns) => patterns.into_iter().map(Vec::new(), |acc, (_, e1)| Vec::new().append(&acc).push(e1)),
    ExprT::Fix (_, _, e) => vec![e],
    ExprT::Tuple (es) => es,
    ExprT::Proj (_, e) => vec![e],
  }
}

