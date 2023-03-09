static TARGET_FUNC: &'static str = "f";
static TARGET_FUNC_ARG: &'static str = "x";

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
struct Param<T> {
  p_name: String,
  p_type: T,
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
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

fn size_of_expr (e: &ExprT) -> i32 { 
	match e {
    ExprT::Var(_) => 1,
    ExprT::Wildcard => 1,
    ExprT::App(e1, e2) => size_of_expr(&*e1) + size_of_expr(&*e2),
    ExprT::Func(_, e1) => size_of_expr(&*e1), 
    ExprT::Ctor(_, e1) => size_of_expr(&*e1) + 1, 
    ExprT::Unctor(_, e1) => size_of_expr(&*e1), 
    ExprT::Eq (_, e1, e2) => size_of_expr (&*e1) + size_of_expr (&*e2) + 1,
    ExprT::Match (e, patterns) => patterns.iter().fold(size_of_expr (&*e) + 1, |acc, (_, e1)| acc + size_of_expr (&**e1) + 1),
    ExprT::Fix (_, _, e) => size_of_expr(&*e),
    ExprT::Tuple (es) => es.iter().fold(0, |acc, e1| acc + size_of_expr(&**e1)),
    ExprT::Proj (_, e) => size_of_expr(&*e)
  }
}

fn contains_id (i:&String, e: &ExprT) -> bool { 
	match e { 
    ExprT::Var(j) => i.eq(j),
    ExprT::App(e1, e2) => contains_id(i, e1) || contains_id(i, e2),
    ExprT::Func (_, e) => contains_id(i, e),
    ExprT::Ctor (_, e) => contains_id(i, e),
    ExprT::Unctor (_, e) => contains_id(i, e),
    ExprT::Eq(_, e1, e2) => contains_id(i, e1) || contains_id(i, e2),
    ExprT::Match (_, patterns) => patterns.iter().fold(false, |acc, (_, e1)| (acc || contains_id(i, e1))),
    ExprT::Fix (j, _, e) => i.eq(j) || contains_id (i, e),
    ExprT::Tuple (es) => es.iter().fold(false, |acc, e| acc || contains_id(i, e)),
    ExprT::Proj (_, e) => contains_id(i, e),
    _ => false,
  }
}
	