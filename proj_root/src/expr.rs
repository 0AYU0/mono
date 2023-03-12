use crate::types::T;

static TARGET_FUNC: &'static str = "f";
static TARGET_FUNC_ARG: &'static str = "x";

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone)]
struct Param {
  p_name: String,
  p_type: T,
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone)]
pub enum ExprT {
  Var(String),
  Wildcard,
  App(Box<ExprT>, Box<ExprT>),
  Func(Param, Box<ExprT>), //should be Func(Param<Type.T>, T)
  Ctor(String, Box<ExprT>),
  Unctor(String, Box<ExprT>),
  Eq(bool, Box<ExprT>, Box<ExprT>),
  Match(Box<ExprT>, Vec<(Box<ExprT>, Box<ExprT>)>),
  Fix(String, T, Box<ExprT>), //should be Fix(String, T, T)
  Tuple(Vec<Box<ExprT>>),
  Proj(i32, Box<ExprT>),
}



fn destruct_tuple (e: ExprT) -> Vec<Box<ExprT>> { 
	match e {
    ExprT::Tuple(es) => es,
    _ => Vec::new()
  }
}

fn sizeof(expr: &ExprT) -> i32 {
  match expr {
      ExprT::Var(_) => 1,
      ExprT::Wildcard => 1,
      ExprT::App(e1, e2) => sizeof(e1) + sizeof(e2),
      ExprT::Func(_, e1) => sizeof(e1),
      ExprT::Ctor(_, e1) => sizeof(e1) + 1,
      ExprT::Unctor(_, e1) => sizeof(e1),
      ExprT::Eq(_, e1, e2) => sizeof(e1) + sizeof(e2) + 1,
      ExprT::Match(e, patterns) => patterns.iter().fold(sizeof(e) + 1, |acc, (_, e1)| acc + sizeof(e1) + 1),
      ExprT::Fix(_, _, e) => sizeof(e),
      ExprT::Tuple(es) => es.iter().fold(0, |acc, e1| acc + sizeof(e1)),
      ExprT::Proj(_, e) => sizeof(e),
  }
}


fn contains_id (i: &String, expr: &ExprT) -> bool { 
	match expr { 
    ExprT::Var(j) => i == j.as_str(),
    ExprT::App(e1, e2) => contains_id(i, e1) || contains_id(i, e2),
    ExprT::Func (_, e) => contains_id(i, e),
    ExprT::Ctor (_, e) => contains_id(i, e),
    ExprT::Unctor (_, e) => contains_id(i, e),
    ExprT::Eq(_, e1, e2) => contains_id(i, e1) || contains_id(i, e2),
    ExprT::Match (_, patterns) => patterns.iter().fold(false, |acc, (_, e1)| (acc || contains_id(i, e1))),
    ExprT::Fix (j, _, e) => i == j.as_str() || contains_id (i, e),
    ExprT::Tuple (es) => es.iter().fold(false, |acc, e| acc || contains_id(i, e)),
    ExprT::Proj (_, e) => contains_id(i, e),
    _ => false,
  }
}
	
fn children_of_expr (expr: &ExprT) -> Vec<&ExprT> { 
	match expr { 
    ExprT::Var(_) => Vec::new(),
    ExprT::Wildcard => Vec::new(),
    ExprT::App(e1, e2) => vec![e1, e2],
    ExprT::Func (_, e) => vec![e],
    ExprT::Ctor (_, e) => vec![e],
    ExprT::Unctor (_, e) => vec![e],
    ExprT::Eq(_, e1, e2) => vec![e1, e2],
    ExprT::Match (_, patterns) => patterns.iter().map(|(_, e1)| e1.as_ref()).collect(),
    ExprT::Fix (_, _, e) => vec![e],
    ExprT::Tuple (es) => es.iter().map(|x| x.as_ref()).collect(),
    ExprT::Proj (_, e) => vec![e],
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
  FuncV(Param, ExprT),
  CtorV(String, Box<Value>),
  TupleV(Vec<Box<Value>>),
  WildcardV,
  Bot
}

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
  TypeDeclaration(String, T),
  ExprDeclaration(String, Box<Declaration>)
}
