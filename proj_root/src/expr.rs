use egg::LanguageChildren;
use std::cmp::{min, max};

use crate::types::T;

pub static TARGET_FUNC: &'static str = "f";
pub static TARGET_FUNC_ARG: &'static str = "x";

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone)]
pub enum PatternT {
  Tuple(Vec<PatternT>),
  Ctor(String, Box<PatternT>),
  Var(String),
  Wildcard,
}

impl PatternT {
  fn contains_id (&self, i: &String) -> bool { 
    match self {
      PatternT::Tuple(ps) => ps.iter().any(|ty| ty.contains_id(i)),
      PatternT::Ctor(_, p) => p.contains_id(i),
      PatternT::Var(x) => x == i,
      PatternT::Wildcard => false
    }
  }
}

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
  Func(Param, Box<ExprT>),
  Ctor(String, Box<ExprT>),
  Unctor(String, Box<ExprT>),
  Eq(bool, Box<ExprT>, Box<ExprT>),
  Match(Box<ExprT>, Vec<(ExprT, ExprT)>),
  Fix(String, T, Box<ExprT>),
  Tuple(Vec<ExprT>),
  Proj(i32, Box<ExprT>),
}

fn destruct_tuple (e: ExprT) -> Vec<ExprT> { 
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

fn depth(expr: &ExprT) -> i32 {
  match expr {
      ExprT::Var(_) => 1,
      ExprT::Wildcard => 1,
      ExprT::App(e1, e2) => max(depth(e1), depth(e2)) + 1,
      ExprT::Func(_, e1) => depth(e1) + 1,
      ExprT::Ctor(_, e1) => depth(e1) + 1,
      ExprT::Unctor(_, e1) => depth(e1) + 1,
      ExprT::Eq(_, e1, e2) => max(depth(e1), depth(e2)) + 1,
      ExprT::Match(e, patterns) => patterns.iter().fold(depth(e), |acc, (_, e1)| max(acc, depth(e1))) + 1, // todo
      ExprT::Fix(_, _, e) => depth(e) + 1,
      ExprT::Tuple(es) => {
        if es.is_empty(){
          return 1;
        } else {
          let vec_depths = es.iter().map(|x| depth(x)).collect::<Vec<i32>>();
          let max = vec_depths.iter().max();
          match max {
            Some(x) => *x,
            _ => 0,
          }
        }
      },
      ExprT::Proj(_, e) => depth(e) + 1,
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
    ExprT::Match (_, patterns) => patterns.iter().map(|(_, e1)| e1).collect(),
    ExprT::Fix (_, _, e) => vec![e],
    ExprT::Tuple (es) => es.iter().map(|x| x).collect(),
    ExprT::Proj (_, e) => vec![e],
  }
}

pub fn unit() -> ExprT {
  return ExprT::Tuple(Vec::new());
}

pub fn true_() -> ExprT {
  return ExprT::Ctor("True".to_string(), Box::new(unit()));
}

pub fn false_() -> ExprT {
  return ExprT::Ctor("False".to_string(), Box::new(unit()));
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
  FuncV(Param, ExprT),
  CtorV(String, Box<Value>),
  TupleV(Vec<Value>),
  WildcardV,
  Bot
}

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
  TypeDeclaration(String, T),
  ExprDeclaration(String, Box<Declaration>)
}
