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
  pub fn contains_id (&self, i: &String) -> bool { 
    match self {
      PatternT::Tuple(ps) => ps.iter().any(|ty| ty.contains_id(i)),
      PatternT::Ctor(_, p) => p.contains_id(i),
      PatternT::Var(x) => x == i,
      PatternT::Wildcard => false
    }
  }
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone)]
pub struct Param {
  pub p_name: String,
  pub p_type: T,
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
  Match(Box<ExprT>, Vec<(PatternT, ExprT)>),
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

pub fn is_ctor_exp(t: ExprT) -> bool {
	match t { 
    ExprT::Ctor(_, _) => true,
	  _ => false 
  }
}

pub fn is_unctor_exp(t: ExprT) -> bool {
	match t { 
    ExprT::Unctor(_, _) => true,
	  _ => false 
  }
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

pub fn exp_of_value(v:Value) -> Option<ExprT> {
  match v {
    Value::FuncV (p, e) => Some(ExprT::Func (p, Box::new(e))),
    Value::CtorV (i, v) => {
      let res = exp_of_value(*v);
      match res {
        Some(r) => Some(ExprT::Ctor(i, Box::new(r))),
        _ => None
      }
    },
    Value::WildcardV => Some(ExprT::Wildcard),
    Value::TupleV(vs) => {
      let res = vs.iter().map(|v| exp_of_value(v.clone())).collect();
      match res {
        Some(r) =>  Some(ExprT::Tuple (r)),
        _ => None
      }
    },
    Value::Bot => None
  }
}

fn replace(i: &str, e_with: ExprT, e: ExprT) -> ExprT {
    let replace_simple = |e: ExprT| replace(i, e_with.clone(), e);
    let e_orig = e.clone();
    match e {
        ExprT::Wildcard => e,
        ExprT::Eq(b, e1, e2) => ExprT::Eq(b, Box::new(replace_simple(*e1)), Box::new(replace_simple(*e2))),
        ExprT::Var(i_) => {
            if i == i_ {
                e_with
            } else {
                e_orig
            }
        }
        ExprT::App(e1, e2) => ExprT::App(Box::new(replace_simple(*e1)), Box::new(replace_simple(*e2))),
        ExprT::Func(Param{p_name: i_, p_type: t}, e_) => {
            if i == i_ {
                e_orig
            } else {
                ExprT::Func(Param{p_name: i_, p_type: t}, Box::new(replace_simple(*e_)))
            }
        }
        ExprT::Ctor(i_, e_) => ExprT::Ctor(i_, Box::new(replace_simple(*e_))),
        ExprT::Unctor(i_, e_) => ExprT::Unctor(i_, Box::new(replace_simple(*e_))),
        ExprT::Match(e_, branches) => {
            let branches = branches
                .into_iter()
                .map(|(p, e)| {
                    if p.contains_id(&i.to_string()) {
                        (p, e)
                    } else {
                        (p, replace_simple(e))
                    }
                })
                .collect();
              ExprT::Match(Box::new(replace_simple(*e_)), branches)
        }
        ExprT::Fix(i_, t, e_) => {
            if i == i_ {
                e_orig
            } else {
              ExprT::Fix(i_, t, Box::new(replace_simple(*e_)))
            }
        }
        ExprT::Tuple(es) => ExprT::Tuple(es.into_iter().map(replace_simple).collect()),
        ExprT::Proj(i_, e_) => ExprT::Proj(i_, Box::new(replace_simple(*e_))),
    }
}

pub fn evaluate(e: ExprT) -> Option<Value> {
    match e {
      ExprT::Wildcard => Some(Value::WildcardV),
      ExprT::Var(i) => None,
      ExprT::App(e1, e2) => {
        let v1 = evaluate(*e1)?;
        let expr = exp_of_value(v1)?;
        match expr {
          ExprT::Func(Param{p_name: i, ..}, exp1) => {
            let v2 = evaluate(*e2)?;
            let expr2 = exp_of_value(v2)?;
            evaluate(replace(&i, expr2, *exp1))
          },
          ExprT::Wildcard => Some(Value::WildcardV),
          _ => None
        }
      },
      _ => todo!()
    }
}
