use egg::LanguageChildren;
use std::cmp::{min, max};
use std::collections::HashMap;
use crate::specification::{*};
use std::fmt;
use egg::{*};
use std::fmt::{Display};

use crate::types::T;

pub static TARGET_FUNC: &'static str = "f";
pub static TARGET_FUNC_ARG: &'static str = "x";

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Hash, Debug)]
pub enum PatternT {
  Tuple(Vec<PatternT>),
  Ctor(String, Box<PatternT>),
  Var(String),
  Wildcard,
}

impl fmt::Display for PatternT {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
        PatternT::Tuple(ts) => write!(f, "[{}]", ts.iter().map(|t| format!("{:?}", t)).collect::<Vec<_>>().join(", ")),
          PatternT::Ctor(i, t) => write!(f, "{}({})", i, t),
          PatternT::Var(i) => write!(f, "{}", i),
          PatternT::Wildcard => write!(f, "_"),
      }
  }
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

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Hash)]
pub struct Param {
  pub p_name: String,
  pub p_type: T,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Hash, From)]
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

fn variant_eq<T>(a: &T, b: &T) -> bool {
  std::mem::discriminant(a) == std::mem::discriminant(b)
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Hash, Debug)]
#[cfg_attr(feature = "serde-1", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprLang {
  /// The operator for an enode
  pub expr: ExprT,
  /// The enode's children `Id`s
  pub children: Vec<Id>,
}

impl ExprLang {
  /// Create an enode with the given string and children
  pub fn new(ex: impl Into<ExprT>, children: Vec<Id>) -> Self {
      let expr = ex.into();
      Self { expr, children }
  }

  /// Create childless enode with the given string
  pub fn leaf(expr: impl Into<ExprT>) -> Self {
      Self::new(expr, vec![])
  }
}

impl FromOp for ExprLang {
  type Error = std::convert::Infallible;

  fn from_op(expr: &str, children: Vec<Id>) -> Result<Self, Self::Error> {
      Ok(Self {
          expr: expr.into(),
          children,
      })
  }
}

impl egg::Language for ExprLang {
  fn matches(&self, other: &Self) -> bool {
    let e1 = self.expr.clone();
    let e2 = other.expr.clone();
    return variant_eq(&e1, &e2);
  }

  fn children(&self) -> &[Id] {
    return &self.children;
  }

  fn children_mut(&mut self) -> &mut [Id] {
      &mut self.children
  }
}

/*#[derive(Debug, Hash, PartialEq, Eq, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "serde-1", derive(serde::Serialize, serde::Deserialize))]
impl egg::Language for ExprLang {
  fn matches(&self, other: &Self) -> bool {
      true
  }

  fn children(&self) -> &[Id] {
      &self.children
  }

  fn children_mut(&mut self) -> &mut [Id] {
      &mut self.children
  }
}

impl fmt::Display for Param {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fn show(e: &Param) -> String {
      format!("fun ({}:{})\n", e.p_name, e.p_type)
    }
    write!(f, "{}", show(self))
  }
}*/

impl fmt::Debug for ExprT {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      fn show(e: &ExprT, indent: usize) -> String {
          fn make_indent(indent: usize) -> String {
              " ".repeat(indent * 2)
          }

          match e {
            ExprT::Var(i) => i.clone(),
            ExprT::Wildcard => "_".to_owned(),
            ExprT::App(e1, e2) => format!("({} {})", show(e1, indent), show(e2, indent)),
            ExprT::Func(Param{p_name: p, p_type: t}, e1) => format!("fun ({}:{}) ->\n{}", p, t, show(e1, indent + 1)),
            ExprT::Ctor(i, e1) => format!("{}({})", i, show(e1, 0)),
            ExprT::Unctor(i, e1) => format!("Un_{}({})", i, show(e1, indent)),
            ExprT::Eq(b, e1, e2) => format!("{} {} {}", show(e1, indent), if *b { "=" } else { "<>" }, show(e2, indent)),
            ExprT::Match(e1, patterns) => {
                  let mut result = format!("match {} with\n", show(e1, 0));
                  for (p, e2) in patterns {
                      result.push_str(&format!("{}{} ->\n{}\n", make_indent(indent+1), p, show(e2, indent)));
                  }
                  result
              },
              ExprT::Fix(i, t, e1) => format!("let rec ({i}: {t}) =\n{}", show(e1, indent + 1), i = i, t = t),
              ExprT::Tuple(es) => format!("{}",
                  es.iter().map(|e| show(e, 0)).collect::<Vec<_>>().join(", ")
              ),
              ExprT::Proj(i, e1) => format!("({}).{}", show(e1, 0), i),
          }
          .chars()
          .fold((make_indent(indent), true), |(mut acc, sep), c| {
              if sep && c != '\n' {
                  acc.push_str(&make_indent(indent));
              }
              acc.push(c);
              (acc, c == '\n')
          })
          .0
      }

      write!(f, "{}", show(self, 0))
  }
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

pub fn is_wildcard(t: ExprT) -> bool {
	match t { 
    ExprT::Wildcard => true,
	  _ => false 
  }
}

pub fn is_ctor_exp(t: ExprT) -> bool {
	match t { 
    ExprT::Ctor(_, _) => true,
	  _ => false 
  }
}

pub fn unitv() -> Value {
  return Value::TupleV(Vec::new());
}

pub fn truev_() -> Value {
  return Value::CtorV("True".to_string(), Box::new(unitv()));
}

pub fn falsev_() -> Value {
  return Value::CtorV("False".to_string(), Box::new(unitv()));
}

fn value_of_bool(b:bool) -> Value {
	if b { truev_()} else { falsev_()}
}

pub fn is_unctor_exp(t: ExprT) -> bool {
	match t { 
    ExprT::Unctor(_, _) => true,
	  _ => false 
  }
}

#[derive(PartialEq, Eq, Clone)]
pub enum Value {
  FuncV(Param, ExprT),
  CtorV(String, Box<Value>),
  TupleV(Vec<Value>),
  WildcardV,
  Bot
}

impl fmt::Debug for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
        Value::Bot => write!(f, "bot"),
        _ => {
          let exp = exp_of_value(self.clone());
          match exp {
            Some(e) => write!(f, "{:?}", e),
            _ => write!(f, "{:?}", exp)
          }
        },
    }
  }
}


#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
  TypeDeclaration(String, T),
  ExprDeclaration(String, ExprT)
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

fn matches_pattern_and_extractions(p: &PatternT, v: &Value) -> Option<Vec<(String, Value)>> {
  match (p, v) {
      (PatternT::Tuple(ps), Value::TupleV(vs)) => Some(ps.iter().zip(vs.iter()).map(|(p, v)| matches_pattern_and_extractions(p, v).unwrap()).flatten().collect()),
      (PatternT::Ctor(i, p), Value::CtorV(i_, v)) => {
          if i == i_ {
              matches_pattern_and_extractions(p, v)
          } else {
              None
          }
      },
      (PatternT::Var(i), v) => {
          if let Value::WildcardV = v {
              None
          } else {
              Some(vec![(i.to_string(), v.clone())])
          }
      },
      (PatternT::Wildcard, _) => Some(vec![]),
      _ => None
  }
}

pub fn replace_holes(eval_context: EvalContext, exp: ExprT) -> ExprT {
  //print!("REPLACE_HOLES attempting to fill EC {:?} with EXP {:?}\n", eval_context, exp);
  eval_context.iter().fold(exp.clone(), |acc, (i, e)| replace(i, e.clone(), acc))
}

pub fn replace(i: &str, e_with: ExprT, e: ExprT) -> ExprT {
    let replace_simple = |e: ExprT| replace(i, e_with.clone(), e);
    let e_orig = e.clone();
    match e {
        ExprT::Wildcard => e,
        ExprT::Eq(b, e1, e2) => ExprT::Eq(b, Box::new(replace_simple(*e1)), Box::new(replace_simple(*e2))),
        ExprT::Var(i_) => {
            if i == i_ {
                //print!("Currently attempting to fill hole {:?} with {:?}\n", e_with, i.to_string());
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
    let e_cop = e.clone();
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
      ExprT::Eq(b, e1, e2) => {
        let v1 = evaluate(*e1)?;
        let v2 = evaluate(*e2)?;
        let eq = v1 == v2;
        let res = if b { eq } else { !eq };
        Some(value_of_bool(res))
      },
      ExprT::Func(a, v) => Some(Value::FuncV(a.clone(), *v.clone())),
      ExprT::Ctor(i, e) => {
        let v = evaluate(*e)?;
        Some(Value::CtorV(i, Box::new(v)))
      },
      ExprT::Match(e, branches) => {
        let v = evaluate(*e)?;
        let mut bindings_branchexp_opt = vec![];
        for (p, branch_e) in branches.iter() {
            let mut binding_opt = None;
            match matches_pattern_and_extractions(&p, &v) {
                Some(bindings)=> binding_opt = Some((bindings, branch_e)),
                _ => {}
            };
            bindings_branchexp_opt.push(binding_opt);
        }
        let (bindings, branch_e) = bindings_branchexp_opt
            .iter()
            .find_map(|x| x.clone())?;
        let mut eval_context:EvalContext = HashMap::new();
        for (i, v) in bindings.iter() {
            assert_ne!(*v, Value::WildcardV);
            let exp = exp_of_value(v.clone())?;
            eval_context.insert(i.clone(), exp);
        }
        evaluate(replace_holes(eval_context, branch_e.clone()))
    },
    ExprT::Fix (i,_,e2) => {
      evaluate(replace(&i, e_cop, *e2))
    },
    ExprT::Tuple(es) => {
      let vs: Vec<Value> = es.iter().filter_map(|s| evaluate(s.clone())).collect();
      let len = vs.len();
      let ret = Some(Value::TupleV(vs));
      if len == es.len() { ret } else { None }
    },
    ExprT::Proj(i, e) => {
      let v = evaluate(*e)?;
      match v {
        Value::WildcardV => Some(Value::WildcardV),
        Value::TupleV(vs) => {
          if vs.len() > 0 { Some(vs[i as usize].clone()) } else { None }
        },
        _ => None
      }
    },
    ExprT::Unctor(i, e) => {
      let v = evaluate(*e)?;
      match v {
        Value::CtorV(i1, v1) => {
          assert_eq!(i, i1);
          Some(*v1)
        },
        _ => None
      }
    }
  }
}

pub fn evaluate_with_context(eval_context:EvalContext, e:ExprT) -> Option<Value> {
	let e = replace_holes(eval_context, e);
  evaluate(e)
}