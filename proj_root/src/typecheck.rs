use crate::specification::{*};
use crate::expr::{ExprT, PatternT};
use crate::types::T::{*, self};
use crate::types::{*};

pub fn concretify(td: &TypeDefinition, t: &T) -> T{
  match t {
    Named(s) => {
      td.get(s)
        .map(|k| concretify(td, &k))
        .unwrap_or_else(|| *t)
    },
    _ => *t
  }
}

pub fn typecheck_pattern(td: &TypeDefinition, p: &PatternT, t: &T) -> Option<Vec<(String, T)>>{
  match (p, &concretify(td, t)) {
    (PatternT::Tuple(p1), T::Tuple(p2)) => {
      let merges: Vec<Vec<(String, T)>> = p1.iter().zip(p2.iter()).map(|(p, t)| typecheck_pattern(td, p, t).unwrap()).collect();
      Some(merges.concat())
    },
    (PatternT::Ctor (i,p), T::Variant (variants)) => {
      let filtered: Option<&T> = variants.iter().filter_map(|(s, t)| if s == i { Some(t) } else { None }).next();
      match filtered {
        Some(ty) => typecheck_pattern(td, p, ty),
        _ => None
      }
    },
    (PatternT::Ctor (i,p), T::Named (x)) => {
      let t =  match td.get(x) {
        Some(Variant(k)) => {
          let filtered: Option<&T> = k.iter().filter_map(|(s, t)| if s == i { Some(t) } else { None }).next();
          match filtered {
            Some(ty) => typecheck_pattern(td, p, ty),
            _ => None
          }
        },
        _ => None,
      };
      t
    },
    (PatternT::Var (i), _) => Some(vec![(*i, *t)]),
    (PatternT::Wildcard, _) => Some(Vec::new()),
    _ => None
  }
}

pub fn typecheck(ec: &EvalContext, tc: &TypeContext, td: &TypeDefinition, vc: &VariantContext, e: ExprT) -> Option<T> {
  match e {
    ExprT::Wildcard => None,
    ExprT::Unctor(i, _) => {
      vc.get(&i).map(|(t1, _)| *t1)
    },
    ExprT::Var(s) => {
      tc.get(&s).map(|t1| *t1)
    },
    ExprT::App(e1, e2) => {
      typecheck(ec, tc, td, vc, *e1)
        .map(|t| {
            match concretify(td, &t) {
                Arrow(_, t12) => Some(*t12),
                _ => None,
            }
        })
    },
    ExprT::Func(param, e) => {
      let name = param.p_name;
      let ty = param.p_type;
      let mut _tc = tc.clone();
      _tc.insert(name, ty.clone());
      let t1 = typecheck(ec, tc, td, vc, *e);
      match t1 {
        Some(x) => Some(Arrow(Box::new(ty), Box::new(x))),
        _ => None,
      }
    },
    ExprT::Ctor(s1, e1) => {
      let t = typecheck(ec, tc, td, vc, *e1);
      match vc.get(&s1) {
        Some((_, t1)) => Some((*t1).clone()),
        _ => None,
      }
    },
    _ => todo!()
  }
}
