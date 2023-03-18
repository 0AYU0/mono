use crate::specification::{*};
use crate::expr::{ExprT, PatternT};
use crate::types::T::{*, self};
use crate::types::{*};

pub fn concretify(td: &TypeDefinition, t: &T) -> T{
  match t {
    Named(s) => {
      match td.get(s) {
        Some(k) => concretify(td, &k),
        _ => (*t).clone(),
      }
    },
    _ => (*t).clone()
  }
}

pub fn typecheck_pattern(td: &TypeDefinition, p: &PatternT, t: &T) -> Vec<(String, T)>{
  match (p, concretify(td, t)) {
    (PatternT::Tuple(p1), T::Tuple(p2)) => {
      let merges: Vec<(Vec<(String, T)>)> = p1.iter().zip(p2.iter()).map(|(p, t)| typecheck_pattern(td, p, t)).collect();
      merges.concat()
    },
    (PatternT::Ctor (i,p), T::Variant (variants)) => {
      for (s, t) in variants.iter() {
        if(s == i) {
          return typecheck_pattern(td, p, t);
        }
      }
      Vec::new()
    }
    (PatternT::Ctor (i,p), T::Named (x)) => {

    }
  }
}

pub fn typecheck(ec: &EvalContext, tc: &TypeContext, td: &TypeDefinition, vc: &VariantContext, e: ExprT) -> Option<T> {
  match e {
    ExprT::Wildcard => None,
    ExprT::Unctor(i, _) => {
        match vc.get(&i) {
          Some((t1, _)) => Some((*t1).clone()),
          _ => None,
        }
    },
    ExprT::Var(s) => {
      match tc.get(&s) {
        Some(t1) => Some((*t1).clone()),
        _ => None,
      }
    },
    ExprT::App(e1, e2) => {
      let expr_t1 = typecheck(ec, tc, td, vc, *e1);
      match expr_t1 {
        Some(t) => {
          let t1 = concretify(td, &t);
          if let Arrow(_, t12) = t1 {
            Some(*t12)
          } else {
            None
          }
        },
        _ => None,
      }
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
