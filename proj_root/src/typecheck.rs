use crate::specification::{*};
use crate::expr::{ExprT, PatternT};
use crate::types::T::{*, self};
use crate::types::{*};

pub fn concretify(td: &TypeDefinition, t: &T) -> T{
  match t {
    Named(s) => {
      td.get(s)
        .map(|k| concretify(td, &k))
        .unwrap_or_else(|| t.clone())
    },
    _ => t.clone()
  }
}

pub fn typecheck_pattern(td: &TypeDefinition, p: &PatternT, t: &T) -> Option<Vec<(String, T)>>{
  match (p, &concretify(td, t)) {
    (PatternT::Tuple(p1), T::Tuple(p2)) => {
      let merges: Vec<Vec<(String, T)>> = p1.iter().zip(p2.iter()).map(|(p, t)| typecheck_pattern(td, p, t).unwrap()).collect();
      Some(merges.concat())
    },
    (PatternT::Ctor (i,p), T::Variant (variants)) => {
      // CHANGED TO Iterator::map()
      variants.iter()
        .filter_map(|(s, t)| if s == i { Some(t) } else { None })
        .next()
        .map(|ty| typecheck_pattern(td, p, ty)).unwrap()
    },
    (PatternT::Ctor (i,p), T::Named (x)) => {
      match td.get(x) {
        Some(Variant(k)) => {
          k.iter()
           .filter_map(|(s, t)| if s == i { Some(t) } else { None })
           .next()
           .map(|ty| typecheck_pattern(td, p, ty)).unwrap()
        },
        _ => None,
      }
    },
    (PatternT::Var (i), _) => Some(vec![(i.clone(), t.clone())]),
    (PatternT::Wildcard, _) => Some(Vec::new()),
    _ => None
  }
}

pub fn typecheck(ec: &EvalContext, tc: &TypeContext, td: &TypeDefinition, vc: &VariantContext, e: &ExprT) -> Option<T> {
  match e {
    ExprT::Wildcard => None,
    ExprT::Unctor(i, _) => {
      vc.get(i).map(|(t1, _)| t1.clone())
    },
    ExprT::Var(s) => {
      //print!("Current tc: {:?}\n", tc);
      tc.get(s).map(|t1| t1.clone())
    },
    ExprT::App(e1, e2) => {
      typecheck(ec, tc, td, vc, e1)
        .map(|t| {
            match concretify(td, &t) {
                Arrow(_, t12) => Some(*t12),
                _ => None,
            }
        }).unwrap()
    },
    ExprT::Func(param, e) => {
      let name = &param.p_name;
      let ty = &param.p_type;
      let mut _tc = tc.clone();
      _tc.insert(name.clone(), ty.clone());

      typecheck(ec, &_tc, td, vc, e).map(|x| Arrow(Box::new(ty.clone()), Box::new(x)))
    },
    ExprT::Ctor(s1, e1) => {
      // do you actually use t for anything LOL
      let t = typecheck(ec, tc, td, vc, e1);
      vc.get(s1).map(|(_, t1)| Some((*t1).clone())).unwrap()
    },
    ExprT::Match(e, branches) => {
      let t = concretify(td, &typecheck(ec, tc, td, vc, e).unwrap());
      let ts: Vec<Option<T>> = branches.iter().map(|(p, e)| {
        let its = typecheck_pattern(td, p, &t);
        let mut _tc = tc.clone();
        match its {
          Some(its_add) => {
            its_add.iter().map(|(k,v)| _tc.insert(k.to_string(),v.clone()));
            typecheck(ec, &_tc, td, vc, e)
          },
          _ => None
        }
      }).collect();
      ts.get(0).unwrap().clone()
    },
    ExprT::Fix(i, t, e) => {
      let mut _tc = tc.clone();
      _tc.insert(i.to_string(), t.clone());
      typecheck(ec, &_tc, td, vc, e)
    },
    ExprT::Tuple(es) => {
      Some(T::Tuple(es.iter().map(|e| typecheck(ec, tc, td, vc, e).unwrap()).collect()))
    },
    ExprT::Proj(i, e) => {
      let t = concretify(td, &typecheck(ec, tc, td, vc, e).unwrap());
      if let T::Tuple(ts) = t {
        Some(ts[*i as usize].clone())
      } else {
        None
      }
    },
    ExprT::Eq(_, e1, e2) => Some(T::_bool())
  }
}
