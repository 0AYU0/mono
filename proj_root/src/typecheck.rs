use crate::specification::{*};
use crate::expr::ExprT;
use crate::types::T::{*, self};
use crate::types::{*};

pub fn concretify(td: &TypeDefinition, t: &T) -> T{
  match t {
    Named(s) => {
      match td.get(s) {
        Some(k) => concretify(td, &k),
        _ => *t,
      }
    },
    _ => *t
  }
}

pub fn typecheck(ec: EvalContext, tc: TypeContext, td: &TypeDefinition, vc:VariantContext, e: ExprT) -> Option<T> {
  match e {
    ExprT::Wildcard => None,
    ExprT::Unctor(i, _) => {
        match vc.get(&i) {
          Some((t1, _)) => Some(*t1),
          _ => None,
        }
    },
    ExprT::Var(s) => {
      match tc.get(&s) {
        Some(t1) => Some(*t1),
        _ => None,
      }
    },
    ExprT::App(e1, e2) => {
      let exprT1 = typecheck(ec, tc, td, vc, *e1);
      match exprT1 {
        Some(t) => {
          let t1 = concretify(td, &t);
          if let Arrow(t11, t12) = t1 {
            Some(*t12)
          } else {
            None
          }
        },
        _ => None,
      }
    }
  }
}
