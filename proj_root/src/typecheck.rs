use crate::specification::{*};
use crate::expr::ExprT;
use crate::types::T::{*, self};

pub fn concretify(td: TypeDefinition, t: T) -> T{
  match t {
    Named(s) => {
      if (td.contains_key(s)) {
        return concretify(td, td.get(s));
      } else { return t;}
    },
    _ => t
  }
}

pub fn typecheck(ec: EvalContext, tc: TypeContext, td: TypeDeclaration, vc:VariantContext, e: ExprT) {
  match e {
    Wildcard => printf("not typeable: {:?}", e),
    Unctor(i, _) => {
      if !vc.contains_key(i) {
        printf("typecheck error: unctor: {:?}", i)
      } else {
        vc.get(i).first()
      }
    },
    Var(s) => {
      if !tc.contains_key(s) {
        printf("typecheck error: var: {:?}", s)
      } else {
        get(s)
      }
    },
    App(e1, e2) => {
      let t1 = concretify(td, typecheck(ec, tc, td, vc, e1));
    }
  }
}
