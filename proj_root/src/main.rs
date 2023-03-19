mod expr;
mod types;
mod specification;
mod bool_band;
mod generator;
mod typecheck;
use crate::expr::{*};
use crate::expr::ExprT::{*, self};
use crate::bool_band::{*};
use crate::types::T::{*, self};
use crate::specification::{*};
use crate::generator::{*};

fn main() {
  let mut plist: Vec<ExprT> = vec![ExprT::Tuple(Vec::new()), Var(TARGET_FUNC_ARG.to_string()),  Var(TARGET_FUNC.to_string())];
  let max_depth: i32 = 4;
  let (input_values, desired_type): (T, T) = (get_synth_type().0, get_synth_type().1);
  let mut tc: TypeContext = get_type_context();
  tc.insert(TARGET_FUNC.to_string(), Arrow(Box::new(input_values.clone()), Box::new(desired_type)));
  tc.insert(TARGET_FUNC_ARG.to_string(), input_values.clone());
  let spec: SpecT = specification::SpecT::new(get_synth_type(), get_eval_context(), tc, get_type_definition(), get_variant_context(), get_synth_examples());
  let grow_funcs: Vec<fn(&Vec<ExprT>, &SpecT, i32) -> Vec<ExprT>> = vec![grow_app, grow_ctor, grow_unctor, grow_eq, grow_tuple, grow_proj];
  for curr_depth in 1..3 {
    plist = grow_funcs.iter().fold(plist.clone(), |acc, grow_func| [acc, grow_func(&plist, &spec, curr_depth)].concat());
    print!("{:?}\n", plist);
  }
}