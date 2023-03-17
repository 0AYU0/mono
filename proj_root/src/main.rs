mod expr;
mod types;
mod specification;
mod bool_band;
mod generator;
use crate::expr::{*};
use crate::expr::ExprT::{*, self};
use crate::bool_band::{*};
use crate::types::T::{*, self};
use crate::specification::{*};
use crate::generator::{*};

fn main() {
  let mut plist: Vec<ExprT> = vec![Var(TARGET_FUNC_ARG.to_string()), true_(), false_()];
  
  let (input_values, desired_type): (T, T) = (get_synth_type().0, get_synth_type().1);
  let mut tc: TypeContext = get_type_context();
  tc.insert(TARGET_FUNC.to_string(), Arrow(Box::new(input_values.clone()), Box::new(desired_type)));
  tc.insert(TARGET_FUNC_ARG.to_string(), input_values.clone());
  let spec: SpecT = specification::SpecT::new(get_synth_type(), get_eval_context(), get_type_context(), get_type_definition(), get_variant_context(), get_synth_examples());
  let grow_funcs: Vec<fn(&Vec<ExprT>, &SpecT) -> Vec<ExprT>> = vec![grow_app, grow_ctor, grow_unctor, grow_eq];
  plist = grow_funcs.iter().fold(plist.clone(), |acc, grow_func| [acc, grow_func(&plist, &spec)].concat());
}
