use crate::types::T::{*, self};
use crate::specification::{*};
use crate::expr::Value::{*, self};
use crate::expr::ExprT::{*, self};
use std::collections::HashMap;

pub fn get_bool() -> T {
  let bool: T = Variant(vec![("True".to_string(), T::Tuple(Vec::new())), ("False".to_string(), T::Tuple(Vec::new()))]);
  return bool;
}

pub fn get_synth_type() -> (T, T) {
  return (T::Tuple(vec![Named("bool".to_string()), Named("bool".to_string())]), Named("bool".to_string()));
}

pub fn get_eval_context() -> EvalContext {
  return HashMap::new();
}

pub fn get_type_context() -> TypeContext {
  return HashMap::new();
}

pub fn get_type_definition() -> TypeDefinition {
  return HashMap::from([("bool".to_string(), get_bool())]);
}

pub fn get_variant_context() -> VariantContext {
  return HashMap::new();
}

pub fn get_synth_examples() -> Vec<(Value, Value)> {
  let true_v: Value = CtorV("True".to_string(), Box::new(TupleV(Vec::new())));
  let false_v: Value = CtorV("False".to_string(), Box::new(TupleV(Vec::new())));
  let first = (TupleV(vec![Box::new(true_v.clone()), Box::new(true_v.clone())]), true_v.clone());
  let second = (TupleV(vec![Box::new(true_v.clone()), Box::new(false_v.clone())]), false_v.clone());
  let third = (TupleV(vec![Box::new(false_v.clone()), Box::new(true_v.clone())]), false_v.clone());
  let fourth = (TupleV(vec![Box::new(false_v.clone()), Box::new(false_v.clone())]), false_v.clone());
  return vec![first, second, third, fourth];
}
