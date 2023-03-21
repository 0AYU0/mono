use crate::types::T::{*, self};
use crate::specification::{*};
use crate::expr::Value::{*, self};
use crate::expr::ExprT::{*, self};
use crate::expr::Declaration::{*, self};
use std::collections::HashMap;


pub fn get_nat() -> T {
  let nat: T = Variant(vec![("O".to_string(), T::Tuple(Vec::new())), ("S".to_string(), T::Named("nat".to_string()))]);
  return nat;
}

pub fn get_list() -> T {
  let list: T = Variant(vec![("Nil".to_string(), T::Tuple(Vec::new())), ("Cons".to_string(), T::Tuple(vec![T::Named("nat".to_string()), T::Named("list".to_string())]))]);
  return list;
}

pub fn get_synth_type() -> (T, T) {
  return (Named("list".to_string()), Named("nat".to_string()));
}

pub fn get_eval_context() -> EvalContext {
  return HashMap::new();
}

pub fn get_declarations() -> Vec<Declaration> {
  return vec![TypeDeclaration("nat".to_string(), get_nat()), TypeDeclaration("list".to_string(), get_list())];
}

pub fn get_type_context() -> TypeContext {
  return HashMap::new();
}

pub fn get_type_definition() -> TypeDefinition {
  return HashMap::from([("nat".to_string(), get_nat()), ("list".to_string(), get_list())]);
}

pub fn get_variant_context() -> VariantContext {
  let Nil = ("Nil".to_string(), (T::Tuple(Vec::new()), Named("list".to_string())));
  let Cons = ("Cons".to_string(), (T::Tuple(vec![Named("nat".to_string()), Named("list".to_string())]), T::Tuple(vec![Named("list".to_string())])));
  let O = ("O".to_string(), (T::Tuple(Vec::new()), Named("nat".to_string())));
  let S = ("S".to_string(), (Named("nat".to_string()), Named("nat".to_string())));
  return HashMap::from([Nil, Cons, O, S]);
}

pub fn get_synth_examples() -> Vec<(Value, Value)> {
  let nil: Value = CtorV("Nil".to_string(), Box::new(TupleV(Vec::new())));
  let zero = CtorV("O".to_string(), Box::new(TupleV(Vec::new())));
  let cons_one: Value = CtorV("Cons".to_string(), Box::new(TupleV(vec![zero.clone(), nil.clone()])));
  let cons_two: Value = CtorV("Cons".to_string(), Box::new(TupleV(vec![zero.clone(), CtorV("Cons".to_string(), Box::new(TupleV(vec![zero.clone(), nil.clone()])))])));
  return vec![(nil.clone(), nil.clone()), (cons_two.clone(), cons_one.clone()), (cons_one.clone(), cons_one.clone())];
}