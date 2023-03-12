use crate::types::T::{*, self};
use crate::expr::Value::{*, self};

pub fn get_bool() -> T {
  let bool: T = Variant(vec![("True".to_string(), Tuple(Vec::new())), ("False".to_string(), T::Tuple(Vec::new()))]);
  return bool;
}

pub fn get_io_examples() -> Vec<(Value, Value)> {
  let trueV: Value = CtorV("True".to_string(), Box::new(TupleV(Vec::new())));
  let falseV: Value = CtorV("False".to_string(), Box::new(TupleV(Vec::new())));
  let first = (TupleV(vec![Box::new(trueV), Box::new(trueV)]), trueV);
  let second = (TupleV(vec![Box::new(trueV), Box::new(falseV)]), falseV);
  return vec![first, second]
}
