use crate::types::T;
use crate::types::T::*;

pub fn get_bool() -> T {
  let bool: T = Variant(vec![("True".to_string(), Named("true".to_string())), ("False".to_string(), Named("false".to_string()))]);
  return bool;
}
