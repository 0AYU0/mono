use crate::types::T;
use crate::types::T::*;
use crate::expr::ExprT;
use crate::expr::Value;
use std::collections::HashMap;

type UnprocessedSpec = Vec<(Vec<ExprT>, ExprT)>;

type Spec = Vec<(Value, Value)>;

/* Unconverted OCamL code for handling unprocessed specs, may not need to use? 

type t_unprocessed = string list (* import file list *)
                     * declaration list (* type and value declarations *)
                     * Type.t (* type of a target function to be synthesized *)
                     * unprocessed_spec (* behavioral constraint *)
[@@deriving show]
*/

type EvalContext = HashMap<String, ExprT>;
type TypeContext = HashMap<String, T>;
type TypeDefinition = HashMap<String, T>;
type VariantContext = HashMap<String, (T, T)>;

enum SpecT {
  SynthType(T, T),
  EC(EvalContext),
  TC(TypeContext),
	TD(TypeDefinition),
  VC(VariantContext),
  Spec(Vec<(Value, Value)>),
}

fn st_to_pair (synth_type: T) -> (T, T) {
  fn f (mut acc: Vec<T>, t: T) -> (Vec<T>, T) {
    match t {
      T::Arrow(t1, t2) => {
        acc.push(*t1);
        return f (acc, *t2)
      },
      _ => {
        let new_acc = acc.into_iter().rev().collect();
        return (new_acc, t)
      }
    }
  }
	let (ts, t): (Vec<T>, T) = f(Vec::new(), synth_type);
  if ts.len() == 1{
    return (ts[0].clone(), t)
  } else {
    return (T::Tuple(ts), t)
  }
}
	
fn extract_variants (t: T) -> Vec<(String, T)>{
  match t {
    T::Named(_) => Vec::new(),
    T::Arrow(t1, t2) => {
      let mut t1_var: Vec<(String, T)> = extract_variants(*t1);
      t1_var.append(&mut extract_variants(*t2));
      return t1_var
    }
    T::Tuple(tys) => tys.iter().fold(Vec::new(), | vs, ty | [vs, extract_variants((*ty).clone())].concat()),
    T::Variant(vs) => {
      let clone = vs.clone();
      return vs.iter().fold(clone, | acc, (_, ty)| [acc, extract_variants((*ty).clone())].concat())
    }
  }
}