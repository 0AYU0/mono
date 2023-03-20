use crate::types::T;
use crate::types::T::*;
use crate::expr::ExprT;
use crate::expr::{*};
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

pub type EvalContext = HashMap<String, ExprT>;
pub type TypeContext = HashMap<String, T>;
pub type TypeDefinition = HashMap<String, T>;
pub type VariantContext = HashMap<String, (T, T)>;

pub struct SpecT {
  pub synth_type: (T, T),
  pub ec: EvalContext,
  pub tc: TypeContext,
	pub td: TypeDefinition,
  pub vc: VariantContext,
  pub spec: Vec<(Value, Value)>,
}

impl SpecT {
  // A public constructor method
  pub fn new(synth_type: (T, T), ec: EvalContext, tc: TypeContext, td: TypeDefinition, vc: VariantContext, spec: Vec<(Value, Value)>) -> SpecT {
    SpecT {
          synth_type: synth_type,
          ec: ec,
          tc: tc,
          td: td,
          vc: vc,
          spec: spec,
      }
  }
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

pub fn expected_sig (spec: &SpecT) -> Vec<Value> {
  let mut output_bank: Vec<Value> = Vec::new();
  return output_bank;
}

pub fn process_spec (spec: &SpecT, bank: &Vec<ExprT>) -> Vec<((Value, Value), Vec<ExprT>)> {
  let io_examples: &Vec<(Value, Value)>= &spec.spec;
  let mut processed_spec: Vec<((Value, Value), Vec<ExprT>)> = Vec::new();
  for test in io_examples.iter() {
    processed_spec.push((test.clone(), Vec::new()));
  } 
  for expr in bank.iter(){
    print!("Currently evaluating {:?}\n", expr);
    let result: Option<Value> = evaluate_with_context(spec.ec.clone(), expr.clone());
    let mut index = 0;
    match result {
      Some(r1) => {
        print!("Reached result {:?}\n", r1);
        for test in io_examples.iter() {
          if r1 == test.1 {
            processed_spec[index].1.push(expr.clone());
            print!("{:?} satisfies IO example {:?}\n", expr, test);
          }
          index += 1;
        }
      },
      _ => ()      
    }
  }
  return processed_spec;
}