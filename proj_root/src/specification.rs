use crate::bool_impl::get_declarations;
use crate::types::T;
use crate::expr::ExprT;
use crate::expr::{*};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use crate::expr::Declaration;
use crate::typecheck::*;

type UnprocessedSpec = Vec<(Vec<ExprT>, ExprT)>;

type Spec = Vec<(Value, Value)>;

/* Unconverted OCamL code for handling unprocessed specs, may not need to use? 

type t_unprocessed = string list (* import file list *)
                     * declaration list (* type and value declarations *)
                     * Type.t (* type of a target function to be synthesized *)
                     * unprocessed_spec (* behavioral constraint *)
[@@deriving show]
*/

pub type EvalContext = HashMap<String, ExprT>; // External function names to corresponding expressions ex. add x -> S(x)
pub type TypeContext = HashMap<String, T>;     // Internal variables to corresponding types ex. x (function arg parameter) -> bool
pub type TypeDefinition = HashMap<String, T>;  // Mapping of external type ex. bool -> Variant(True, False)
pub type VariantContext = HashMap<String, (T, T)>; // Mapping of variant_type -> (arg_type, parent_type / resulting type)

#[derive(Clone, Debug)]
pub struct SpecT {
  pub synth_type: (T, T),       
  pub ec: EvalContext,
  pub tc: TypeContext,
	pub td: TypeDefinition,
  pub vc: VariantContext,
  pub spec: Vec<(Value, Value)>,
}


impl SpecT {
  // A public constructor method for a new behavioral specification
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

/* 
  Transforms the input synthesis type from a set of arrow functions to a tuple
  list -> nat becomes (list, nat)
  bool -> bool -> bool becomes ((bool, bool), bool)
*/
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
	
/* Extract all variants from a type declaration ex. Named("bool") -> Vec<("True"), ...), ("False", ...)>*/
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

/* Process external types and function operators provided in the specification */
pub fn process_decl_list(decls: Vec<Declaration>) -> (EvalContext, TypeContext, TypeDefinition, VariantContext){
  let mut ec:EvalContext = HashMap::new();
  let mut tc:TypeContext = HashMap::new();
  let mut td:TypeDefinition = HashMap::new();
  let mut vc:VariantContext = HashMap::new();

  for decl in decls.iter() {
    match decl {
      Declaration::TypeDeclaration (id, ty) => {
        let all_variants = extract_variants(ty.clone());
        td.insert(id.to_string(), ty.clone());
        for (ctor_id, arg_ty) in all_variants.iter(){
          vc.insert(ctor_id.to_string(), (arg_ty.clone(), T::Named(id.to_string())));
        }
      },
      Declaration::ExprDeclaration(id, e) => {
        ec.insert(id.to_string(), replace_holes(ec.clone(), e.clone()));
        let ty = typecheck(&ec, &tc, &td, &vc, &e).unwrap();
        tc.insert(id.to_string(), ty);
      }
    }
  }
  //print!("EC {:?} TC {:?} TD {:?} VC {:?}", ec, tc, td, vc);
  return (ec, tc, td, vc)
}

/*
  Input: Specification, bank of components, and current observational equivalence mapping
  Output: Tuple of (valid expressions, map IO example to satisfying expressions, map IO examples to satisfying ,types to expressions)
 */
pub fn process_spec (spec: &SpecT, bank: &HashSet<ExprT>, obs_eq: &mut HashMap<String, ExprT>) -> (HashSet<ExprT>, Vec<((Value, Value), Vec<ExprT>)>, HashMap<ExprT, Vec<usize>>, HashMap<T, HashSet<ExprT>>) {
  let io_examples: &Vec<(Value, Value)>= &spec.spec;
  let mut io_blocks: Vec<((Value, Value), Vec<ExprT>)> = Vec::new();
  let decls = get_declarations();
  let (ec, tc, td, vc) = process_decl_list(decls);
  let mut program_blocks: HashMap<ExprT, Vec<usize>> = HashMap::new();

  let mut ty_to_exprs: HashMap<T, HashSet<ExprT>> = HashMap::new();

  for test in io_examples.iter() {
    io_blocks.push((test.clone(), Vec::new()));
  } 
  let mut new_bank = HashSet::new();
  for expr in bank.iter() {
    let mut outputs = Vec::new();
    let mut index = 0;
    for test in io_examples.iter(){
      let test_i = exp_of_value(test.0.clone()).unwrap();
      let e1 = replace(&TARGET_FUNC_ARG.to_string(), test_i.clone(), expr.clone());
      //print!("Trying to evaluate {:?}\n", e1);
      let result: Option<Value> = evaluate_with_context(ec.clone(), e1.clone());
      match result {
        Some(r1) => {
          let res_type = typecheck(&ec, &tc, &td, &vc, &e1);
          match res_type {
            Some(t) => {
              if ty_to_exprs.contains_key(&t) {
                let mut hs = ty_to_exprs.get(&t).unwrap().clone();
                hs.insert(expr.clone());
                ty_to_exprs.insert(t, hs);
              } else {
                let mut hs = HashSet::new();
                hs.insert(expr.clone());
                ty_to_exprs.insert(t, hs);
              }
            },
            _ => {print!("invalid type");}
          }
          //print!("Reached result {:?}\n", r1);
          new_bank.insert(expr.clone());
          if r1 == test.1 {
            (io_blocks[index]).1.push(expr.clone());
            outputs.push(true);
          } else {
            outputs.push(false);
          }
        },
        _ => {
          outputs.push(false);
          ()
        }      
      }
      index += 1;
    }
    let x = format!("{:?}", outputs);
    let mut y = Vec::new();
    for i in 0..outputs.len() {
      if outputs[i] {
        y.push(i);
      }
    }
    if(y.len() > 0){
      program_blocks.insert(expr.clone(), y);
    }

    if !obs_eq.contains_key(&x){
      obs_eq.insert(x, expr.clone());
    }
  }
  for (_, value) in obs_eq.iter(){
    new_bank.insert(value.clone());
  }
  return (new_bank, io_blocks, program_blocks, ty_to_exprs);
}