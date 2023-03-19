use crate::expr::Value::{*, self};
use crate::expr::ExprT::{*, self};
use crate::expr::{*};
use crate::types::{*};
use crate::types::T::{*, self};
use crate::specification::{*};
use crate::typecheck::{*};
use std::collections::BTreeSet;

pub fn grow_app(bank: &Vec<ExprT>, spec: &SpecT, curr_depth: i32) -> Vec<ExprT> {
  //Filters out all arrow functions
  let type_context: &mut TypeContext = &mut (spec.tc).clone();
  type_context.retain(|_, v| is_arrow_type((*v).clone()));
  /*let mut result_ty_arg_tys_arg_expss_set: BTreeSet<(T, Vec<T>, Vec<ExprT>)> = BTreeSet::new();
  for (_, (arg_ty, parent_ty)) in type_context {
    result_ty_arg_tys_arg_expss_set.insert((parent_ty.clone(), vec![arg_ty.clone()], Vec::new()));
  }*/

  let mut new_bank: Vec<ExprT> = Vec::new();
  for component_one in bank.iter() {
    for component_two in bank.iter() {   
        new_bank.push(ExprT::App(Box::new(component_one.clone()), Box::new(component_two.clone())));
    }
  }
  return new_bank;
}

pub fn grow_ctor(bank: &Vec<ExprT>, spec: &SpecT, curr_depth: i32) -> Vec<ExprT> {
  let variant_context: &VariantContext = &spec.vc;
  let mut result_ty_arg_tys_arg_expss_set: BTreeSet<(T, Vec<T>, Vec<ExprT>)> = BTreeSet::new();
  for (_, (arg_ty, parent_ty)) in variant_context {
    result_ty_arg_tys_arg_expss_set.insert((parent_ty.clone(), vec![arg_ty.clone()], Vec::new()));
  }

  /*let type_defs: &TypeDefinition = &spec.td;

  // Find all Variants that are constructors
  let mut ctor_types: Vec<(String, T)> = Vec::new();
  for (_, v) in type_defs.iter() {
    match v {
      Variant(vec) => {
        for (s1, t1) in vec.iter(){
          match t1 {
            Named(_) => ctor_types.push((s1.to_string(), t1.clone())),
            _ => continue,
          }
        }
      },
      _ => continue,
    }
  }*/

  let mut expression_bank: Vec<ExprT> = Vec::new();
  //Need to do extra pruning since the constructors can only act on certain types based on what is in the 'named'
  for (s1, (arg_ty, parent_ty)) in variant_context.iter() {
    for expr in bank.iter() {
      let ty: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, expr);
      match ty {
        Some(s_ty) => {
          if *arg_ty == s_ty {
          expression_bank.push(ExprT::Ctor(s1.to_string(), Box::new(expr.clone())));
          }
        }
        None => print!("Typecheck failed on: {:?}\n", *expr),
      }
      /*match (s1, (arg_ty, parent_ty)) {
        (s1, (arg_ty, parent_ty)) => expression_bank.push(ExprT::Ctor(s1.to_string(), Box::new(component.clone()))),
        _ => continue,
      }*/
    }
  }
  return expression_bank;
}

pub fn grow_unctor(bank: &Vec<ExprT>, spec: &SpecT, curr_depth: i32) -> Vec<ExprT> {
  let variant_context: &VariantContext = &spec.vc; 
  let mut result_ty_arg_tys_arg_expss_set: BTreeSet<(T, Vec<T>, Vec<ExprT>)> = BTreeSet::new();
  for (_, (arg_ty, parent_ty)) in variant_context {
    result_ty_arg_tys_arg_expss_set.insert((parent_ty.clone(), vec![arg_ty.clone()], Vec::new()));
  }

  /*let type_defs: &TypeDefinition = &spec.td;

  // Find all Variants that are constructors
  let mut ctor_types: Vec<(String, T)> = Vec::new();
  for (_, v) in type_defs.iter() {
    match v {
      Variant(vec) => {
        for (s1, t1) in vec.iter(){
          match t1 {
            Named(_) => ctor_types.push((s1.to_string(), t1.clone())),
            _ => continue,
          }
        }
      },
      _ => continue,
    }
  }*/

  let mut expression_bank: Vec<ExprT> = Vec::new();
  //Need to do extra pruning since the constructors can only act on certain types based on what is in the 'named'
  for (s1, (arg_ty, parent_ty)) in variant_context.iter() {
    for expr in bank.iter() {
      let ty: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, expr);
      match ty {
        Some(s_ty) => {if s_ty == *parent_ty && !(*arg_ty == T::_unit()){
          expression_bank.push(ExprT::Unctor(s1.to_string(), Box::new(expr.clone())))
          }
          //print!("Typecheck worked on: {:?} with {:?}\n", *expr, s_ty)
        },
        None => print!("Typecheck failed on: {:?}\n", *expr),
      }
    }
  }
  return expression_bank;
}

/*This may not actually have to do anything - in trio it just returns the exact same 'bank' or mapping of expressions */
pub fn grow_eq(bank: &Vec<ExprT>, spec: &SpecT, curr_depth: i32) -> Vec<ExprT> {
  let mut new_bank: Vec<ExprT> = Vec::new();
  for component_one in bank.iter() {
    for component_two in bank.iter() {   
        new_bank.push(ExprT::Eq(true, Box::new(component_one.clone()), Box::new(component_two.clone())));
        new_bank.push(ExprT::Eq(false, Box::new(component_one.clone()), Box::new(component_two.clone())));
    }
  }
  return Vec::new();
}

pub fn grow_tuple(bank: &Vec<ExprT>, spec: &SpecT, curr_depth: i32) -> Vec<ExprT> {
  let mut new_bank: Vec<ExprT> = Vec::new();
  let vector_tuples = grow_tuple_helper(bank, spec, curr_depth, vec![Vec::new()]); 
  for vec in vector_tuples.iter() {
    new_bank.push(ExprT::Tuple(vec.to_vec()));
  }
  return new_bank;
}

pub fn grow_tuple_helper(bank: &Vec<ExprT>, spec: &SpecT, curr_depth: i32, curr_tuples: Vec<Vec<ExprT>>) -> Vec<Vec<ExprT>> {
  if curr_depth == 0 {
    return curr_tuples;
  }
  let mut new_tuples = Vec::new();
  for component in bank.iter() {
    for tuple in curr_tuples.iter() {
      let mut t1: Vec<ExprT> = tuple.clone();
      t1.push(component.clone());
      new_tuples.push(t1);
    }
  }
  return grow_tuple_helper(bank, spec, curr_depth - 1, new_tuples); 
}

pub fn grow_proj(bank: &Vec<ExprT>, spec: &SpecT, curr_depth: i32) -> Vec<ExprT> {
  let mut new_bank: Vec<ExprT> = Vec::new(); 
  if curr_depth == 1{
    let input_ty = &spec.synth_type.0;
    match input_ty {
      T::Tuple(vec) => {
        for size in 0..vec.len() {
          new_bank.push(ExprT::Proj(size.try_into().unwrap(), Box::new(ExprT::Var(TARGET_FUNC_ARG.to_string()))))
        }
      },
      _ => ()
    }
    return new_bank;
  }
  for component in bank.iter() {
    match component {
      ExprT::Tuple(vec) => {
        for size in 0..vec.len() {
          new_bank.push(ExprT::Proj(size.try_into().unwrap(), Box::new(component.clone())))
        }
      }
      _ => ()
    }
  }
  return new_bank;

}


