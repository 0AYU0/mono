use crate::expr::Value::{*, self};
use crate::expr::ExprT::{*, self};
use crate::expr::{*};
use crate::specification::{*};

pub fn growApp(bank: Vec<ExprT>, spec: SpecT) -> Vec<ExprT> {
  let mut newBank: Vec<ExprT> = Vec::new();
  for component_one in bank.iter() {
    for component_two in bank.iter() {   
        newBank.append(ExprT::App(component_one, component_two));
    }
  }
  return newBank;
}

pub fn growCtor(bank: Vec<ExprT>, spec: SpecT) -> Vec<ExprT> {
  let mut type_defs: TypeDefinition = spec.td;

  // Find all Variants that are constructors
  let mut ctor_types: Vec((String, T)) = Vec::new();
  for (_, v) in type_defs.iter() {
    match v {
      Variant(vec) => {
        for (s1, t1) in vec.iter(){
          match t1 {
            Named(_) => ctor_types.append((s1, t1)),
            _ => continue,
          }
        }
      },
      _ => continue,
    }
  }

  let mut expressionBank: Vec<ExprT> = Vec::new();
  //Need to do extra pruning since the constructors can only act on certain types based on what is in the 'named'
  for t in ctor_types.iter() {
    for component in bank.iter() {
      match t {
        (s1, Named(_)) => expressionBank.append(ExprT::Ctor(s1, component)),
        _ => continue,
      }
    }
  }
  return expressionBank;
}

pub fn growUnctor(bank: Vec<ExprT>, spec: SpecT) -> Vec<ExprT> {
  let mut type_defs: TypeDefinition = spec.td;

  // Find all Variants that are constructors
  let mut ctor_types: Vec((String, T)) = Vec::new();
  for (_, v) in type_defs.iter() {
    match v {
      Variant(vec) => {
        for (s1, t1) in vec.iter(){
          match t1 {
            Named(_) => ctor_types.append((s1, t1)),
            _ => continue,
          }
        }
      },
      _ => continue,
    }
  }

  let mut expressionBank: Vec<ExprT> = Vec::new();
  //Need to do extra pruning since the constructors can only act on certain types based on what is in the 'named'
  for t in ctor_types.iter() {
    for component in bank.iter() {
      match t {
        (s1, Named(_)) => expressionBank.append(ExprT::Unctor(s1, component)),
        _ => continue,
      }
    }
  }
  return expressionBank;
}

pub fn growEq(bank: Vec<ExprT>, spec: SpecT) -> Vec<ExprT> {
  let mut newBank: Vec<ExprT> = Vec::new();
  for component_one in bank.iter() {
    for component_two in bank.iter() {   
        newBank.append(ExprT::Eq(true, component_one, component_two));
        newBank.append(ExprT::Eq(false, component_one, component_two));
    }
  }
  return newBank;
}

