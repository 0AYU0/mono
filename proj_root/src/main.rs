mod expr;
mod types;
mod specification;
mod bool_impl;
mod generator;
mod typecheck;
// mod egg_test;
use crate::expr::{*};
use crate::expr::ExprT::{*, self};
use crate::bool_impl::{*};
use crate::types::T::{*, self};
use crate::specification::{*};
use crate::generator::{*};
use std::collections::{HashMap, HashSet};
use egg::*;

fn main() {
  let (input_values, desired_type): (T, T) = (get_synth_type().0, get_synth_type().1);
  let mut tc: TypeContext = get_type_context();
  tc.insert(TARGET_FUNC.to_string(), Arrow(Box::new(input_values.clone()), Box::new(desired_type)));
  tc.insert(TARGET_FUNC_ARG.to_string(), input_values.clone());
  let io_examples: Vec<(Value, Value)> = get_synth_examples();
  let spec: SpecT = specification::SpecT::new(get_synth_type(), get_eval_context(), tc, get_type_definition(), get_variant_context(), io_examples);
  
  let mut plist: HashSet<ExprT> = HashSet::from_iter(vec![ExprT::Tuple(Vec::new()), Var(TARGET_FUNC_ARG.to_string()),  Var(TARGET_FUNC.to_string())].iter().cloned());
  let grow_funcs: Vec<fn(&HashSet<ExprT>, &SpecT, i32) -> HashSet<ExprT>> = vec![grow_app, grow_ctor, grow_unctor, grow_eq, grow_tuple, grow_proj];
  let max_depth: i32 = 5;
  let mut satisfying_blocks: Vec<((Value, Value), Vec<ExprT>)> = Vec::new(); 
  let mut program_blocks: HashMap<ExprT, Vec<usize>> = HashMap::new(); 
  let mut obs_eq: HashMap<String, ExprT> = HashMap::new();
  let rules: Vec<Rewrite<ExprLang, ()>> = vec![];
  //print!("{:?}\n", plist);
  let mut curr_depth = 1;
  while curr_depth < max_depth {
    //print!("Iteration: {:?}", curr_depth);
    plist = grow_funcs.iter().fold(plist.clone(), |mut acc, grow_func| {acc.extend(grow_func(&plist, &spec, curr_depth)); acc});
    (plist, satisfying_blocks, program_blocks) = process_spec(&spec, &plist, &mut obs_eq, &rules);
    print!("Plist: {:?}\n\n", plist);
    for block in satisfying_blocks.iter() {
      print!("Satisfying Blocks: IO Example - {:?}, Blocks - {:?}\n\n", block.0, block.1);
    }
    for (expr, pts) in program_blocks.iter() {
      print!("Program Blocks: Expr: {:?}, Points {:?}\n\n", expr, pts);
    }
    let res: Option<ExprT> = grow_match(&spec, program_blocks);
    match res {
      Some(ex) => {
        print!("Found complete program: \n{:?}\n", wrap(spec.clone(), ex));
        break;
      },
      _ => {curr_depth += 1;}
    }
  }

  let mut io_output = Vec::new();
  for i in &spec.spec {
    io_output.push(true);
  }

  print!("Programs that satisfy all IO Examples: {:?}", obs_eq.get(&format!("{:?}", io_output)));
}