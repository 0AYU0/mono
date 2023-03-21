mod expr;
mod types;
mod specification;
mod bool_band;
mod generator;
mod typecheck;
use crate::expr::{*};
use crate::expr::ExprT::{*, self};
use crate::bool_band::{*};
use crate::types::T::{*, self};
use crate::specification::{*};
use crate::generator::{*};
use std::collections::{HashMap, HashSet};

fn main() {
  //Initialize desired type and original type context ('f', 'x', [])
  let (input_values, desired_type): (T, T) = (get_synth_type().0, get_synth_type().1);
  let mut tc: TypeContext = get_type_context();
  tc.insert(TARGET_FUNC.to_string(), Arrow(Box::new(input_values.clone()), Box::new(desired_type)));
  tc.insert(TARGET_FUNC_ARG.to_string(), input_values.clone());

  //Grab IO examples and synthesis constraint
  let io_examples: Vec<(Value, Value)> = get_synth_examples();
  let spec: SpecT = specification::SpecT::new(get_synth_type(), get_eval_context(), tc, get_type_definition(), get_variant_context(), io_examples);
  
  //Map expressions to satisfying IO examples and components
  let mut plist: HashSet<ExprT> = HashSet::from_iter(vec![ExprT::Tuple(Vec::new()), Var(TARGET_FUNC_ARG.to_string()),  Var(TARGET_FUNC.to_string())].iter().cloned());
  let grow_funcs: Vec<fn(&HashSet<ExprT>, &SpecT, i32) -> HashSet<ExprT>> = vec![grow_app, grow_ctor, grow_unctor, grow_eq, grow_proj];
  let mut satisfying_blocks: Vec<((Value, Value), Vec<ExprT>)> = Vec::new(); 
  let mut program_blocks: HashMap<ExprT, Vec<usize>> = HashMap::new(); 
  let mut obs_eq: HashMap<String, ExprT> = HashMap::new();
  let mut ty_to_exprs: HashMap<T, HashSet<ExprT>> = HashMap::new();
  let mut complete_matches: Vec<ExprT> = Vec::new();

  // Set depth
  let mut curr_depth = 1;
  let max_depth: i32 = 5;

  // Core bottom up enumeration
  while curr_depth < max_depth {
    plist = grow_funcs.iter().fold(plist.clone(), |mut acc, grow_func| {acc.extend(grow_func(&plist, &spec, curr_depth)); acc});
    (plist, satisfying_blocks, program_blocks, ty_to_exprs) = process_spec(&spec, &plist, &mut obs_eq);
    print!("Plist: {:?}\n\n", plist);
    for block in satisfying_blocks.iter() {
      print!("Satisfying Blocks: IO Example - {:?}, Blocks - {:?}\n\n", block.0, block.1);
    }
    for (expr, pts) in program_blocks.iter() {
      print!("Program Blocks: Expr: {:?}, Points {:?}\n\n", expr, pts);
    }

    // Add top down propogation for match expressions
    let res: Option<Vec<ExprT>> = grow_match(&spec, program_blocks, ty_to_exprs, &mut complete_matches.clone());
    match res {
      Some(ex) => {
        let matchesClone = ex.clone();
        complete_matches = ex;
        print!("Found complete program: \n{:?}\n", wrap(spec.clone(), matchesClone.iter().next().unwrap().clone()));
        return;
      },
      _ => {curr_depth += 1;}
    }
  }

  // Does a non match expression satisfy spec?
  let mut io_output = Vec::new();
  for _ in &spec.spec {
    io_output.push(true);
  }
  print!("Programs that satisfy all IO Examples: {:?}", obs_eq.get(&format!("{:?}", io_output)));
}