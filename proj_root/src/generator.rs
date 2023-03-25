use egg::{Pattern, Condition};

use crate::expr::Value::{*, self};
use crate::expr::ExprT::{*, self};
use crate::expr::{*, self};
use crate::types::{*};
use crate::types::T::{*, self};
use crate::specification::{*};
use crate::typecheck::{*};
use std::collections::{BTreeSet, HashSet, HashMap};
use std::hash::Hash;

/* Given a solution expression, wraps around it as recursive  */
pub fn wrap(spec: SpecT, e: ExprT) -> ExprT {
  let (arg_ty, out_ty): (T, T) = spec.synth_type;
  let func = ExprT::Func(Param { p_name: TARGET_FUNC_ARG.to_string(), p_type: arg_ty.clone() }, Box::new(e));
  return ExprT::Fix(TARGET_FUNC.to_string(), T::Arrow(Box::new(arg_ty), Box::new(out_ty)), Box::new(func));
}

pub fn grow_app(bank: &HashSet<ExprT>, spec: &SpecT, curr_depth: i32) -> HashSet<ExprT> {
  //Filters out all arrow functions
  /*let type_context: &mut TypeContext = &mut (spec.tc).clone();
  type_context.retain(|_, v| is_arrow_type((*v).clone()));
  let mut result_ty_arg_tys_arg_expss_set: BTreeSet<(T, Vec<T>, Vec<ExprT>)> = BTreeSet::new();
  for (_, (arg_ty, parent_ty)) in type_context {
    result_ty_arg_tys_arg_expss_set.insert((parent_ty.clone(), vec![arg_ty.clone()], Vec::new()));
  }*/

  let mut new_bank: HashSet<ExprT> = HashSet::new();
  for expr1 in bank.iter() {
    let t1: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, expr1);
      match t1 {
        Some(r_t1) => {
          match r_t1 {

            //Our first expression must be a function T:Arrow()
            Arrow(arg_ty, _) => {
              for expr2 in bank.iter() {   
                let t2: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, expr2);
                match t2 {
                  Some(r_t2) => { 

                    //Our second expression must typecheck to the input argument of the first
                    if *arg_ty == r_t2 {
                      new_bank.insert(ExprT::App(Box::new(expr1.clone()), Box::new(expr2.clone())));
                    }
                  },
                  _ => ()
                }          
              }
            },
            _ => ()
          }
        },
        None => print!("Typecheck failed on: {:?}\n", *expr1),
      } 
  }
  print!("Growing app: {:?}\n", new_bank);
  return new_bank;
}

pub fn grow_ctor(bank: &HashSet<ExprT>, spec: &SpecT, curr_depth: i32) -> HashSet<ExprT> {
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

  let mut expression_bank: HashSet<ExprT> = HashSet::new();
  //Need to do extra pruning since the constructors can only act on certain types based on what is in the 'named'
  for (s1, (arg_ty, parent_ty)) in variant_context.iter() { //Might be a (Tuple, Named), or (Named, Named)
    match arg_ty {
      
      //Typechecking on a constructor of multiple expressions ex. cons(nat, list)
      T::Tuple(vec) => {
        if vec.len() == 0 {
          expression_bank.insert(normalize(&ExprT::Ctor(s1.to_string(), Box::new(ExprT::Tuple(vec![])))));
        } else {
          let mut possibleTwo: Vec<(ExprT, ExprT)> = Vec::new(); //Get all of the possible tuples of expressions
          for expr1 in bank.iter() {
            let ty1: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, expr1);
            match ty1 {
              Some(s_ty) => {
                if vec[0] == s_ty {
                  for expr2 in bank.iter() {
                    let ty2: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, expr2);
                    match ty2 {
                      Some(r_ty) => {
                        if vec[1] == r_ty {
                          possibleTwo.push((expr1.clone(), expr2.clone()));
                        }
                      }
                      None => print!("Typecheck failed on: {:?}\n", *expr1),
                    }
                  }
                }
              }
              None => print!("Typecheck failed on: {:?}\n", *expr1),
            }
          }
          for (first, second) in possibleTwo.iter() {
            expression_bank.insert(normalize(&ExprT::Ctor(s1.to_string(), Box::new(ExprT::Tuple(vec![first.clone(), second.clone()])))));
          }
        }
      }

      //Typechecking on a constructor of a single argument, compared against the argument as a whole
      T::Named(_) => {
        for expr in bank.iter() {
          let ty: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, expr);
          match ty {
            Some(s_ty) => {
              if *arg_ty == s_ty {
                expression_bank.insert(normalize(&ExprT::Ctor(s1.to_string(), Box::new(expr.clone()))));
              }
            }
            None => print!("Typecheck failed on: {:?}\n", *expr),
          }
        }
      },
      _ => print!("Arg type was not tuple or named: {:?}\n", *arg_ty)
      /*match (s1, (arg_ty, parent_ty)) {
        (s1, (arg_ty, parent_ty)) => expression_bank.push(ExprT::Ctor(s1.to_string(), Box::new(component.clone()))),
        _ => continue,
      }*/
    }
  }
  print!("Growing ctor: {:?}\n", expression_bank);
  return expression_bank;
}

pub fn grow_unctor(bank: &HashSet<ExprT>, spec: &SpecT, curr_depth: i32) -> HashSet<ExprT> {
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

  let mut expression_bank: HashSet<ExprT> = HashSet::new();
  //Need to do extra pruning since the constructors can only act on certain types based on what is in the 'named'
  for (s1, (arg_ty, parent_ty)) in variant_context.iter() {
    for expr in bank.iter() {
      let ty: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, expr);
      match ty {
        Some(s_ty) => {
          if s_ty == *parent_ty && !(*arg_ty == T::_unit()) {
            expression_bank.insert(normalize(&ExprT::Unctor(s1.to_string(), Box::new(expr.clone()))));
          }
          //print!("Typecheck worked on: {:?} with {:?}\n", *expr, s_ty)
        },
        None => print!("Typecheck failed on: {:?}\n", *expr),
      }
    }
  }

  // Compare directly with the function argument, since the input may vary - we do not have to typecheck againt the parent
  let ty: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, &ExprT::Var(TARGET_FUNC_ARG.to_string()));
  match ty {
    Some(s_ty) => {
      match s_ty {
        Named(s1) => {
          let td = spec.td.get(&s1).unwrap();
          match td {
            T::Variant(vec) => {
              let test :Vec<&String>= vec.iter().map(|((s2, arg_ty))| {
                if !(*arg_ty == T::_unit()) {
                  expression_bank.insert(normalize(&ExprT::Unctor(s2.to_string(), Box::new(ExprT::Var(TARGET_FUNC_ARG.to_string())))));
                }
                s2
              }).collect();
              print!("Test: {:?}", test)
            },
            _ => ()
          }          
        }
        _ => ()
      }
    },
    None => print!("Typecheck failed"),
  }
  print!("Growing unctor: {:?}\n", expression_bank);
  return expression_bank;
}

/*This may not actually have to do anything - in trio it just returns the exact same 'bank' or mapping of expressions */
pub fn grow_eq(bank: &HashSet<ExprT>, spec: &SpecT, curr_depth: i32) -> HashSet<ExprT> {
  let mut new_bank: Vec<ExprT> = Vec::new();
  for component_one in bank.iter() {
    for component_two in bank.iter() {   
        new_bank.push(ExprT::Eq(true, Box::new(component_one.clone()), Box::new(component_two.clone())));
        new_bank.push(ExprT::Eq(false, Box::new(component_one.clone()), Box::new(component_two.clone())));
    }
  }
  return HashSet::new();
}

//No typechecking, call a helper function - may be bloated since tuples can get continually nested and grow exponentially based on the input depth
pub fn grow_tuple(bank: &HashSet<ExprT>, spec: &SpecT, curr_depth: i32) -> HashSet<ExprT> {
  let mut new_bank: HashSet<ExprT> = HashSet::new();
  let vector_tuples: HashSet<Vec<ExprT>> = grow_tuple_helper(bank, spec, curr_depth, HashSet::from_iter(vec![Vec::new()].iter().cloned())); 
  for vec in vector_tuples.iter() {
    new_bank.insert(ExprT::Tuple(vec.to_vec()));
  }
  return new_bank;
}

pub fn grow_tuple_helper(bank: &HashSet<ExprT>, spec: &SpecT, curr_depth: i32, curr_tuples: HashSet<Vec<ExprT>>) -> HashSet<Vec<ExprT>> {
  if curr_depth == 0 {
    return curr_tuples;
  }
  let mut new_tuples: HashSet<Vec<ExprT>> = HashSet::new();
  for component in bank.iter() {
    for tuple in curr_tuples.iter() {
      let mut t1: Vec<ExprT> = tuple.clone();
      t1.push(component.clone());
      new_tuples.insert(t1);
    }
  }
  return grow_tuple_helper(bank, spec, curr_depth - 1, new_tuples); 
}

// Retrieve an element within a tuple
pub fn grow_proj(bank: &HashSet<ExprT>, spec: &SpecT, curr_depth: i32) -> HashSet<ExprT> {
  let mut new_bank: HashSet<ExprT> = HashSet::new(); 

  // At the base depth, add projections on the input argument
  if curr_depth == 1 {
    let input_ty: &T = &spec.synth_type.0;
    match input_ty {
      T::Tuple(vec) => {
        for size in 0..vec.len() {
          new_bank.insert(ExprT::Proj(size.try_into().unwrap(), Box::new(ExprT::Var(TARGET_FUNC_ARG.to_string()))));
        }
      },
      _ => ()
    }
    return new_bank;
  }

  //If the given component evaluates to some tuple
  for component in bank.iter() {
    let ty = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, &component);
    match ty {
      Some(res) => {
        match res {
            T::Tuple(vec) => {
              for size in 0..vec.len() {
                new_bank.insert(ExprT::Proj(size.try_into().unwrap(), Box::new(component.clone())));
              }
            }
            _ => ()
          }
        },
        _ => ()
      }
    }
  //print!("Growing proj: {:?}\n", new_bank);
  return new_bank;
}

//In the recursive case, may need to add more examples or have less pruning
pub fn grow_match(spec: &SpecT, points: HashMap<ExprT, Vec<usize>>, ty_to_exprs: HashMap<T, HashSet<ExprT>>, complete_matches: &mut Vec<ExprT>) -> Option<Vec<ExprT>> {
  let mut input_type = &spec.synth_type.0;
  let mut scrutinees: HashMap<ExprT, T> = HashMap::new();
  let mut match_queue: Vec<ExprT> = Vec::new();
  let mut io_points: HashMap<ExprT, HashMap<String,Vec<usize>>> = HashMap::new(); 
  let io_examples = &spec.spec;


  // The scrutinee (match parameter) is based on the input
  match input_type {
    T::Tuple(vec) => {
      for i in 0..vec.len() {
        let expr = ExprT::Proj(i as i32, Box::new(ExprT::Var(TARGET_FUNC_ARG.to_string())));
        let arg_ty = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, &expr);
        match arg_ty {
          Some(ty) => {
            scrutinees.insert(expr, ty);
          }
          None => print!("{:?} does not typecheck\n", expr)
        }
      }
    }
    T::Named(x) => {
      scrutinees.insert(ExprT::Var(TARGET_FUNC_ARG.to_string()), input_type.clone());
    }
    _ => print!("Input type was not a tuple\n") 
  }

  // Based on each scrutinee, if it can be an inductive type, add unconstructors on the given input parameter
  let mut available_uncons:HashSet<ExprT> = HashSet::new();
  for (scrutinee, _) in scrutinees.iter() {
    print!("Scrutinees: {:?}\n", scrutinee);  
    let mut pt_vec: HashMap<String, Vec<usize>> = HashMap::new();
    let mut index = 0;
    for (input, _) in io_examples.iter() {
      let test_i = exp_of_value(input.clone()).unwrap();      
      let e1 = replace(&TARGET_FUNC_ARG.to_string(), test_i.clone(), scrutinee.clone());
      //print!("e1: {:?}\n", e1);  
      let result: Option<Value> = evaluate_with_context(spec.ec.clone(), e1.clone());
      //print!("result: {:?}\n", result);  
      match result {
        Some(CtorV(s, _)) => {
          available_uncons.insert(ExprT::Unctor(s.to_string(), Box::new(scrutinee.clone())));
          pt_vec.entry(s)
              .or_insert(Vec::new())
              .push(index);
        }
        //should be some ctorV, sc_ty is the type that the scrutinee evaluates to
        _ => print!("Unable to evaluate {:?}\n", e1) 
      }
      index += 1;
    }
    print!("Projection: {:?}, Points: {:?}\n", scrutinee, pt_vec);

    match_queue.push(ExprT::Match(Box::new(scrutinee.clone()), pt_vec.keys().map(|s| (PatternT::Ctor(s.to_string(), Box::new(PatternT::Wildcard)), ExprT::Wildcard)).collect()));
    io_points.insert(scrutinee.clone(), pt_vec);
  }
  print!("Available Uncons: {:?}\n", available_uncons);
  //print!("ty_to_exprs: {:?}\n", ty_to_exprs);
  let mut completePrograms: HashSet<ExprT> = HashSet::new();
  while !match_queue.is_empty() {
    let mut valid_expr_map: HashMap<String, Vec<ExprT>> = HashMap::new();
    let mut candidate: Option<ExprT> = match_queue.pop();
    print!("Candidate: {:?}\n", candidate);
    let test1 = candidate.clone().unwrap();
    
    match candidate {
      Some(un_cand) => match un_cand {
        ExprT::Match(ex, branch) => {
          let clonedBranch = branch.clone();
          for (pattern, hole) in clonedBranch.iter() {
            if(is_wildcard(hole.clone())){
              let pt_map: &HashMap<String,Vec<usize>> = io_points.get(&ex).unwrap();
              match pattern {
                PatternT::Ctor(s, t) => {
                    let pts: Vec<usize> = pt_map.get(s).unwrap().to_vec();
                    let mut valid_exprs:Vec<ExprT> = fill_match_holes(&pts,& points);
                    let mut scrut: HashSet<ExprT> = HashSet::new();
                    let temp = scrutinees.iter().map(|(s,_)| scrut.insert(s.clone()));
                    let mut rec_expr: HashSet<ExprT> = get_valid_recursive_components(&spec.synth_type.1, &ty_to_exprs, &available_uncons, &scrut);
                    let mut rec_vec = Vec::from_iter(rec_expr).clone();
                    print!("Rec Exprs: {:?}\n", rec_vec);
                    valid_exprs.append(&mut rec_vec);
                    valid_exprs.append(complete_matches);
                    for index in 0..valid_exprs.len(){
                    let newCandidate = replace_branch(test1.clone(), valid_exprs[index].clone(), pattern.clone());
                    if(is_match_complete(newCandidate.clone())) {
                      completePrograms.insert(newCandidate.clone());
                    } else {
                      match_queue.push(newCandidate.clone())    
                    }      
                  }             
                },
                _ => print!("p is not a ctor")
              }
            }            
          }
        },
        _ => print!("{:?} not a match!\n", un_cand.clone()),
      },
      _ => print!("Pop from match queue failed")
    }
  }
  if(!completePrograms.is_empty()){
    return Some(Vec::from_iter(completePrograms).clone());
  } else {
    return None;
  }
}

// Given a set of input and output points, return a set of expressions that satisfy each of the inputs in the match branch
fn fill_match_holes(input_pts: &Vec<usize>, output_pts: &HashMap<ExprT, Vec<usize>>) -> Vec<ExprT>{
  let mut satisfying_comp: Vec<ExprT> = Vec::new();
  let input_pt_set: HashSet<usize> = input_pts.iter().copied().collect();
  for (expr, output_pt) in output_pts.iter() {
    if(input_pt_set.iter().all(|item| output_pt.contains(item))){
      satisfying_comp.push(expr.clone());
    }
  }
  return satisfying_comp;
}


//Given a candidate match statement, and a pattern to match, return a new match statement with the inserted expression on that branch
fn replace_branch (candidate: ExprT, e: ExprT, pattern: PatternT) -> ExprT {
  let mut newVec = Vec::new();
  match candidate {
    ExprT::Match(scrutinee, vec) => {
      let mut first = true;
      for (pat, expr) in vec {
        if pattern == pat && is_wildcard(expr.clone()){
          first = false;
          newVec.push((pat, e.clone()));
        } else {
          newVec.push((pat, expr.clone()));
        }
      }
      return ExprT::Match(scrutinee.clone(), newVec);
    }
    _ => {
      print!("New candidate is not a vector");
      ExprT::Wildcard
    }
  }
}

fn is_match_complete(candidate: ExprT) -> bool {
  match candidate {
    ExprT::Match(_, vec) => {
      for (_, expr) in vec.iter() {
        if is_wildcard(expr.clone()) {
          return false;
        }
      }
    }
    _ => return false
  }
  return true;
}

pub fn get_valid_recursive_components(
  desired_ty: &T,
  ty_to_exprs: &HashMap<T, HashSet<ExprT>>,
  available_uncons: &HashSet<ExprT>,
  scrutinees: &HashSet<ExprT>,
) -> HashSet<ExprT> {
  let empty = HashSet::new();
  let exprs = ty_to_exprs.get(&desired_ty).unwrap_or(&empty);
  exprs
      .iter()
      .filter(|e| count_recursions(*e) > 0)
      .filter(|e| using_allowed_unconstructor(*e, available_uncons))
      .filter(|e| {
          let call_exprs = get_recursive_calls(&e);
          call_exprs.iter().all(|call| match call {
            ExprT::App(_, arg_exp) => {
                  let es = match *arg_exp.clone() {
                      ExprT::Tuple(es) => es,
                      _ => vec![*arg_exp.clone()],
                  };
                  es.iter().all(|e| {
                      // argument contains unconstructor: termination guaranteed
                      !get_unconstructors(e).is_empty()
                          || 
                          !scrutinees.contains(e)
                  })
              }
              _ => unreachable!(),
          })
      })
      .cloned()
      .collect()
}