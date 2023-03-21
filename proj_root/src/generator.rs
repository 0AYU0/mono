use crate::expr::Value::{*, self};
use crate::expr::ExprT::{*, self};
use crate::expr::{*, self};
use crate::types::{*};
use crate::types::T::{*, self};
use crate::specification::{*};
use crate::typecheck::{*};
use std::collections::{BTreeSet, HashSet, HashMap};

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
            Arrow(arg_ty, _) => {
              for expr2 in bank.iter() {   
                let t2: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, expr2);
                match t2 {
                  Some(r_t2) => { 
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
  //print!("{:?} at iteration {:?}\n", new_bank, curr_depth);
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
  for (s1, (arg_ty, parent_ty)) in variant_context.iter() {
    for expr in bank.iter() {
      let ty: Option<T> = typecheck(&spec.ec, &spec.tc, &spec.td, &spec.vc, expr);
      match ty {
        Some(s_ty) => {
          if *arg_ty == s_ty {
          expression_bank.insert(ExprT::Ctor(s1.to_string(), Box::new(expr.clone())));
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
            expression_bank.insert(ExprT::Unctor(s1.to_string(), Box::new(expr.clone())));
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

pub fn grow_proj(bank: &HashSet<ExprT>, spec: &SpecT, curr_depth: i32) -> HashSet<ExprT> {
  let mut new_bank: HashSet<ExprT> = HashSet::new(); 
  if curr_depth == 1{
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
  for component in bank.iter() {
    match component {
      ExprT::Tuple(vec) => {
        for size in 0..vec.len() {
          new_bank.insert(ExprT::Proj(size.try_into().unwrap(), Box::new(component.clone())));
        }
      }
      _ => ()
    }
  }
  return new_bank;

}

//In the recursive case, may need to add more examples or have less pruning
pub fn grow_match(spec: &SpecT, points: HashMap<ExprT, Vec<usize>>)  {
  let mut input_type = &spec.synth_type.0;
  let mut scrutinees: HashMap<ExprT, T> = HashMap::new();
  let mut match_queue: Vec<ExprT> = Vec::new();
  let mut io_points: HashMap<ExprT, HashMap<String,Vec<usize>>> = HashMap::new(); 
  let io_examples = &spec.spec;

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
    _ => print!("Input type was not a tuple\n") 
  }


  for (scrutinee, _) in scrutinees.iter() {
    let mut pt_vec: HashMap<String, Vec<usize>> = HashMap::new();
    let mut index = 0;
    for (input, _) in io_examples.iter() {
      let test_i = exp_of_value(input.clone()).unwrap();
      let e1 = replace(&TARGET_FUNC_ARG.to_string(), test_i.clone(), scrutinee.clone());
      let result: Option<Value> = evaluate_with_context(spec.ec.clone(), e1.clone());
      match result {
        Some(CtorV(s, _)) => {
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

  for cand in match_queue.iter() {
    match cand {
      ExprT::Match(ex, branch) => {
        for (p, exp) in branch.iter() {
          let pt_map: &HashMap<String,Vec<usize>> = io_points.get(ex).unwrap();
          match p {
            PatternT::Ctor(s, t) => {
              let pts = pt_map.get(s).unwrap();
            },
            _ => print!("p  is not a ctor")
          }
        }
      },
      _ => print!("{:?} not a match!\n", cand),
    }
  }
}

fn replace_branch (cand:ExprT, e: ExprT) {

}

