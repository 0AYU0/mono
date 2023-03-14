mod expr;
mod types;
mod specification;
mod bool_band;
use crate::expr::TARGET_FUNC_ARG;
use crate::expr::ExprT::{*, self};
use crate::bool_band::{*};

fn main() {
  let mut plist: Vec<ExprT> = vec![Var(TARGET_FUNC_ARG.to_string()), true_(), false_()];
  let input_values = get_synth_type().0;
  let desired_type = get_synth_type().1;
}
