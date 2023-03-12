use std::cmp::Ordering;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum T {
  Named(String),
  Arrow(Box<T>, Box<T>),
  Tuple(Vec<T>),
  Variant(Vec<(String, T)>)
}

impl T {
    fn _unit() -> T {
        T::Tuple(vec![])
    }

    fn _t() -> T {
        T::Named("t".to_string())
    }

    fn _bool() -> T {
        T::Named("bool".to_string())
    }

    fn _nat() -> T {
        T::Named("nat".to_string())
    }
}

fn cmp_tuple(t1: T, t2: T) -> Option<std::cmp::Ordering> {
    match (t1, t2) {
        (T::Tuple(v1), T::Tuple(v2)) => if v1.is_empty() {
            Some(Ordering::Less)
        } else if v2.is_empty() {
            Some(Ordering::Greater)
        } else {
            None
        }
        
        _ => None,
    }
}

type Variants = Vec<(String, T)>;


fn is_named_type(ty: T) -> bool { 
	match ty {
        T::Named(_) => true, 
        _ => false,
    }
}
	
fn is_arrow_type(ty: T) -> bool { 
	match ty {
        T::Arrow(_, _) => true,
        _ => false,
    }
}

fn is_tuple_type(ty: T) -> bool { 
	match ty { 
        T::Tuple(_) => true,
        _ => false,
    }
}

fn is_variant_type(ty: T) -> bool { 
	match ty { 
        T::Variant(_) => true,
        _ => false,
    }
}
