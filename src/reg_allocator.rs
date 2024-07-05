use std::collections::{HashMap, HashSet};

//registers are really 32 - 255 but it's easier to just go from 0 - 223

type Reg = usize;
/// A temporary value.
type Temp = String;

///Structure for allocating registers to variables.
/// # Allocation Rules
/// - Each temporary gets a node.
/// - An edge exists between two temporary if they exist simultaneously.
struct InterferenceGraph {
  adj_list:HashMap<Temp, HashSet<Temp,>,>,
}

impl InterferenceGraph {
  pub fn new() -> Self {
    InterferenceGraph { adj_list:HashMap::new(), }
  }
}
