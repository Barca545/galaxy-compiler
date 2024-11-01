use crate::interner::lookup;
use std::collections::HashMap;
use std::fmt::Debug;
// Refactor:
// - Might need functionality to roll back current when traversing the tables
//   "graph"
// - Consider changing the interface so I never interact with the children
//   tables directly and only go through the parent. Will depend on how the
//   tables are actually used.
// - Rename the "graph" holding all the tables. SymbolTables is too close to
//   SymbolTable.
// - Should the

// TODO
// - I do not think the types here and in the AST should not be different.

///A graph-like structure which holds all of the apps [`SymbolTable`]s.
struct SymbolTables {
  tables:Vec<SymbolTable,>,
  current:usize,
}

impl SymbolTables {
  pub fn new() -> SymbolTables {
    SymbolTables {
      tables:vec![SymbolTable::new(None, 0,)],
      current:0,
    }
  }

  ///Create and add a new child scope and its [`SymbolTable`].
  pub fn new_scope(&mut self,) -> &mut SymbolTable {
    self.tables.push(SymbolTable::new(Some(self.current,), self.current,),);
    self.current = self.tables.len() - 1;
    self.current_mut()
  }

  ///Returns a reference to the parent of the current [`SymbolTable`].
  pub fn parent(&self,) -> &SymbolTable {
    match &self.tables[self.current].parent {
      Some(parent,) => &self.tables[*parent],
      None => panic!("root has no parent"),
    }
  }

  ///Return a refrence to the current [`SymbolTable`].
  pub fn current(&self,) -> &SymbolTable {
    &self.tables[self.current]
  }

  ///Return a mutable refrence to the current [`SymbolTable`].
  pub fn current_mut(&mut self,) -> &mut SymbolTable {
    &mut self.tables[self.current]
  }
}

#[derive(Debug, Clone, Copy, PartialEq,)]
pub enum DataType {
  Int,
  Float,
  Usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash,)]
pub struct Symbol {
  pub idx:u32,
}

impl Debug for Symbol {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    f.debug_struct("Symbol",).field("value", &lookup(self.idx,),).finish()
  }
}

impl From<u32,> for Symbol {
  fn from(idx:u32,) -> Self {
    Symbol { idx, }
  }
}

#[derive(Debug, Clone, Copy,)]
pub struct SymbolData {
  data_type:DataType,
}
impl SymbolData {
  pub fn new(data_type:DataType,) -> SymbolData {
    SymbolData { data_type, }
  }
}

struct SymbolTable {
  idx:usize,
  attribute_table:HashMap<Symbol, SymbolData,>,
  parent:Option<usize,>,
}

impl SymbolTable {
  pub fn new(parent:Option<usize,>, idx:usize,) -> Self {
    SymbolTable {
      attribute_table:HashMap::new(),
      parent,
      idx,
    }
  }

  pub fn set(&mut self, sym:Symbol, data:SymbolData,) {
    self.attribute_table.insert(sym, data,);
  }

  ///Returns the [`SymbolTable`]'s parent.
  fn parent<'p,>(&self, tables:&'p SymbolTables,) -> &'p SymbolTable {
    match self.parent {
      Some(parent_idx,) => &tables.tables[parent_idx],
      None => panic!("Node {} has no parents.", self.idx),
    }
  }

  ///Get a [`Symbol`]'s information.
  pub fn get(&self, sym:&Symbol, tables:&SymbolTables,) -> SymbolData {
    match self.attribute_table.get(sym,) {
      Some(data,) => *data,
      None => {
        let parent = self.parent(tables,);
        match parent.attribute_table.get(sym,) {
          Some(data,) => *data,
          None => parent.parent(tables,).get(sym, tables,),
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::{Symbol, SymbolData, SymbolTables};
  use crate::{interner::intern, symbol_table::DataType};

  #[test]
  fn create_update_table() {
    let mut tables = SymbolTables::new();
    let table = tables.current_mut();

    let sym = Symbol::from(intern("test",),);
    let data_ty = SymbolData::new(DataType::Usize,);

    table.set(sym, data_ty,);

    let test_table = &tables.tables[0];
    let test_data = test_table.get(&sym, &tables,);

    assert_eq!(test_data.data_type, DataType::Usize);
  }

  #[test]
  fn create_child_scopes() {
    //Create a new scope

    //Create multiple children inside the scope with different attribute tables

    //Create a deeply nested scope and call get on something in the root scope
  }

  #[test]
  fn modify_a_symbol_table_entry() {
    // Create a table

    // Access a table mutably

    // Alter an entry

    // Check it was altered
  }
}
