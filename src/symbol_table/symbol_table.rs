use super::{SymTy, Symbol};
use std::fmt::{Debug, Display};
use std::ops::Index;

// Refactor:
// - I think the types here and in the AST should not be different. Is there
//   anyway to unify them?
// - Give LocalDecl a better definition.

#[derive(Debug,)]
///A struct containing the [`declaration information`](LocalDecl) for all the
/// locals in the app.
pub struct SymbolTable {
  vars:Vec<LocalDecl,>,
}

impl SymbolTable {
  /// Creates a new `SymbolTable`.
  pub fn new() -> Self {
    SymbolTable { vars:Vec::new(), }
  }

  ///Returns the [`LocalDecl`] for a given [`DefId`].
  pub fn lookup(&self, id:DefId,) -> &LocalDecl {
    &self.vars[id]
  }

  ///Adds a new [`LocalDecl`] to the `SymbolTable`s current scope.
  pub fn insert(&mut self, sym:Symbol, ty:SymTy, mutable:bool,) -> DefId {
    let id = DefId(self.vars.len(),);
    // let scope = self.current;
    let var = LocalDecl::new(id, ty, sym, mutable,);

    self.vars.push(var,);
    id
  }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy,)]
///A [newtype](https://doc.rust-lang.org/rust-by-example/generics/new_types.html) wrapping a [`LocalDecl`]'s index in the [`SymbolTable`].
pub struct DefId(pub usize,);

impl Index<DefId,> for Vec<LocalDecl,> {
  type Output = LocalDecl;

  fn index(&self, index:DefId,) -> &Self::Output {
    &self[index.0]
  }
}

impl Debug for DefId {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    Display::fmt(&self, f,)
  }
}

impl Display for DefId {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    write!(f, "DefId({})", self.0)
  }
}

#[derive(Debug, PartialEq,)]
pub struct LocalDecl {
  id:DefId,
  ///The user-defined name of the variable.
  name:Symbol,
  ///Indicates whether the variable is mutatable.
  mutable:bool,
  ty:SymTy,
}

impl LocalDecl {
  ///Creates a new [`LocalDecl`].
  fn new(id:DefId, ty:SymTy, name:Symbol, mutable:bool,) -> Self {
    Self { id, name, mutable, ty, }
  }
}

#[cfg(test)]
mod test {
  use super::{DefId, Symbol, SymbolTable};
  use crate::{
    interner::intern,
    symbol_table::{symbol_table::LocalDecl, SymTy},
  };

  #[test]
  fn create_and_update_table() {
    let mut table = SymbolTable::new();

    let sym = Symbol::from(intern("var",),);
    let varid = table.insert(sym, SymTy::new_array(SymTy::Usize,), false,);
    let fetched_var = table.lookup(varid,);

    //Check the fetched var matches the expected data
    assert_eq!(
      fetched_var,
      &LocalDecl {
        id:DefId(0,),
        name:sym,
        mutable:false,
        ty:SymTy::new_array(SymTy::Usize,),
      }
    );

    //Test it can hold two vars
    let sym = Symbol::from(intern("var_2",),);
    let varid_2 = table.insert(sym, SymTy::Usize, false,);
    let fetched_var_2 = table.lookup(varid_2,);

    //Check the fetched var matches the expected daya
    assert_eq!(
      fetched_var_2,
      &LocalDecl {
        id:DefId(1,),
        name:sym,
        mutable:false,
        ty:SymTy::Usize,
      }
    );

    //Test shadowing works
    let sym = Symbol::from(intern("var",),);
    let varid_3 = table.insert(sym, SymTy::Bool, false,);
    let fetched_var_3 = table.lookup(varid_3,);

    assert_eq!(
      fetched_var_3,
      &LocalDecl {
        id:DefId(2,),
        name:sym,
        mutable:false,
        ty:SymTy::Bool,
      }
    );
  }
}
