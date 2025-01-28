use super::{SymTy, Symbol};
use std::fmt::{Debug, Display};
use std::ops::Index;

// Refactor:
// - I think the types here and in the AST should not be different. Is there
//   anyway to unify them?
// - Give LocalDecl a better definition.
// - All the functions to return the information inside LocalDecl could probably
//   return refrences

#[derive(Debug, Clone,)]
///A struct containing the [`declaration information`](LocalDecl) for all the
/// locals in the app.
pub struct SymbolTable {
  vars:Vec<LocalDecl,>,
  temps:Vec<LocalDecl,>,
}

impl SymbolTable {
  /// Creates a new `SymbolTable`.
  pub fn new() -> Self {
    SymbolTable {
      vars:Vec::new(),
      temps:Vec::new(),
    }
  }

  ///Returns the [`LocalDecl`] for a given [`DefId`] in the `vars` field.
  pub fn lookup(&self, id:DefId,) -> &LocalDecl {
    &self.vars[id]
  }

  ///Returns the [`LocalDecl`] for a given [`DefId`] in the `temps` field.
  pub fn lookup_temp(&self, id:DefId,) -> &LocalDecl {
    &self.temps[id]
  }

  ///Adds a new [`LocalDecl`] to the `SymbolTable`s current scope.
  pub fn insert(&mut self, sym:Symbol, ty:SymTy, mutable:bool,) -> DefId {
    let id = DefId(self.vars.len(),);
    // let scope = self.current;
    let var = LocalDecl::new(id, ty, sym, mutable,);

    self.vars.push(var,);
    id
  }

  /// Adds a new temporary to the `Temps` field.
  pub fn new_temp(&mut self, ty:SymTy,) {
    let id = DefId(self.temps.len(),);
    let temp = LocalDecl::new(id, ty, Symbol::from(format!("t_{}", id.0),), false,);
    self.temps.push(temp,);
  }

  ///Returns the declaration of the last temporary added.
  pub fn last_temp(&self,) -> &LocalDecl {
    self.temps.last().unwrap()
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

#[derive(Debug, PartialEq, Clone,)]
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
  pub fn new(id:DefId, ty:SymTy, name:Symbol, mutable:bool,) -> Self {
    Self { id, name, mutable, ty, }
  }

  /// Returns the [`DefId`].
  pub fn id(&self,) -> DefId {
    self.id
  }

  /// Returns the name as a [`Symbol`].
  pub fn name(&self,) -> Symbol {
    self.name
  }

  /// Returns a boolean indicating whether the local is mutable.
  pub fn mutable(&self,) -> bool {
    self.mutable
  }

  /// Returns the local's [type](SymTy).
  pub fn ty(&self,) -> SymTy {
    self.ty.clone()
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
