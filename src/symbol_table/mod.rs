pub mod sym_visitor;
pub mod symbol_table;

use crate::{
  ast::{Literal, Ty, P},
  interner::{intern, lookup},
};
use std::fmt::{Debug, Display};

// Refactor:
// - Is there a reason for SymTy and Ty to be different?

#[derive(Clone, Copy, PartialEq, Eq, Hash,)]
pub struct Symbol {
  pub idx:u32,
}

impl Debug for Symbol {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    Display::fmt(&self, f,)
  }
}

impl Display for Symbol {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    write!(f, "Symbol({})", &lookup(self.idx,))
  }
}

impl From<u32,> for Symbol {
  fn from(idx:u32,) -> Self {
    Symbol { idx, }
  }
}

impl From<Symbol,> for u32 {
  fn from(value:Symbol,) -> Self {
    value.into()
  }
}

impl From<&str,> for Symbol {
  fn from(value:&str,) -> Self {
    Symbol { idx:intern(value,), }
  }
}

impl From<String,> for Symbol {
  fn from(value:String,) -> Self {
    Symbol { idx:intern(value.as_str(),), }
  }
}

impl From<Literal,> for Symbol {
  fn from(value:Literal,) -> Self {
    match value {
      Literal::Bool(b,) => Symbol { idx:intern(&b.to_string(),), },
      Literal::Integer(i,) => Symbol { idx:intern(&i.to_string(),), },
      Literal::Float(f,) => Symbol { idx:intern(&f.to_string(),), },
      Literal::Usize(u,) => Symbol { idx:intern(&u.to_string(),), },
      Literal::Str(symbol,) => symbol,
      // Literal::DefId(_,) => panic!("Cannot make a symbol from a DefId"),
    }
  }
}

#[derive(Clone, PartialEq, Eq,)]
pub enum SymTy {
  Int,
  Float,
  Usize,
  Str,
  Bool,
  Array(P<SymTy,>,),
  /// A function and its return type.
  Fn(Option<P<SymTy,>,>,),
}

impl SymTy {
  ///Creates a new [`Ty::Array`] containing the passed `Ty`.
  pub fn new_array(ty:SymTy,) -> Self {
    SymTy::Array(P::new(ty,),)
  }
}

impl From<Ty,> for SymTy {
  fn from(ty:Ty,) -> Self {
    match ty {
      Ty::Int => SymTy::Int,
      Ty::Float => SymTy::Float,
      Ty::Usize => SymTy::Usize,
      Ty::Str => SymTy::Str,
      Ty::Bool => SymTy::Bool,
      Ty::Array(p,) => SymTy::Array(P::new(SymTy::from(p.copy(),),),),
    }
  }
}

impl From<Literal,> for SymTy {
  fn from(value:Literal,) -> Self {
    match value {
      Literal::Bool(_,) => Self::Bool,
      Literal::Integer(_,) => Self::Int,
      Literal::Float(_,) => Self::Float,
      Literal::Usize(_,) => Self::Usize,
      Literal::Str(_,) => Self::Str,
      // Literal::DefId(_,) => panic!("DefId is not a type"),
    }
  }
}

impl From<&Ty,> for SymTy {
  fn from(ty:&Ty,) -> Self {
    match ty {
      Ty::Int => SymTy::Int,
      Ty::Float => SymTy::Float,
      Ty::Usize => SymTy::Usize,
      Ty::Str => SymTy::Str,
      Ty::Bool => SymTy::Bool,
      Ty::Array(p,) => SymTy::Array(P::new(SymTy::from(p.copy(),),),),
    }
  }
}

impl Display for SymTy {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    match self {
      Self::Int => write!(f, "Int"),
      Self::Float => write!(f, "Float"),
      Self::Usize => write!(f, "Usize"),
      Self::Str => write!(f, "Str"),
      Self::Bool => write!(f, "Bool"),
      Self::Array(ty,) => write!(f, "Array<{}>", **ty),
      Self::Fn(ty,) => match ty {
        Some(ty,) => write!(f, "Fn -> {}", **ty),
        None => write!(f, "Fn"),
      },
    }
  }
}

impl Debug for SymTy {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    Display::fmt(&self, f,)
  }
}
