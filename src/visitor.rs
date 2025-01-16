use crate::ast::{Block, Expression, Func, Pat, RawIdent, Statement};

pub trait Visitor {
  type Result;

  fn visit_expr(&mut self, expr:&Expression,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_expr_mut(&mut self, expr:&mut Expression,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_stmt(&mut self, stmt:&Statement,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_stmt_mut(&mut self, stmt:&mut Statement,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_pat(&mut self, pat:&Pat,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_pat_mut(&mut self, pat:&mut Pat,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_ident(&mut self, ident:&RawIdent,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_ident_mut(&mut self, ident:&mut RawIdent,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_block(&mut self, block:&Block,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_block_mut(&mut self, block:&mut Block,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_func(&mut self, func:&Func,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }

  fn visit_func_mut(&mut self, func:&mut Func,) -> Self::Result {
    panic!("Not implemented for this Visitor")
  }
}

pub trait Visitable<V,> {
  fn visit_with(&self, visitor:&mut V,);
}
