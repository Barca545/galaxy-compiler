use crate::ast::{Block, Expression, Func, IdentInner, Pat, Statement};

pub trait Visitor {
  type Result;

  #[allow(unused)]
  fn visit_expr(&mut self, expr:&Expression,) -> Self::Result {
    panic!("`visit_expr` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_expr_mut(&mut self, expr:&mut Expression,) -> Self::Result {
    panic!("`visit_expr_mut` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_stmt(&mut self, stmt:&Statement,) -> Self::Result {
    panic!("`visit_stmt` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_stmt_mut(&mut self, stmt:&mut Statement,) -> Self::Result {
    panic!("`visit_stmt_mut` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_pat(&mut self, pat:&Pat,) -> Self::Result {
    panic!("`visit_pat` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_pat_mut(&mut self, pat:&mut Pat,) -> Self::Result {
    panic!("`visit_pat_mut` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_ident(&mut self, ident:&IdentInner,) -> Self::Result {
    panic!("`visit_ident` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_ident_mut(&mut self, ident:&mut IdentInner,) -> Self::Result {
    panic!("`visit_ident_mut` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_block(&mut self, block:&Block,) -> Self::Result {
    panic!("`visit_block` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_block_mut(&mut self, block:&mut Block,) -> Self::Result {
    panic!("`visit_block_mut` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_func(&mut self, func:&Func,) -> Self::Result {
    panic!("`visit_func` not implemented for this Visitor")
  }

  #[allow(unused)]
  fn visit_func_mut(&mut self, func:&mut Func,) -> Self::Result {
    panic!("`visit_func_mut` not implemented for this Visitor")
  }
}

pub trait Visitable<V,> {
  fn visit_with(&mut self, visitor:&mut V,);
}
