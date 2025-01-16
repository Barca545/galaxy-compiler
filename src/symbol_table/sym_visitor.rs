use super::{
  symbol_table::{DefId, SymbolTable},
  SymTy, Symbol,
};
use crate::{
  ast::{AbstractSyntaxTree, Block, Expression, ExpressionKind, Func, Pat, PatKind, RawIdent, Statement, StatementKind, P},
  errors::SymbolGenerationErrors,
  visitor::{Visitable, Visitor},
};
use std::{
  collections::HashMap,
  fmt::{Debug, Display},
  ops::Index,
};

// Refactor:
// - I think everytime the visitor enters a block it should create a new scope.
//   Add a new visit block method to the trait.
// - How does a symbol table handle functions come to think of it
// - Add errors that bubble up if a problem occurs (I think this basically just
//   boils down to an unrecognized identifier)

//Visit expressions and replace instances of a variable with the DefId
//It's possible this will necessitate an IR, if so I'll need to look into what
// a control-flow graph is
// If I want to avoid making a new IR just for this, I could make identites
// contain like a trait that allows it to hold a RawIdent or DefId
// I think it would just be a tag like how Sized works
// Trait InnerIdent
// Either that or make RawIdents an Enum...probably this one since it is more
// straightforward

impl Visitable<SymbolTableBuilder,> for AbstractSyntaxTree {
  fn visit_with(&self, visitor:&mut SymbolTableBuilder,) {
    for stmt in &self.statements {
      visitor.visit_stmt(stmt,);
    }
  }
}

#[derive(Debug,)]
///A [`Visitor`] that walks the AST and builds a [`SymbolTable`].
pub struct SymbolTableBuilder {
  // ///Maps a symbol to its [`DefId`]. Updates when a new
  // /// [`VarDecl`](super::symbol_table::VarDecl) is encountered.
  // symbol_alises:HashMap<Symbol, DefId,>,
  scopes:Vec<Scope,>,
  table:SymbolTable,
}

impl SymbolTableBuilder {
  fn new() -> Self {
    SymbolTableBuilder {
      // symbol_alises:HashMap::new(),
      scopes:vec![Scope::new(ScopeId(0,), None,)],
      table:SymbolTable::new(),
    }
  }

  fn insert(&mut self, sym:Symbol, ty:SymTy, mutable:bool,) {
    let id = self.table.insert(sym, ty, mutable,);
    self.scopes.last_mut().unwrap().symbol_alises.insert(sym, id,);
  }

  ///Enters a child [scope](Scope) of the current scope.
  /// If the Script is currently inside another scope the newly
  /// created scope's will contain the previous scope as its parent.
  pub fn new_scope(&mut self, has_parent:bool,) {
    let parent = if has_parent { Some(ScopeId(self.scopes.len() - 1,),) } else { None };
    let id = ScopeId(self.scopes.len(),);
    let scope = Scope::new(id, parent,);
    self.scopes.push(scope,);
  }

  ///Returns to the previous scope.
  pub fn exit_scope(&mut self,) {
    self.scopes.pop();
  }

  /// Returns the [`DefId`] for a given [`Symbol`].
  pub fn lookup(&self, sym:Symbol,) -> DefId {
    // Traverse the scope stack backwards beginning with the current scope until a
    // symbol's alias is found
    let mut scopeid = Some(self.scopes.last().unwrap().id,);

    while scopeid.is_some() {
      let scope = &self.scopes[scopeid.unwrap()];

      if let Some(id,) = scope.symbol_alises.get(&sym,) {
        return *id;
      }
      scopeid = scope.parent;
    }

    // Error it no alias is found
    // Produce an error if an undeclared variable is encountered
    // print!("{}", );
    panic!("{}", SymbolGenerationErrors::UnrecognizedIdentifier { symbol:sym, });
  }
}

impl Visitor for SymbolTableBuilder {
  type Result = ();

  /// Visit the [`Statement`]s in the script. Record variable and function
  /// declarations in a [`SymbolTable`] and replace instances of identies with
  /// the corresponding [`DefId`].
  fn visit_stmt(&mut self, stmt:&Statement,) {
    // Check if the statement is a let statement
    match &stmt.kind {
      StatementKind::Let(local,) => {
        if let PatKind::Ident(ref ident,) = local.pat.kind {
          // Insert the VarDecl into the symbol table
          let id = self.table.insert(ident.symbol, SymTy::from(local.ty.clone().unwrap().copy(),), local.mutable,);
          // Store the ID indexed by symbol
          self.scopes.last_mut().unwrap().symbol_alises.insert(ident.symbol, id,);
        }
      }
      StatementKind::Expression(expr,) => self.visit_expr(expr,),
      StatementKind::Func(func,) => self.visit_func(func,),
    }
  }

  fn visit_expr(&mut self, expr:&Expression,) -> Self::Result {
    // Call visit_expr recursively down to check for any Identies
    match &expr.kind {
      ExpressionKind::Ident(ident,) => self.visit_ident(ident,),
      ExpressionKind::BinOp(lhs, _, rhs,) => {
        self.visit_expr(lhs,);
        self.visit_expr(rhs,);
      }
      ExpressionKind::Unary(_, expr,) => self.visit_expr(expr,),
      ExpressionKind::If(cond, block_1, block_2,) => {
        self.visit_expr(cond,);

        self.visit_block(block_1,);

        if let Some(block_2,) = block_2 {
          self.visit_block(block_2,);
        }
      }
      ExpressionKind::Assign(var, val,) => {
        self.visit_expr(var,);
        self.visit_expr(val,);
      }
      ExpressionKind::AssignOp(var, _, val,) => {
        self.visit_expr(var,);
        self.visit_expr(val,);
      }
      ExpressionKind::Tuple(tup,) => {
        for expr in tup {
          self.visit_expr(expr,);
        }
      }
      ExpressionKind::WhileLoop(con, body,) => {
        self.visit_expr(con,);
        self.visit_block(body,);
      }
      ExpressionKind::ForLoop { pat, iter, body, } => {
        self.visit_pat(pat,);
        self.visit_expr(iter,);
        self.visit_block(body,);
      }
      ExpressionKind::Range { right, left, .. } => {
        self.visit_expr(right,);
        self.visit_expr(left,)
      }
      _ => {}
    }
  }

  ///Replace [`ExpressionKind::Ident`] with with its [`DefId`] or return an
  /// error if the variable has not been declared.
  fn visit_ident(&mut self, ident:&RawIdent,) -> Self::Result {
    let id = self.lookup(ident.symbol,);
    // Replace the Ident with the DefId

    // There should be some way to replace the current let branch so its logic
    // is handled here
  }

  fn visit_pat(&mut self, pat:&Pat,) -> Self::Result {
    match &pat.kind {
      PatKind::Ident(ident,) => self.visit_ident(ident,),
      PatKind::Range { start, end, .. } => {
        self.visit_expr(start,);
        self.visit_expr(end,);
      }
      PatKind::Tuple(pats,) => {
        for pat in pats.as_slice() {
          self.visit_pat(&pat,);
        }
      }
    }
  }

  fn visit_block(&mut self, block:&Block,) -> Self::Result {
    self.new_scope(true,);
    for stmt in &block.inner_block {
      self.visit_stmt(stmt,)
    }
    self.exit_scope();
  }

  fn visit_func(&mut self, func:&Func,) -> Self::Result {
    // Insert the function as a locl into the current scope
    match &func.sig.result {
      Some(ret,) => self.insert(func.name.symbol, SymTy::Fn(Some(P::new(SymTy::from(ret,),),),), false,),
      None => self.insert(func.name.symbol, SymTy::Fn(None,), false,),
    }

    self.new_scope(false,);

    // Insert the function arguments as locals
    for arg in &func.sig.params {
      // Insert the VarDecl into the symbol table
      let id = self.table.insert(arg.0, SymTy::from(arg.1.copy(),), false,);
      // Store the ID indexed by symbol
      self.scopes.last_mut().unwrap().symbol_alises.insert(arg.0, id,);
    }

    // Handle any declarations or identity usage in the function body
    for stmt in &func.body.inner_block {
      self.visit_stmt(stmt,)
    }
    self.exit_scope();
  }
}

struct Scope {
  ///The scope's [`ScopeId`]. Mainly useful for debugging.
  id:ScopeId,
  ///Maps a symbol to its [`DefId`]. Updates when a new
  /// [`LocalDecl`](super::symbol_table::LocalDecl) is encountered.
  symbol_alises:HashMap<Symbol, DefId,>,
  parent:Option<ScopeId,>,
}

impl Scope {
  ///Creates new [`Scope`] from a given parent's [`ScopeId`].
  fn new(id:ScopeId, parent:Option<ScopeId,>,) -> Self {
    Scope {
      id,
      symbol_alises:HashMap::new(),
      parent,
    }
  }
}

impl Debug for Scope {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    Display::fmt(&self, f,)
  }
}
impl Display for Scope {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    write!(
      f,
      "Scope {{\n id:{},\n symbol_alises:{:?},\n parent:{:?} \n }}",
      self.id, self.symbol_alises, self.parent
    )
  }
}

#[derive(Clone, Copy, PartialEq, Eq,)]
/// A [newtype](https://doc.rust-lang.org/rust-by-example/generics/new_types.html) wrapping a [`Scope`]'s index in the [`SymbolTable`].
struct ScopeId(usize,);

impl Index<ScopeId,> for Vec<Scope,> {
  type Output = Scope;

  fn index(&self, index:ScopeId,) -> &Self::Output {
    &self[index.0]
  }
}

impl Debug for ScopeId {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    Display::fmt(&self, f,)
  }
}

impl Display for ScopeId {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    write!(f, "ScopeId({})", self.0)
  }
}

#[cfg(test)]
mod test {
  use super::SymbolTableBuilder;
  use crate::{
    parser::Parser,
    symbol_table::{symbol_table::DefId, SymTy, Symbol},
    visitor::Visitable,
  };
  use std::panic;

  #[test]
  fn lookup_returns_correct_def_id() {
    // Confirm it looks up stuff in the current scope
    let mut builder = SymbolTableBuilder::new();
    let sym_1 = Symbol::from("var_1",);
    let sym_2 = Symbol::from("var_2",);
    // let sym_3 = Symbol::from("var_3",);

    builder.insert(sym_1, SymTy::Str, true,);

    builder.new_scope(true,);
    builder.new_scope(true,);
    builder.new_scope(true,);

    builder.insert(sym_2, SymTy::Int, true,);

    // Confirm lookup works for symbols defined in a parent scope
    assert_eq!(DefId(0), builder.lookup(sym_1,));

    // Confirm lookup works for symbols stored in the current scope
    assert_eq!(DefId(1), builder.lookup(sym_2,));

    // Confirm the symbols are in the correct scope
    assert_eq!(builder.scopes[0].symbol_alises.get(&sym_1).unwrap(), &DefId(0));
    assert_eq!(builder.scopes[3].symbol_alises.get(&sym_2).unwrap(), &DefId(1));
  }

  #[test]
  fn lookup_fails_for_orphaned_scopes() {
    let mut builder = SymbolTableBuilder::new();
    let sym_1 = Symbol::from("var_1",);
    let sym_2 = Symbol::from("var_2",);

    builder.insert(sym_1, SymTy::Float, false,);
    builder.new_scope(true,);
    builder.new_scope(false,);
    builder.insert(sym_2, SymTy::Bool, true,);

    // Confirm lookup succeeds for symbols defined in the current orphaned scope
    assert_eq!(DefId(1), builder.lookup(sym_2,));

    // Confirm lookup fails for symbols defined in higher level scopes
    let result = catch_unwind_silent(|| builder.lookup(sym_1,),);
    assert!(result.is_err());

    // Confirm the symbols are in the correct scope
    assert_eq!(builder.scopes[0].symbol_alises.get(&sym_1).unwrap(), &DefId(0));
    assert_eq!(builder.scopes[2].symbol_alises.get(&sym_2).unwrap(), &DefId(1));
  }

  #[test]
  fn parse_ast_into_symbol_tree() {
    let source = "
    let mut var:int = 2;
    var = 4;

    // This should cause a new orphaned scope to spawn with 2 variables
    fn test_function(a:int,b:float,){};
    ";

    let mut p = Parser::new(source,);
    let mut visitor = SymbolTableBuilder::new();

    let ast = p.parse();

    ast.visit_with(&mut visitor,);

    dbg!(visitor.table);
  }

  #[test]
  fn undeclared_local_in_ast_causes_error() {}

  fn catch_unwind_silent<F:FnOnce() -> R + panic::UnwindSafe, R,>(f:F,) -> std::thread::Result<R,> {
    let prev_hook = panic::take_hook();
    panic::set_hook(Box::new(|_| {},),);
    let result = panic::catch_unwind(f,);
    panic::set_hook(prev_hook,);
    result
  }
}
