use super::{
  symbol_table::{DefId, SymbolTable},
  SymTy, Symbol,
};
use crate::{
  ast::{AbstractSyntaxTree, Block, Expression, ExpressionKind, Func, IdentInner, Literal, LocalKind, Pat, PatKind, Statement, StatementKind, P},
  errors::SymbolGenerationErrors,
  visitor::{Visitable, Visitor},
};
use eyre::Result;
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
// - Get rid of IdentInner. There must be a more elegant way of doing that.

impl Visitable<SymbolTableBuilder,> for AbstractSyntaxTree {
  fn visit_with(&mut self, visitor:&mut SymbolTableBuilder,) {
    for stmt in &mut self.statements {
      visitor.visit_stmt_mut(stmt,);
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
  pub fn new() -> Self {
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
  pub fn lookup(&self, sym:Symbol,) -> Result<DefId,> {
    // Traverse the scope stack backwards beginning with the current scope until a
    // symbol's alias is found
    let mut scopeid = Some(self.scopes.last().unwrap().id,);

    while scopeid.is_some() {
      let scope = &self.scopes[scopeid.unwrap()];

      if let Some(id,) = scope.symbol_alises.get(&sym,) {
        return Ok(*id,);
      }
      scopeid = scope.parent;
    }

    // Produce an error if an undeclared variable is encountered
    Err(SymbolGenerationErrors::UnrecognizedIdentifier { symbol:sym, }.into(),)
  }

  pub fn build(&self,) -> SymbolTable {
    self.table.clone()
  }
}

impl Visitor for SymbolTableBuilder {
  type Result = ();

  /// Visit the [`Statement`]s in the script. Record variable and function
  /// declarations in a [`SymbolTable`] and replace instances of identies with
  /// the corresponding [`DefId`].
  fn visit_stmt_mut(&mut self, stmt:&mut Statement,) {
    // Check if the statement is a let statement
    match &mut stmt.kind {
      StatementKind::Let(local,) => {
        // Parse the expression on the other side of the assign
        // This has to happen before the assigned ident is finalized
        match &mut local.kind {
          LocalKind::Init(expr,) => self.visit_expr_mut(expr,),
          LocalKind::Decl => panic!("Should have factored this out earlier"),
        }

        let mutable = local.mutable;
        let ty = SymTy::from(local.ty.clone().unwrap().copy(),);
        if let PatKind::Ident(ident,) = &mut local.pat.kind {
          // Insert the LocalDecl into the symbol table
          let id = self.table.insert(ident.symbol(), ty, mutable,);

          // Store the ID indexed by symbol
          self.scopes.last_mut().unwrap().symbol_alises.insert(ident.symbol(), id,);

          *ident = IdentInner::DefId(id,);
        }
      }
      StatementKind::Expression(expr,) => self.visit_expr_mut(expr,),
      StatementKind::Func(func,) => self.visit_func_mut(func,),
    }
  }

  fn visit_expr_mut(&mut self, expr:&mut Expression,) -> Self::Result {
    // Call visit_expr recursively down to check for any Identies
    match &mut expr.kind {
      // ExpressionKind::Literal(lit,) => {
      //   let ty = SymTy::from(**lit,);
      //   let sym = Symbol::from(**lit,);
      //   let id = self.table.insert(sym, ty, false,);

      //   self.scopes.last_mut().unwrap().symbol_alises.insert(sym, id,);

      //   **lit = Literal::DefId(id,);

      //   dbg!(lit);
      // }
      ExpressionKind::Ident(ident,) => self.visit_ident_mut(ident,),
      ExpressionKind::BinOp(lhs, _, rhs,) => {
        self.visit_expr_mut(lhs,);
        self.visit_expr_mut(rhs,);
      }
      ExpressionKind::Unary(_, expr,) => self.visit_expr_mut(expr,),
      ExpressionKind::If(cond, block_1, block_2,) => {
        self.visit_expr_mut(cond,);

        self.visit_block_mut(block_1,);

        if let Some(block_2,) = block_2 {
          self.visit_block_mut(block_2,);
        }
      }
      ExpressionKind::Assign(var, val,) => {
        self.visit_expr_mut(var,);
        self.visit_expr_mut(val,);
      }
      ExpressionKind::AssignOp(var, _, val,) => {
        self.visit_expr_mut(var,);
        self.visit_expr_mut(val,);
      }
      ExpressionKind::Tuple(tup,) => {
        for expr in tup {
          self.visit_expr_mut(expr,);
        }
      }
      ExpressionKind::WhileLoop(con, body,) => {
        self.visit_expr_mut(con,);
        self.visit_block_mut(body,);
      }
      ExpressionKind::ForLoop { pat, iter, body, } => {
        self.visit_pat(pat,);
        self.visit_expr_mut(iter,);
        self.visit_block_mut(body,);
      }
      ExpressionKind::Range { right, left, .. } => {
        self.visit_expr_mut(right,);
        self.visit_expr_mut(left,)
      }
      ExpressionKind::Return(expr,) => {
        if let Some(expr,) = expr {
          self.visit_expr_mut(expr,)
        };
      }
      // ExpressionKind::Break => todo!(),
      // ExpressionKind::Continue => todo!(),
      _ => {}
    }
  }

  ///Replace [`ExpressionKind::Ident`] with with its [`DefId`] or return an
  /// error if the variable has not been declared.
  fn visit_ident_mut(&mut self, ident:&mut IdentInner,) -> Self::Result {
    let id = self.lookup(ident.symbol(),).unwrap();
    // Replace the Ident with the DefId
    *ident = IdentInner::DefId(id,);
    // There should be some way to replace the current let branch so its logic
    // is handled here
  }

  fn visit_pat_mut(&mut self, pat:&mut Pat,) -> Self::Result {
    match &mut pat.kind {
      PatKind::Ident(ident,) => self.visit_ident_mut(ident,),
      PatKind::Range { start, end, .. } => {
        self.visit_expr_mut(start,);
        self.visit_expr_mut(end,);
      }
      PatKind::Tuple(pats,) => {
        for pat in pats.as_slice() {
          self.visit_pat(&pat,);
        }
      }
    }
  }

  fn visit_block_mut(&mut self, block:&mut Block,) -> Self::Result {
    self.new_scope(true,);
    for stmt in &mut block.inner_block {
      self.visit_stmt_mut(stmt,)
    }
    self.exit_scope();
  }

  fn visit_func_mut(&mut self, func:&mut Func,) -> Self::Result {
    // Insert the function's return as a local into the current scope
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
    for stmt in func.body.iter_mut() {
      self.visit_stmt_mut(stmt,)
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
  use std::panic;

  use super::SymbolTableBuilder;
  use crate::{
    ast::{ExpressionKind, FnSig, IdentInner, Literal, LocalKind, StatementKind, Ty},
    parser::Parser,
    symbol_table::{
      symbol_table::{DefId, LocalDecl},
      SymTy, Symbol,
    },
    visitor::Visitable,
  };

  #[test]
  fn lookup_returns_correct_def_id() {
    // Confirm it looks up stuff in the current scope
    let mut builder = SymbolTableBuilder::new();
    let sym_1 = Symbol::from("var_1",);
    let sym_2 = Symbol::from("var_2",);

    builder.insert(sym_1, SymTy::Str, true,);

    builder.new_scope(true,);
    builder.new_scope(true,);
    builder.new_scope(true,);

    builder.insert(sym_2, SymTy::Int, true,);

    // Confirm lookup works for symbols defined in a parent scope
    assert_eq!(DefId(0), builder.lookup(sym_1,).unwrap());

    // Confirm lookup works for symbols stored in the current scope
    assert_eq!(DefId(1), builder.lookup(sym_2,).unwrap());

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
    assert_eq!(DefId(1), builder.lookup(sym_2,).unwrap());

    // Confirm lookup fails for symbols defined in higher level scopes
    let result = builder.lookup(sym_1,);
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

    let a:int = var;
    ";

    let mut p = Parser::new(source,);
    let mut builder = SymbolTableBuilder::new();

    let mut ast = p.parse();

    ast.visit_with(&mut builder,);

    // let table = builder.build();

    // Check the AST has updated correctly

    // Check the type and value of var
    match &ast.statements[0].kind {
      StatementKind::Let(local,) => match &local.kind {
        LocalKind::Init(expr,) => match &expr.kind {
          ExpressionKind::Literal(lit,) => match **lit {
            Literal::Integer(int,) => assert_eq!(2, int),
            _ => panic!("Expected an integer not a {:?}", lit),
          },
          _ => panic!("Expected a literal"),
        },
        LocalKind::Decl => panic!("Expected an initialized local"),
      },
      _ => panic!("Expected a let statment not {:?}", &ast.statements[0].kind),
    }

    // Check the identity type and the value of var's assingment is correct
    match &ast.statements[1].kind {
      StatementKind::Expression(expr,) => match &expr.kind {
        ExpressionKind::Assign(ident, val,) => match (&ident.kind, &val.kind,) {
          (ExpressionKind::Ident(inner,), ExpressionKind::Literal(val,),) => {
            assert_eq!(inner, &IdentInner::DefId(DefId(0)));
            assert_eq!(**val, Literal::Integer(4));
          }
          _ => panic!("Expected the assignment {} = {}", ident.kind, val.kind),
        },
        _ => panic!("Expected an assign expression not {}", &expr.kind),
      },
      _ => panic!("Expected an expression not {:?}", &ast.statements[0].kind),
    }

    // Check the signature of function is accurate
    match &ast.statements[2].kind {
      StatementKind::Func(func,) => {
        assert_eq!(
          func.sig,
          FnSig {
            params:vec![(Symbol::from("a"), Ty::Int), (Symbol::from("b"), Ty::Float)],
            result:None
          }
        );
      }
      _ => panic!("Expected bool not {:?}", &ast.statements[2].kind),
    }

    // Check a has the correct type
    match &ast.statements[3].kind {
      StatementKind::Let(local,) => match &local.kind {
        LocalKind::Init(expr,) => match &expr.kind {
          ExpressionKind::Ident(ident,) => match ident {
            IdentInner::DefId(id,) => assert_eq!(DefId(0), *id),
            IdentInner::Raw(raw,) => unreachable!("Should not be {}.", raw),
          },
          // match **lit {
          //   Literal::(int,) => assert_eq!(true, bool),
          //   _ => panic!("Expected an Integer not a {:?}", lit),
          // },
          _ => panic!("Expected a literal"),
        },
        LocalKind::Decl => panic!("Expected an initialized local"),
      },
      _ => panic!("Expected a let statment not {:?}", &ast.statements[0].kind),
    }

    // Check the symbol table contains the correct symbols
    assert_eq!(&LocalDecl::new(DefId(0,), SymTy::Int, "var".into(), true,), builder.table.lookup(DefId(0,),));
    assert_eq!(
      &LocalDecl::new(DefId(1,), SymTy::Fn(None), "test_function".into(), false,),
      builder.table.lookup(DefId(1,),)
    );
    assert_eq!(&LocalDecl::new(DefId(2,), SymTy::Int, "a".into(), false,), builder.table.lookup(DefId(2,),));
    assert_eq!(&LocalDecl::new(DefId(3,), SymTy::Float, "b".into(), false,), builder.table.lookup(DefId(3,),));
    assert_eq!(&LocalDecl::new(DefId(4,), SymTy::Int, "a".into(), false,), builder.table.lookup(DefId(4,),));
  }

  #[test]
  #[should_panic(expected = "called `Result::unwrap()` on an `Err` value: cannot find value `a` in this scope.")]
  fn undeclared_local_in_ast_causes_error() {
    let source = "a = 5;";

    let mut p = Parser::new(source,);
    let mut ast = p.parse();
    let mut builder = SymbolTableBuilder::new();

    ast.visit_with(&mut builder,);
  }
}
