use crate::{
  ast::{AbstractSyntaxTree as AST, AssignOpKind, BinOpKind, Expression, ExpressionKind, IdentInner, Literal, LocalKind, PatKind, Statement, StatementKind},
  interner::lookup,
  symbol_table::{
    symbol_table::{DefId, LocalDecl, SymbolTable},
    SymTy,
  },
  visitor::{Visitable, Visitor},
};
use eyre::Result;
use std::ops::Index;

// Refactor:
// - Eventually I will turn all of this into a more formal IR with `BasicBlocks`
//   working into control flow and SSA and all that jazz so the `BasicBlock`
//   will need to be more sophisticated
// - Move the IR features bit from the builder to module level documentation.
// - I believe as with the symbol table visitor (which also needs to be renamed)
// - I think eventually the structure which holds all the basic blocks will need
//   to store the symbol table not the basic blocks themselves.
// - Rust compiler uses a `NeedsTemporary` enum to indicate whether a generated
//   operand should be assigned to a temporary
// - I really don't like the ident storing both Raw and DefId, it's both
//   unwieldy and I think indicative of an inefficency somewhere. Ditto literal
// - Having to return an operand in some places where a Place would do ends up
//   requiring unneccesary steps of creating an operand and cloning its place
// - Try to get rid of clones.
// - Maybe having literals as an operand separate from constants is the best I
//   can do but I am skeptical. I just feel like adding them to the symbol table
//   is suboptimal.
// - Maybe what makes the most sense is having the IR visitor build the table
//   internally per block
// - I think place needs to indicate whether its ID points to a temp or a name

// TODO:
// - By the time we reach here I think all let statements should be
//   initializations not defered assignments so update that in the symbol table
//   generation
// - Figure out if this is supposed to be generated alongside the symbol table.
//   I think it would be easier if it was but there might be some benefit to
//   doing them as second passes I am missing
// - I need to figure out how to handle indexing in parsing and the symbol table

#[derive(Debug,)]
struct BasicBlock {
  table:SymbolTable,
  code:Vec<Instruction,>,
  triples:Vec<TripleId,>,
}

impl BasicBlock {
  pub fn new(table:SymbolTable,) -> Self {
    Self {
      table,
      code:Vec::new(),
      triples:Vec::new(),
    }
  }

  /// Append a new [`Instruction`] to the basic block and return its
  /// [`TripleId`].
  pub fn add(&mut self, instruction:Instruction,) -> TripleId {
    let id = self.code.len();
    self.code.push(instruction,);
    TripleId::from(id,)
  }

  /// Swap the positions of two triples.
  pub fn swap(&mut self, id_1:TripleId, id_2:TripleId,) {
    // Find the indicies of the two triples.
    let mut iter = self.triples.iter();
    let a = iter.position(|id| *id == id_1 || *id == id_2,).unwrap();
    let b = iter.position(|id| *id == id_1 || *id == id_2,).unwrap();

    // Swap the two triples
    self.triples.swap(a, b,);
  }
}

#[derive(Debug, Clone, PartialEq,)]
/// Also called an [L-Value](https://en.wikipedia.org/wiki/Value_(computer_science)#lrvalue) in some languages.
pub struct Place {
  // Confirm this should be a DefId
  /// The Index of the value's [`SymbolTable`] entry.
  id:DefId,
  /// A vector of [`AccessKind`]s describing how to access the fields of a value
  /// if necessary.
  access:Vec<AccessKind,>,
}

impl Place {
  fn new(id:DefId, access:Vec<AccessKind,>,) -> Self {
    Self { id, access, }
  }
}

#[derive(Debug, Clone, PartialEq,)]
/// Describes how to access the inner values of arrays and tuples.
pub enum AccessKind {
  /// Idx is a [`Place`] as the value might be computed at runtime and stored.
  Index(Place,),
  Field(usize,),
}

#[derive(Debug, Clone, PartialEq,)]
pub enum Operand {
  /// The index of a named variable's
  /// [`LocalDecl`](crate::symbol_table::symbol_table::LocalDecl) entry in the
  /// [`SymbolTable`].
  Name(Place,),
  /// The index of an immutable identifier's
  /// [`LocalDecl`](crate::symbol_table::symbol_table::LocalDecl) entry in the
  /// [`SymbolTable`].
  Const(Place,),
  /// The index of a temporary's
  /// [`LocalDecl`](crate::symbol_table::symbol_table::LocalDecl) entry in the
  /// [`SymbolTable`].
  Temp(Place,),
  /// A [`Literal`] value.
  Literal(Literal,),
}

impl Operand {
  fn place(&self,) -> Place {
    match self {
      Operand::Name(place,) => place.clone(),
      Operand::Const(place,) => place.clone(),
      Operand::Temp(place,) => place.clone(),
      Operand::Literal(_,) => panic!("Literals do not have a place"),
    }
  }

  fn ty(&self, table:&SymbolTable,) -> SymTy {
    match self {
      Operand::Name(place,) => table.lookup(place.id,).ty(),
      Operand::Const(place,) => table.lookup(place.id,).ty(),
      Operand::Temp(place,) => table.lookup(place.id,).ty(),
      Operand::Literal(lit,) => SymTy::from(*lit,),
    }
  }
}

// The Rust compiler calls these "RValues" and does not include Copy/Move/Jump
// etc while DB calls them "Instructions".
#[derive(Debug, PartialEq,)]
pub enum Instruction {
  // Assignment of the form `x = y op z` where op is an arithmetic or logical operation.
  BinOp(Place, BinOp, Operand, Operand,),
  // assignment of the form x = op y where op is a unary operation.
  UnaryOp(Place, UnOp, Operand,),
  /// x = y copies the value of y into x. Using [`Place`]'s [`AccessKind`] field
  /// can also copy values of the form `x[i] = y` or `x = y[i]` or tuples of the
  /// form `x.i = y` or `x = y.i`. Where i indicates the number of entries
  /// past the starting point.
  Copy(Place, Operand,),
  /// An unconditional jump to the specified instruction index.
  Jump(),
  // /// Jumps to the specified instruction index if
  // CondJump()x
  /// Assign a value to a place.
  Assign(Place, Operand,),
  // More are specified on DB pg 365, pdf pg 388
}

#[derive(Debug, PartialEq,)]
#[allow(non_camel_case_types)]
pub enum BinOp {
  MINUS,
  PLUS,
  SLASH,
  STAR,
  EQUAL_EQUAL,
  NOT_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,
  AND,
  OR,
}

#[derive(Clone, Debug, PartialEq,)]
pub enum UnOp {
  Not,
  Minus,
}

#[derive(Debug, PartialEq,)]
///A [newtype](https://doc.rust-lang.org/rust-by-example/generics/new_types.html) wrapping an [`Instruction`]'s index in the [`SymbolTable`].
struct TripleId(usize,);

impl Index<TripleId,> for Vec<Instruction,> {
  type Output = Instruction;

  fn index(&self, index:TripleId,) -> &Self::Output {
    &self[index.0]
  }
}

impl From<usize,> for TripleId {
  fn from(value:usize,) -> Self {
    TripleId(value,)
  }
}

#[derive(Debug,)]
/// Traverses the [`Ast`](AbstractSyntaxTree) and coverts it to an
/// IR. Builds `BasicBlocks` and their [`SymbolTable`]s.
///
/// # IR Features
/// - [Three-address code](https://en.wikipedia.org/wiki/Three-address_code)
///   instructions.
/// - TODO: [Static single-assignment form](https://en.wikipedia.org/wiki/Static_single-assignment_form).
/// - TODO: [Control flow](https://en.wikipedia.org/wiki/Control-flow_graph)
///   representation.
struct IrVisitor {
  block:BasicBlock,
}

impl IrVisitor {
  pub fn new(table:SymbolTable,) -> Self {
    Self {
      block:BasicBlock::new(table,),
    }
  }

  ///Creates a new temp from a given [`DefId`] and returns its [`LocalDecl`].
  fn new_temp(&mut self, ty:SymTy,) -> &LocalDecl {
    self.block.table.new_temp(ty,);
    self.block.table.last_temp()
  }
}

// Tricky because visit_expr should return an operand.
// But statement should return an instruction.
// I am unsure how to rectify this.

// I'm actually thinking expr returning an operand is incorrect. At least some
// branches 100% should not
impl Visitor for IrVisitor {
  type Result = Option<Result<Operand,>,>;

  fn visit_stmt(&mut self, stmt:&Statement,) -> Self::Result {
    match &stmt.kind {
      StatementKind::Let(local,) => {
        let defid = match &local.pat.kind {
          PatKind::Ident(inner,) => match inner {
            IdentInner::DefId(id,) => id,
            IdentInner::Raw(raw,) => unreachable!("`{}` should not be reachable!", raw.symbol),
          },
          _ => unreachable!(),
        };

        match &local.kind {
          LocalKind::Init(expr,) => {
            // Generate whatever is needed for the expression and return an operand
            let lhs = Place::new(*defid, vec![],);
            let rhs = self.visit_expr(expr,).unwrap().unwrap();
            // Add the assignment call
            self.block.add(Instruction::Assign(lhs, rhs,),)
          }

          // Convert into a real error that actually prints the variable's name and location
          LocalKind::Decl => {
            panic!(
              // These sentences might possibly be two separate errors which will end up occuring in two separate places
              "Must initialize a variable before it can be assigned. Variable {} is never initialized.",
              lookup(self.block.table.lookup(*defid,).name(),)
            )
          }
        };
        None
      }
      StatementKind::Expression(expr,) => self.visit_expr(expr,),
      StatementKind::Func(func,) => self.visit_func(func,),
    }
  }

  fn visit_expr(&mut self, expr:&Expression,) -> Self::Result {
    match &expr.kind {
      ExpressionKind::Literal(lit,) => Some(Ok(Operand::Literal(**lit,),),),
      ExpressionKind::Ident(ident,) => {
        // Construct an Operand for the ident and return its ID
        match ident {
          IdentInner::DefId(id,) => {
            //Convert the identity to an operand
            return Some(Ok(Operand::Name(Place::new(*id, vec![],),),),);
          }
          IdentInner::Raw(ident,) => unreachable!("Should have converted the RawIdent in an earlier stage. Should not be {ident}."),
        };
      }
      ExpressionKind::Assign(target, rhs,) => {
        //Evaluate the 1st expression as a place and the 2nd expression as an operand
        let target = self.visit_expr(target,).unwrap().unwrap();
        let rhs = self.visit_expr(rhs,).unwrap().unwrap();

        self.block.add(Instruction::Assign(target.place(), rhs,),);

        None
      }
      ExpressionKind::AssignOp(target, op, rhs,) => {
        let target = self.visit_expr(&target,).unwrap().unwrap();
        let rhs = self.visit_expr(rhs,).unwrap().unwrap();

        // Desugar into a binary operation and an assign

        // Assign the result of the binop to a temporary
        let temp = match op {
          AssignOpKind::STAR_EQUAL => {
            let temp = Place::new(self.new_temp(target.ty(&self.block.table,),).id(), vec![],);

            // Pop the temp onto the instruction stack
            self.block.add(Instruction::BinOp(temp.clone(), BinOp::STAR, target.clone(), rhs,),);

            // Return the temporary as an operand
            Operand::Temp(temp,)
          }
          AssignOpKind::SLASH_EQUAL => {
            let temp = Place::new(self.new_temp(target.ty(&self.block.table,),).id(), vec![],);

            // Pop the temp onto the instruction stack
            self.block.add(Instruction::BinOp(temp.clone(), BinOp::SLASH, target.clone(), rhs,),);

            // Return the temporary as an operand
            Operand::Temp(temp,)
          }
          AssignOpKind::PLUS_EQUAL => {
            let temp = Place::new(self.new_temp(target.ty(&self.block.table,),).id(), vec![],);

            // Pop the temp onto the instruction stack
            self.block.add(Instruction::BinOp(temp.clone(), BinOp::PLUS, target.clone(), rhs,),);

            // Return the temporary as an operand
            Operand::Temp(temp,)
          }
          AssignOpKind::MINUS_EQUAL => {
            let temp = Place::new(self.new_temp(target.ty(&self.block.table,),).id(), vec![],);

            // Pop the temp onto the instruction stack
            self.block.add(Instruction::BinOp(temp.clone(), BinOp::MINUS, target.clone(), rhs,),);

            // Return the temporary as an operand
            Operand::Temp(temp,)
          }
        };

        // Assign the temporary to the target
        self.block.add(Instruction::Assign(target.place(), temp,),);
        None
      }
      ExpressionKind::BinOp(expr_1, op, expr_2,) => {
        // Generate the binary operation and set it equal to a temporary
        let operand_1 = self.visit_expr(expr_1,).unwrap().unwrap();
        let operand_2 = self.visit_expr(expr_2,).unwrap().unwrap();

        // operand 1 is used to generate the temporary. It doesn't really matter which
        // one does it. Since all it cares about is using the id to fetch the type
        let temp = Place::new(self.new_temp(operand_1.ty(&self.block.table,),).id(), vec![],);

        match op {
          BinOpKind::MINUS => self.block.add(Instruction::BinOp(temp.clone(), BinOp::MINUS, operand_1, operand_2,),),
          BinOpKind::PLUS => self.block.add(Instruction::BinOp(temp.clone(), BinOp::PLUS, operand_1, operand_2,),),
          BinOpKind::SLASH => self.block.add(Instruction::BinOp(temp.clone(), BinOp::SLASH, operand_1, operand_2,),),
          BinOpKind::STAR => self.block.add(Instruction::BinOp(temp.clone(), BinOp::STAR, operand_1, operand_2,),),
          BinOpKind::EQUAL_EQUAL => self.block.add(Instruction::BinOp(temp.clone(), BinOp::EQUAL_EQUAL, operand_1, operand_2,),),
          BinOpKind::NOT_EQUAL => self.block.add(Instruction::BinOp(temp.clone(), BinOp::NOT_EQUAL, operand_1, operand_2,),),
          BinOpKind::GREATER => self.block.add(Instruction::BinOp(temp.clone(), BinOp::GREATER, operand_1, operand_2,),),
          BinOpKind::GREATER_EQUAL => self.block.add(Instruction::BinOp(temp.clone(), BinOp::GREATER_EQUAL, operand_1, operand_2,),),
          BinOpKind::LESS => self.block.add(Instruction::BinOp(temp.clone(), BinOp::LESS, operand_1, operand_2,),),
          BinOpKind::LESS_EQUAL => self.block.add(Instruction::BinOp(temp.clone(), BinOp::LESS_EQUAL, operand_1, operand_2,),),
          BinOpKind::AND => self.block.add(Instruction::BinOp(temp.clone(), BinOp::AND, operand_1, operand_2,),),
          BinOpKind::OR => self.block.add(Instruction::BinOp(temp.clone(), BinOp::OR, operand_1, operand_2,),),
        };
        // Return the temp as an operand
        Some(Ok(Operand::Temp(temp,),),)
      }
      ExpressionKind::Unary(op, expr,) => {
        // Assign the value to a temporary and return the temporary as an operand.
        let operand = self.visit_expr(expr,).unwrap().unwrap();
        let temp = Place::new(self.new_temp(operand.ty(&self.block.table,),).id(), vec![],);

        match op {
          crate::ast::UnOp::Not => self.block.add(Instruction::UnaryOp(temp.clone(), UnOp::Not, operand,),),
          crate::ast::UnOp::Minus => self.block.add(Instruction::UnaryOp(temp.clone(), UnOp::Minus, operand,),),
        };

        Some(Ok(Operand::Temp(temp,),),)
      }
      // I have no idea how to implement these
      ExpressionKind::If(p, p1, p2,) => todo!(),
      ExpressionKind::Tuple(vec,) => todo!(),
      ExpressionKind::WhileLoop(p, p1,) => todo!(),
      ExpressionKind::ForLoop { pat, iter, body, } => todo!(),
      ExpressionKind::Range { left, inclusive, right, } => todo!(),
      ExpressionKind::Break => todo!(),
      ExpressionKind::Continue => todo!(),
      ExpressionKind::Return(p,) => todo!(),
    }
  }
}

impl Visitable<IrVisitor,> for AST {
  fn visit_with(&mut self, visitor:&mut IrVisitor,) {
    for stmt in &self.statements {
      visitor.visit_stmt(stmt,);
    }
  }
}

#[cfg(test)]
mod test {
  use super::IrVisitor;
  use crate::{
    ast::Literal,
    ir::{BinOp, Instruction, Operand, Place, UnOp},
    parser::Parser,
    symbol_table::{
      sym_visitor::SymbolTableBuilder,
      symbol_table::{DefId, LocalDecl},
      SymTy, Symbol,
    },
    visitor::Visitable,
  };

  #[test]
  fn build_produces_block() {
    let source = "
    let mut a:int = 15;
    let b:int = a; 
    let b:int = b + 6;
    b *= -2;
    
    // fn foo(a:bool, d:usize) -> usize {
    //   if a {
    //     d -= 2
    //   }
    //   return d;
    // }
    ";

    let mut p = Parser::new(source,);
    let mut ast = p.parse();

    let mut sym_builder = SymbolTableBuilder::new();

    ast.visit_with(&mut sym_builder,);

    let table = sym_builder.build();

    let mut ir = IrVisitor::new(table,);

    ast.visit_with(&mut ir,);

    // Confirm the code for the block is correct
    let a_place = Place::new(DefId(0,), vec![],);
    let b_1_place = Place::new(DefId(1,), vec![],);
    let b_2_place = Place::new(DefId(2,), vec![],);

    assert_eq!(
      Instruction::Assign(a_place.clone(), Operand::Literal(Literal::Integer(15,),),),
      ir.block.code[0]
    );
    assert_eq!(Instruction::Assign(b_1_place.clone(), Operand::Name(a_place.clone()),), ir.block.code[1]);
    // Confirm that a temp is generated
    assert_eq!(
      LocalDecl::new(DefId(0,), SymTy::Int, Symbol::from("t_0",), false,),
      *ir.block.table.lookup_temp(DefId(0))
    );
    // Confirm the add and assign are generated
    assert_eq!(
      Instruction::BinOp(
        // This is the temp
        Place::new(DefId(0,), vec![],),
        BinOp::PLUS,
        Operand::Name(b_1_place.clone(),),
        Operand::Literal(Literal::Integer(6,),),
      ),
      ir.block.code[2]
    );
    assert_eq!(
      Instruction::Assign(b_2_place.clone(), Operand::Temp(Place::new(DefId(0,), vec![],))),
      ir.block.code[3]
    );
    // Confirm 2 temps are created
    assert_eq!(
      LocalDecl::new(DefId(1,), SymTy::Int, Symbol::from("t_1",), false,),
      *ir.block.table.lookup_temp(DefId(1))
    );
    assert_eq!(
      LocalDecl::new(DefId(2,), SymTy::Int, Symbol::from("t_2",), false,),
      *ir.block.table.lookup_temp(DefId(2))
    );
    // Confirm the unary mult and assign are generated
    assert_eq!(
      Instruction::UnaryOp(Place::new(DefId(1), vec![]), UnOp::Minus, Operand::Literal(Literal::Integer(2))),
      ir.block.code[4]
    );
    assert_eq!(
      Instruction::BinOp(
        b_2_place.clone(),
        BinOp::STAR,
        Operand::Name(b_2_place.clone()),
        Operand::Temp(Place::new(DefId(1), vec![]))
      ),
      ir.block.code[5]
    );
    assert_eq!(
      Instruction::Assign(b_2_place.clone(), Operand::Temp(Place::new(DefId(2,), vec![],)),),
      ir.block.code[6]
    );
  }

  #[test]
  fn swap_works() {}
}
