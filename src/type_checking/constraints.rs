use crate::{
  ast::{Expression, ExpressionKind, Literal},
  symbol_table::Symbol,
};

// Refactor:
// - Update `gen_binop_constraint` so it can handle floats.
// - Update `gen_unary_constraint`so it can handle floats.

// TODO:
// - If I have the compiler turn all loops into a combination of loops and match
//   statements(?) before type inference I won't need to generate as many
//   constraints. (Rust talks about this as part of generating MIR. I am less
//   sure it is possible in my own language.)

#[derive(PartialEq, Debug,)]
struct Constraint {
  lhs:Term,
  rhs:Term,
}

impl Constraint {
  /// Create a new [`Constraint`] with from an expression and a [`Term`].
  fn new(lhs:&ExpressionKind, rhs:Term,) -> Self {
    Constraint {
      lhs:Term::Expr(lhs.clone(),),
      rhs,
    }
  }
}

#[derive(PartialEq, Debug,)]
enum Term {
  Bool,
  Float,
  Int,
  Str,
  //Final implementation will not use strings, but an Interned [String] most likely
  Var(Symbol,),
  Expr(ExpressionKind,),
}

fn generate_contstraints(expr:&ExpressionKind, cons:&mut Vec<Constraint,>,) {
  match expr {
    ExpressionKind::Literal(lit,) => gen_literal_constraint(lit, expr, cons,),
    ExpressionKind::BinOp(lhs, _, rhs,) => gen_binop_constraint(lhs, rhs, expr, cons,),
    ExpressionKind::Unary(_, unary,) => gen_unary_constraint(unary, expr, cons,),
    // ExpressionKind::If(cond, branch_1, branch_2,) => todo!(),
    // ExpressionKind::Ident(raw_ident,) => todo!(),
    // ExpressionKind::Assign(p, p1,) => todo!(),
    // ExpressionKind::AssignOp(p, assign_op_kind, p1,) => todo!(),
    // ExpressionKind::Tuple(vec,) => todo!(),
    // ExpressionKind::WhileLoop(p, p1,) => todo!(),
    // ExpressionKind::ForLoop { pat, iter, body, } => todo!(),
    // ExpressionKind::Range { lhs, _, rhs, } => todo!(),
    // ExpressionKind::Return(p,) => todo!(),
    // None of these are typed
    _ => unreachable!("Break and Continue expressions are untyped. {} cannot yield constraints.", expr),
  }
}

fn gen_literal_constraint(lit:&Literal, expr:&ExpressionKind, cons:&mut Vec<Constraint,>,) {
  let con = match lit {
    Literal::Bool(_,) => Constraint::new(expr, Term::Bool,),
    Literal::Integer(_,) => Constraint::new(expr, Term::Int,),
    Literal::Float(_,) => Constraint::new(expr, Term::Float,),
    Literal::Str(_,) => Constraint::new(expr, Term::Str,),
  };

  cons.push(con,);
}

fn gen_binop_constraint(lhs:&Expression, rhs:&Expression, expr:&ExpressionKind, cons:&mut Vec<Constraint,>,) {
  generate_contstraints(&lhs.kind, cons,);
  generate_contstraints(&rhs.kind, cons,);
  // Both expressions must also be capable of being in a binary expression and the
  // full expression should result in a number
  let consequent = vec![
    Constraint::new(&lhs.kind, Term::Int,),
    Constraint::new(&rhs.kind, Term::Int,),
    Constraint::new(&expr, Term::Int,),
  ];

  cons.extend(consequent,);
}

fn gen_unary_constraint(unary:&Expression, expr:&ExpressionKind, cons:&mut Vec<Constraint,>,) {
  generate_contstraints(&unary.kind, cons,);
  let consequent = vec![Constraint::new(&unary.kind, Term::Int,), Constraint::new(expr, Term::Int,)];
  cons.extend(consequent,);
}

#[cfg(test)]
mod test {
  use super::generate_contstraints;
  use crate::{
    ast::{BinOpKind, Expression, ExpressionKind, Literal, P},
    symbol_table::Symbol,
    token::Location,
    type_checking::constraints::{Constraint, Term},
  };

  #[test]
  fn gen_lit_constraints() {
    let mut cons = Vec::new();

    //Int constraint
    let expr = ExpressionKind::Literal(P::new(Literal::Integer(5,),),);
    generate_contstraints(&expr, &mut cons,);

    assert_eq!(
      cons[0],
      Constraint {
        lhs:Term::Expr(expr.clone()),
        rhs:Term::Int
      }
    );

    //Float constraint
    let expr = ExpressionKind::Literal(P::new(Literal::Float(5.0,),),);
    generate_contstraints(&expr, &mut cons,);
    assert_eq!(
      cons[1],
      Constraint {
        lhs:Term::Expr(expr.clone()),
        rhs:Term::Float
      }
    );

    //Bool constraint
    let expr = ExpressionKind::Literal(P::new(Literal::Bool(true,),),);
    generate_contstraints(&expr, &mut cons,);
    assert_eq!(
      cons[2],
      Constraint {
        lhs:Term::Expr(expr.clone()),
        rhs:Term::Bool
      }
    );

    //Str constraint
    let expr = ExpressionKind::Literal(P::new(Literal::Str(Symbol::from("value",),),),);
    generate_contstraints(&expr, &mut cons,);
    assert_eq!(
      cons[3],
      Constraint {
        lhs:Term::Expr(expr.clone()),
        rhs:Term::Str
      }
    );
  }

  #[test]
  fn gen_binop_constraints() {
    let mut cons = Vec::new();

    let expr = &ExpressionKind::BinOp(
      P::new(Expression {
        id:0,
        kind:ExpressionKind::Literal(P::new(Literal::Integer(5,),),),
        loc:Location::new(),
      },),
      BinOpKind::MINUS,
      P::new(Expression {
        id:0,
        kind:ExpressionKind::Literal(P::new(Literal::Integer(7,),),),
        loc:Location::new(),
      },),
    );
    generate_contstraints(expr, &mut cons,);

    // assert_eq!();
    // assert_eq!();
    // assert_eq!();
    // assert_eq!();
    // assert_eq!();
  }

  #[test]
  fn gen_unary_constraints() {}

  #[test]
  #[should_panic]
  fn gen_constraints_for_untyped_expressions() {
    let mut cons = Vec::new();

    let expr = &ExpressionKind::Continue;
    generate_contstraints(expr, &mut cons,);

    let expr = &ExpressionKind::Break;
    generate_contstraints(expr, &mut cons,);
  }
}
