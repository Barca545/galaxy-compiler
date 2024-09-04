#[cfg(test)]
mod tests {
  use crate::parser::Parser;
  // use crate::ast::{BinOpKind, ExpressionKind, LiteralKind, LocalKind,
  // StatementKind};

  // Add function definitions
  // Add returns to if statements, functions, and loops
  // Add key words "and", "in", "not", and "or"
  // Add back the error checking for the AST
  // - Eat token expect is not working because it has a hardcoded error message
  //   and does not print the input error
  // - Correct statement erroring so expression-statements do not require
  //   semicolons
  // - Erroring if tuples are not closed
  // Should be able to begin a boolean expression with ! (NOT)
  // Should not require blocks to have semicolons after them

  // Add checks to ensure the AST is correct
  #[test]
  fn parse_declaration() {
    // Precedence order error with binops and the slash
    // -Not tokenizing the slash
    let source = r#"
    // Check the binops work with nums
    let mut value_test = 5 + 6 * 7 / 8;

    // // Check the binops work with boolean statements
    let value = true != false == true;

    // let value_2 = (2 + 19, 6, 1);
    "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();
  }

  #[test]
  fn parse_reassignment() {
    let source = r#"
    // Check reassignment works
    value_test = 5;

    value_test += 5;
    "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();
  }

  #[test]
  fn parse_if_expression() {
    let source = r#"
    let test = if true != false {
      let a = 90;
    }
    else {
      let a = 50;
    };
    "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();
  }

  #[test]
  fn parse_while_loop() {
    let source = r#"
    while true {
      value_test += 40;
    };
    "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();
  }

  #[test]
  fn parse_for_loop() {
    let source = r#"
    while true {
      break;
    };

    for n in 1..2 {
      let test = 1;
      if false {
        break;
      };
    };

    for n in 1..=2 {
      let test = 1;
      if true {
        continue;
      };
    };
    "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();
  }

  fn parse_works() {
    // C:\Users\Jamari\.cargo\bin\cargo.exe 'test', '--package',
    // 'galaxy-compiler', '--bin', 'galaxy-compiler', '--',
    // 'parser::tests::parse_works', '--exact', '--show-output'
    // '--nocapture'

    // Switch how equal signs are handled?
    // https://craftinginterpreters.com/global-variables.html#assignment
    // To fix this, variable() should look for and consume the = only if it’s in
    // the context of a low-precedence expression. The code that knows the
    // current precedence is, logically enough, parsePrecedence(). The
    // variable() function doesn’t need to know the actual level. It just
    // cares that the precedence is low enough to allow assignment, so we
    // pass that fact in as a Boolean.

    //Check the AST is correct
    // match ast.statements.clone()[0].kind.clone() {
    //   //Check the let statement
    //   StatementKind::Let(local,) => {
    //     match &local.kind {
    //       LocalKind::Init(expr,) => match &expr.kind {
    //         ExpressionKind::BinOp(lhs, op, rhs,) => {
    //           //Check the value of the lhs
    //           match &lhs.kind {
    //             ExpressionKind::Literal(lit,) => {
    //               assert_eq!(&lit.kind, &LiteralKind::Integer)
    //             }
    //             _ => panic!(),
    //           }

    //           //Check the value of the rhs
    //           match &rhs.kind {
    //             ExpressionKind::Literal(lit,) => {
    //               assert_eq!(&lit.kind, &LiteralKind::Integer)
    //             }
    //             _ => panic!(),
    //           }

    //           //Check the value of the op
    //           assert_eq!(*op, BinOpKind::PLUS);
    //         }
    //         _ => panic!(),
    //       },
    //       _ => panic!(),
    //     };
    //   }
    //   _ => panic!(),
    // }

    // dbg!(ast.statements.clone()[0].kind.clone());
  }
}
