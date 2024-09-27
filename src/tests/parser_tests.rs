#[cfg(test)]
mod tests {
  use crate::ast::{AssignOpKind, BinOpKind, ExpressionKind, LiteralKind, LocalKind, PatKind, StatementKind, UnOp};
  use crate::interner::lookup;
  use crate::parser::Parser;

  // Add checks to ensure the AST is correct

  // move the test boolean ops into the test declaration

  // do != and == have pemdas?

  // reuse as much of the logic in the blocks so tests can be copy pasted easier

  #[test]
  fn test_declaration() {
    let source = r#"
    // Check the binops work with nums
    let mut value_test = 5 + 6 * 7 / 8;

    // Check the binops work with boolean statements
    let value = true != false == true;

    // Check tuple expressions parse properly
    let value_2 = (5 + 6, 7, 8);

    // Check logical and/or and unary expressions parse properly
    let value_3 = !false && true || false;
    "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();

    // Check AST

    // First Declaration
    match ast.statements.clone()[0].kind.clone() {
      StatementKind::Let(local,) => {
        // Check mutability
        match local.pat.clone().kind.clone() {
          PatKind::Ident(raw,) => {
            assert_eq!(raw.mutable, true);
            assert_eq!(lookup(raw.symbol.idx), "value_test");
          }
          _ => panic!("Should be an identity"),
        }

        match local.kind.clone() {
          LocalKind::Init(local,) => {
            match &local.kind {
              ExpressionKind::BinOp(lhs, op, rhs,) => {
                // Test the lhs
                match &lhs.kind {
                  ExpressionKind::Literal(lit,) => {
                    assert_eq!(&lit.kind, &LiteralKind::Integer);
                    assert_eq!(lookup(lit.symbol.idx), "5");
                  }
                  _ => panic!("Should be a literal"),
                }

                // Test the op
                assert_eq!(op, &BinOpKind::PLUS);

                // Test the rhs
                match &rhs.kind {
                  ExpressionKind::BinOp(lhs, op, rhs,) => {
                    // Test the lhs
                    if let ExpressionKind::BinOp(lhs, op, rhs,) = &lhs.kind {
                      match &lhs.kind {
                        ExpressionKind::Literal(lit,) => {
                          assert_eq!(&lit.kind, &LiteralKind::Integer);
                          assert_eq!(lookup(lit.symbol.idx), "6");
                        }
                        _ => panic!("Should be a literal"),
                      }

                      // Test the op
                      assert_eq!(op, &BinOpKind::STAR);

                      match &rhs.kind {
                        ExpressionKind::Literal(lit,) => {
                          assert_eq!(&lit.kind, &LiteralKind::Integer);
                          assert_eq!(lookup(lit.symbol.idx), "7");
                        }
                        _ => panic!("Should be a literal"),
                      }
                    }

                    // Test the op
                    assert_eq!(op, &BinOpKind::SLASH);

                    // Test the rhs
                    match &rhs.kind {
                      ExpressionKind::Literal(lit,) => {
                        assert_eq!(&lit.kind, &LiteralKind::Integer);
                        assert_eq!(lookup(lit.symbol.idx), "8");
                      }
                      _ => panic!("Should be a literal"),
                    }
                  }
                  _ => panic!("Should be a binop"),
                }
              }
              _ => panic!("Should be a binop"),
            }
          }
          _ => panic!("Should be a local initiaton"),
        }
      }
      _ => unreachable!(),
    }

    // Second Declaration
    match ast.statements.clone()[1].kind.clone() {
      StatementKind::Let(local,) => {
        // Check mutability
        match local.pat.clone().kind.clone() {
          PatKind::Ident(raw,) => {
            assert_eq!(raw.mutable, false);
            assert_eq!(lookup(raw.symbol.idx), "value");
          }
          _ => panic!("Should be an identity"),
        }

        match local.kind.clone() {
          LocalKind::Init(local,) => {
            match &local.kind {
              ExpressionKind::BinOp(lhs, op, rhs,) => {
                // Test the lhs
                match &lhs.kind {
                  ExpressionKind::BinOp(lhs, op, rhs,) => {
                    // Test the lhs
                    match &lhs.kind {
                      ExpressionKind::Literal(lit,) => {
                        assert_eq!(&lit.kind, &LiteralKind::Bool);
                        assert_eq!(lookup(lit.symbol.idx), "true");
                      }
                      _ => panic!("Should be a literal"),
                    }

                    assert_eq!(op, &BinOpKind::NOT_EQUAL);

                    // Test the rhs
                    match &rhs.kind {
                      ExpressionKind::Literal(lit,) => {
                        assert_eq!(&lit.kind, &LiteralKind::Bool);
                        assert_eq!(lookup(lit.symbol.idx), "false");
                      }
                      _ => panic!("Should be a literal"),
                    }
                  }
                  _ => panic!("Should be a binop"),
                }

                assert_eq!(op, &BinOpKind::EQUAL_EQUAL);

                // Test the rhs
                match &rhs.kind {
                  ExpressionKind::Literal(lit,) => {
                    assert_eq!(&lit.kind, &LiteralKind::Bool);
                    assert_eq!(lookup(lit.symbol.idx), "true");
                  }
                  _ => panic!("Should be a literal"),
                }
              }
              _ => panic!("Should be a binop"),
            }
          }
          _ => panic!("Should be a local initiaton"),
        }
      }
      _ => unreachable!(),
    }

    // Third Declaration
    match ast.statements.clone()[2].kind.clone() {
      StatementKind::Let(local,) => {
        // Check mutability
        match local.pat.clone().kind.clone() {
          PatKind::Ident(raw,) => {
            assert_eq!(raw.mutable, false);
            assert_eq!(lookup(raw.symbol.idx), "value_2");
          }
          _ => panic!("Should be an identity"),
        }

        match &local.kind {
          LocalKind::Init(local,) => match &local.kind {
            ExpressionKind::Tuple(inner,) => {
              // Test the first tuple element
              match &inner[0].kind {
                ExpressionKind::BinOp(lhs, op, rhs,) => {
                  // Test the lhs
                  match &lhs.kind {
                    ExpressionKind::Literal(lit,) => {
                      assert_eq!(&lit.kind, &LiteralKind::Integer);
                      assert_eq!(lookup(lit.symbol.idx), "5");
                    }
                    _ => panic!("Should be a literal"),
                  }

                  assert_eq!(op, &BinOpKind::PLUS);

                  // Test the rhs
                  match &rhs.kind {
                    ExpressionKind::Literal(lit,) => {
                      assert_eq!(&lit.kind, &LiteralKind::Integer);
                      assert_eq!(lookup(lit.symbol.idx), "6");
                    }
                    _ => panic!("Should be a literal"),
                  }
                }
                _ => panic!("Should be a binop"),
              }
              // Test the second tuple element
              match &inner[1].kind {
                ExpressionKind::Literal(lit,) => {
                  assert_eq!(&lit.kind, &LiteralKind::Integer);
                  assert_eq!(lookup(lit.symbol.idx), "7");
                }
                _ => panic!("Should be a literal"),
              }

              // Test the third tuple element
              match &inner[2].kind {
                ExpressionKind::Literal(lit,) => {
                  assert_eq!(&lit.kind, &LiteralKind::Integer);
                  assert_eq!(lookup(lit.symbol.idx), "8");
                }
                _ => panic!("Should be a literal"),
              }
            }
            _ => panic!("Should be a tuple"),
          },
          _ => panic!("Should be a local initiaton"),
        }
      }
      _ => unreachable!(),
    }

    // Fourth Declaration
    match ast.statements.clone()[3].kind.clone() {
      StatementKind::Let(local,) => {
        // Check mutability
        match local.pat.clone().kind.clone() {
          PatKind::Ident(raw,) => {
            assert_eq!(raw.mutable, false);
            assert_eq!(lookup(raw.symbol.idx), "value_3");
          }
          _ => panic!("Should be an identity"),
        }

        // Check the expression
        match &local.kind {
          LocalKind::Init(local,) => {
            match &local.kind {
              ExpressionKind::BinOp(lhs, op, rhs,) => {
                // Test the lhs
                match &lhs.kind {
                  ExpressionKind::BinOp(lhs, op, rhs,) => {
                    // Test the lhs
                    match &lhs.kind {
                      ExpressionKind::Unary(op, exp,) => {
                        assert_eq!(op, &UnOp::Not);
                        match &exp.kind {
                          ExpressionKind::Literal(lit,) => {
                            assert_eq!(&lit.kind, &LiteralKind::Bool);
                            assert_eq!(lookup(lit.symbol.idx), "false");
                          }
                          _ => panic!("Should be a literal"),
                        }
                      }
                      _ => panic!("Should be a unary op"),
                    }

                    // Test the op
                    assert_eq!(op, &BinOpKind::AND);

                    // Test the rhs
                    match &rhs.kind {
                      ExpressionKind::Literal(lit,) => {
                        assert_eq!(&lit.kind, &LiteralKind::Bool);
                        assert_eq!(lookup(lit.symbol.idx), "true");
                      }
                      _ => panic!("Should be a literal"),
                    }
                  }
                  _ => panic!("Should be a binop"),
                }

                // Test the op
                assert_eq!(op, &BinOpKind::OR);

                // Test the rhs
                match &rhs.kind {
                  ExpressionKind::Literal(lit,) => {
                    assert_eq!(&lit.kind, &LiteralKind::Bool);
                    assert_eq!(lookup(lit.symbol.idx), "false");
                  }
                  _ => panic!("Should be a literal"),
                }
              }
              _ => panic!("Should be a binop"),
            }
          }
          _ => panic!("Should be a local initiaton"),
        }
      }
      _ => panic!("Should be a local initiaton"),
    }
  }

  #[test]
  fn test_reassignment() {
    let source = r#"
    // Check [re]assignment works
    let value_test = 5;
    value_test += 6;
    "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();

    match ast.statements.clone()[0].kind.clone() {
      StatementKind::Let(local,) => {
        // Check mutability
        match local.pat.clone().kind.clone() {
          PatKind::Ident(raw,) => {
            assert_eq!(raw.mutable, false);
            assert_eq!(lookup(raw.symbol.idx), "value_test");
          }
          _ => panic!("Should be an identity"),
        }

        // Check the expression
        match &local.kind {
          LocalKind::Init(local,) => match &local.kind {
            ExpressionKind::Literal(lit,) => {
              assert_eq!(&lit.kind, &LiteralKind::Integer);
              assert_eq!(lookup(lit.symbol.idx), "5");
            }
            _ => panic!("Should be a literal"),
          },
          _ => panic!("Should be a local initiaton"),
        }
      }
      _ => unreachable!(),
    }

    match ast.statements.clone()[1].kind.clone() {
      StatementKind::Expression(expr,) => match expr.kind {
        ExpressionKind::AssignOp(ident, op, val,) => {
          match &ident.kind {
            ExpressionKind::Ident(ident,) => {
              assert_eq!(lookup(ident.symbol.idx), "value_test")
            }
            _ => panic!("Should be an ident"),
          }

          assert_eq!(op, AssignOpKind::PLUS_EQUAL);

          match &val.kind {
            ExpressionKind::Literal(lit,) => {
              assert_eq!(&lit.kind, &LiteralKind::Integer);
              assert_eq!(lookup(lit.symbol.idx), "6");
            }
            _ => panic!("Should be a literal"),
          }
        }
        _ => panic!("{:?}Should be an assign", expr.kind),
      },
      _ => unreachable!(),
    }
  }

  #[test]
  fn test_if_expression() {
    let source = r#"
    let test = if true != false {
      let a = 5;
    }
    else {
      let a = 6;
    };
    "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();

    match ast.statements.clone()[0].kind.clone() {
      StatementKind::Let(local,) => {
        // Check mutability
        match local.pat.clone().kind.clone() {
          PatKind::Ident(raw,) => {
            assert_eq!(raw.mutable, false);
            assert_eq!(lookup(raw.symbol.idx), "test");
          }
          _ => panic!("Should be an identity"),
        }

        // Check the expression
        match &local.kind {
          LocalKind::Init(local,) => {
            match &local.kind {
              ExpressionKind::If(cond, body, else_body,) => {
                // Check the condition
                match &cond.kind {
                  ExpressionKind::BinOp(lhs, op, rhs,) => {
                    // Test lhs
                    match &lhs.kind {
                      ExpressionKind::Literal(lit,) => {
                        assert_eq!(&lit.kind, &LiteralKind::Bool);
                        assert_eq!(lookup(lit.symbol.idx), "true");
                      }
                      _ => panic!("Should be a literal"),
                    }
                    // Test op
                    assert_eq!(op, &BinOpKind::NOT_EQUAL);

                    // Test rhs
                    match &rhs.kind {
                      ExpressionKind::Literal(lit,) => {
                        assert_eq!(&lit.kind, &LiteralKind::Bool);
                        assert_eq!(lookup(lit.symbol.idx), "false");
                      }
                      _ => panic!("Should be a literal"),
                    }
                  }
                  _ => panic!("Should be a binop"),
                }

                // Check the body
                match &body.inner_block[0].kind {
                  StatementKind::Let(local,) => {
                    // Check mutability
                    match local.pat.clone().kind.clone() {
                      PatKind::Ident(raw,) => {
                        assert_eq!(raw.mutable, false);
                        assert_eq!(lookup(raw.symbol.idx), "a");
                      }
                      _ => panic!("Should be an identity"),
                    }

                    match &local.kind {
                      LocalKind::Init(local,) => match &local.kind {
                        ExpressionKind::Literal(lit,) => {
                          assert_eq!(&lit.kind, &LiteralKind::Integer);
                          assert_eq!(lookup(lit.symbol.idx), "5");
                        }
                        _ => panic!("Should be a literal"),
                      },
                      _ => panic!("Should be local declaration"),
                    }
                  }
                  _ => unreachable!(),
                }

                // Check the else
                match &else_body.as_ref().unwrap().inner_block[0].kind {
                  StatementKind::Let(local,) => {
                    // Check mutability
                    match local.pat.clone().kind.clone() {
                      PatKind::Ident(raw,) => {
                        assert_eq!(raw.mutable, false);
                        assert_eq!(lookup(raw.symbol.idx), "a");
                      }
                      _ => panic!("Should be an identity"),
                    }

                    match &local.kind {
                      LocalKind::Init(local,) => match &local.kind {
                        ExpressionKind::Literal(lit,) => {
                          assert_eq!(&lit.kind, &LiteralKind::Integer);
                          assert_eq!(lookup(lit.symbol.idx), "6");
                        }
                        _ => panic!("Should be a literal"),
                      },
                      _ => panic!("Should be local declaration"),
                    }
                  }
                  _ => unreachable!(),
                }
              }
              _ => panic!("Should be an if expression"),
            }
          }
          _ => panic!("Should be local declaration"),
        }
      }
      _ => unreachable!(),
    }
  }

  #[test]
  fn test_while_loop() {
    let source = r#"
    while true {
      let mut value_test = 5;
      value_test += 6
      ;
    };
    "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();

    match ast.statements.clone()[0].kind.clone() {
      StatementKind::Expression(expr,) => match &expr.kind {
        ExpressionKind::WhileLoop(cond, body,) => {
          // Test the condition
          match &cond.kind {
            ExpressionKind::Literal(lit,) => {
              assert_eq!(&lit.kind, &LiteralKind::Bool);
              assert_eq!(lookup(lit.symbol.idx), "true");
            }
            _ => panic!("Should be a literal"),
          }

          // Test the body
          match body.inner_block.clone()[0].kind.clone() {
            StatementKind::Let(local,) => {
              // Check mutability
              match local.pat.clone().kind.clone() {
                PatKind::Ident(raw,) => {
                  assert_eq!(raw.mutable, true);
                  assert_eq!(lookup(raw.symbol.idx), "value_test");
                }
                _ => panic!("Should be an identity"),
              }

              // Check the expression
              match &local.kind {
                LocalKind::Init(local,) => match &local.kind {
                  ExpressionKind::Literal(lit,) => {
                    assert_eq!(&lit.kind, &LiteralKind::Integer);
                    assert_eq!(lookup(lit.symbol.idx), "5");
                  }
                  _ => panic!("Should be a literal"),
                },
                _ => panic!("Should be a local initiaton"),
              }
            }
            _ => unreachable!(),
          }

          match body.inner_block.clone()[1].kind.clone() {
            StatementKind::Expression(expr,) => match expr.kind {
              ExpressionKind::AssignOp(ident, op, val,) => {
                match &ident.kind {
                  ExpressionKind::Ident(ident,) => {
                    assert_eq!(lookup(ident.symbol.idx), "value_test")
                  }
                  _ => panic!("Should be an ident"),
                }

                assert_eq!(op, AssignOpKind::PLUS_EQUAL);

                match &val.kind {
                  ExpressionKind::Literal(lit,) => {
                    assert_eq!(&lit.kind, &LiteralKind::Integer);
                    assert_eq!(lookup(lit.symbol.idx), "6");
                  }
                  _ => panic!("Should be a literal"),
                }
              }
              _ => panic!("{:?}Should be an assign", expr.kind),
            },
            _ => unreachable!(),
          }
        }
        _ => panic!("Should be a while loop"),
      },
      _ => unreachable!(),
    }
  }

  #[test]
  fn test_for_loop() {
    let source = r#"
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

  #[test]
  fn test_fn_declaration() {
    // Add return parsing
    let source = r#"
    fn test(num:int, string:str, boolean:bool,){
      let t = 4; 
    };

    fn test_2(num:int, string:str, boolean:bool) -> bool {
      let t = 4; 
      return t;
    };
    "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();
  }
}
