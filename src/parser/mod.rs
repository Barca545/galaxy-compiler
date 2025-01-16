mod error_reporting;
mod expression;
mod item;
mod pattern;
mod statement;

use crate::{
  ast::{AbstractSyntaxTree, Block, RawIdent, Ty, P},
  errors::ParsingError,
  interner::lookup,
  symbol_table::Symbol,
  token::{Token, TokenKind},
  tokenizer::Tokenizer,
};
use eyre::Result;
use std::{iter::Peekable, vec::IntoIter};

// Rust uses Paths instead of just identifiers but since I am not doing multiple
// modules and don't need to support that there is no need

// Refactor:
// - Each AST node needs a unique index. Can't all be 0.
// - next has higher precedence might need a less than sign
// - BinOps should error if both sides are not the same type
// - Panics need to be turned into actual errors
// - I don't think I need an else if token since else takes an expression, the
//   next expression can just be another if
// - Should if statements error if they reach an EOF without another brace?
// - Do I need the cursor?
// - I think the source can be a constant instead of a field on parser.
// - For ease of use maybe the parsing for let statements and the parsing for
//   the identifier pattern can be distinct
// - Could parse pattern be a match statement

// TO DO:
// - Use ANSI escape codes to color the errors
// - Roll the match statements in the parse function into a parse_rule() method

///Structure used to generate an AST from a
/// [`TokenStream`](crate::tokenizer::TokenStream).
pub struct Parser {
  cursor:usize,
  source:String,
  tokens:Peekable<IntoIter<Token,>,>,
  had_err:bool,
  erroring:bool,
}

impl Parser {
  ///Load a [`TokenStream`](crate::tokenizer::TokenStream) into the [`Parser`].
  pub fn new(source:&str,) -> Self {
    let mut tokenizer = Tokenizer::new();
    let tokens = tokenizer.tokenize(&source,).into_iter().peekable();

    Parser {
      cursor:0,
      source:source.to_string(),
      tokens,
      had_err:false,
      erroring:false,
    }
  }

  /// Checks whether the current [`Token`] has a higher precedence than the
  /// following `Token`.
  fn next_is_less_precedence(&mut self, rbp:u32,) -> bool {
    self.peek().lbp() > rbp
  }

  ///Increments the `cursor` and returns the [`Token`] the cursor now points
  /// to.
  fn next(&mut self,) -> Token {
    self.cursor += 1;
    self.tokens.next().unwrap()
  }

  ///Returns a reference to the next [`Token`].
  fn peek(&mut self,) -> Token {
    *self.tokens.peek().unwrap()
  }

  ///Consumes the next [`Token`].
  ///
  /// For [`TokenKind`]s with fields, the comparison will ignore their fields'
  /// value.
  ///
  ///# Panics
  /// - Panics if the current `Token` is not the expected `TokenKind`.
  fn eat_token_expect(&mut self, token:TokenKind, msg:&str,) -> Result<Token,> {
    if std::mem::discriminant(&self.peek().kind,) != std::mem::discriminant(&token,) {
      self.had_err = true;
      Err(
        ParsingError::UnexpectedToken {
          expected:token,
          recieved:self.peek().kind,
        }
        .into(),
      )
    }
    else {
      Ok(self.next(),)
    }
  }

  /// If the next token is the given [`TokenKind`], returns true.
  fn check_keyword(&mut self, token:TokenKind,) -> bool {
    self.peek().kind == token
  }

  /// If the next [`Token`] is the given keyword, eats it and returns `true`.
  ///
  /// For [`TokenKind`]s with fields, the comparison will ignore their fields'
  /// value.
  ///
  /// An expectation is added for diagnostics purposes.
  pub fn eat_token_if_match(&mut self, token:TokenKind,) -> bool {
    // make this take in a closure that spits our a bool
    if self.check_keyword(token,) {
      self.next();
      true
    }
    else {
      false
    }
  }

  ///Consume a [`TokenStream`](crate::tokenizer::TokenStream) and return an
  /// [`AbstractSyntaxTree`].
  pub fn parse(&mut self,) -> AbstractSyntaxTree {
    let mut ast = AbstractSyntaxTree::new();

    loop {
      match self.peek().kind {
        TokenKind::EOF => break,
        _ => ast.push(self.parse_statement().unwrap(),),
      }
    }

    ast
  }

  fn parse_raw_ident(&mut self,) -> RawIdent {
    let token = self
      .eat_token_expect(TokenKind::IDENTIFIER(Symbol::from(0,),), "Expected an identifier",)
      .unwrap();
    match token.kind {
      TokenKind::IDENTIFIER(symbol,) => RawIdent { symbol, loc:token.loc, },
      _ => unreachable!(),
    }
  }

  /// Parses a [`Block`] (`{Vec<Statemnent>}`).
  fn parse_block(&mut self,) -> Block {
    self
      .eat_token_expect(TokenKind::LEFT_BRACE, "Must follow if statement condition with a brace `{`",)
      .unwrap();

    let mut inner_block = Vec::new();

    while self.peek().kind != TokenKind::RIGHT_BRACE {
      inner_block.push(self.parse_statement().unwrap(),);
    }

    self.eat_token_expect(TokenKind::RIGHT_BRACE, "Blocks must end with a right brace",).unwrap();
    Block { inner_block, }
  }

  ///Checks if the next [`Token`] is mutable and consumes it if so.
  fn parse_mutability(&mut self,) -> bool {
    self.eat_token_if_match(TokenKind::MUT,)
  }

  ///Checks if the next [`Token`]s are `:<ty>` and consumes them if so.
  fn parse_type(&mut self,) -> Option<P<Ty,>,> {
    //Check for a colon if a colon is found continue otherwise return false
    if self.eat_token_if_match(TokenKind::COLON,) {
      //Check for a type if no type is found print an error

      let token = self.eat_token_expect(TokenKind::TYPE(Symbol::from(0,),), "Expected type declaration",).unwrap();

      //If the type is not a valid type, error
      if let TokenKind::TYPE(sym,) = token.kind {
        let val = lookup(sym.idx,);
        match Ty::new(val.clone(),) {
          Some(ty,) => return Some(P::new(ty,),),
          None => panic!("{} is not an accepted type", val),
        };
      }
    }
    None
  }
}

#[cfg(test)]
mod tests {
  use crate::ast::{AssignOpKind, BinOpKind, ExpressionKind, Literal, LocalKind, PatKind, StatementKind, UnOp};
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
                    assert_eq!(**lit, Literal::Integer(5));
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
                          assert_eq!(**lit, Literal::Integer(6));
                        }
                        _ => panic!("Should be a literal"),
                      }

                      // Test the op
                      assert_eq!(op, &BinOpKind::STAR);

                      match &rhs.kind {
                        ExpressionKind::Literal(lit,) => {
                          assert_eq!(**lit, Literal::Integer(7));
                        }
                        _ => panic!("Should be a literal"),
                      }
                    }

                    // Test the op
                    assert_eq!(op, &BinOpKind::SLASH);

                    // Test the rhs
                    match &rhs.kind {
                      ExpressionKind::Literal(lit,) => {
                        assert_eq!(**lit, Literal::Integer(8));
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
        assert_eq!(local.mutable, false);
        match local.pat.clone().kind.clone() {
          PatKind::Ident(raw,) => {
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
                        assert_eq!(**lit, Literal::Bool(true));
                      }
                      _ => panic!("Should be a literal"),
                    }

                    assert_eq!(op, &BinOpKind::NOT_EQUAL);

                    // Test the rhs
                    match &rhs.kind {
                      ExpressionKind::Literal(lit,) => {
                        assert_eq!(**lit, Literal::Bool(false));
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
                    assert_eq!(**lit, Literal::Bool(true));
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
                      assert_eq!(**lit, Literal::Integer(5));
                    }
                    _ => panic!("Should be a literal"),
                  }

                  assert_eq!(op, &BinOpKind::PLUS);

                  // Test the rhs
                  match &rhs.kind {
                    ExpressionKind::Literal(lit,) => {
                      assert_eq!(**lit, Literal::Integer(6));
                    }
                    _ => panic!("Should be a literal"),
                  }
                }
                _ => panic!("Should be a binop"),
              }
              // Test the second tuple element
              match &inner[1].kind {
                ExpressionKind::Literal(lit,) => {
                  assert_eq!(**lit, Literal::Integer(7));
                }
                _ => panic!("Should be a literal"),
              }

              // Test the third tuple element
              match &inner[2].kind {
                ExpressionKind::Literal(lit,) => {
                  assert_eq!(**lit, Literal::Integer(8));
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
                            assert_eq!(**lit, Literal::Bool(false));
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
                        assert_eq!(**lit, Literal::Bool(true));
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
                    assert_eq!(**lit, Literal::Bool(false));
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
            assert_eq!(lookup(raw.symbol.idx), "value_test");
          }
          _ => panic!("Should be an identity"),
        }

        // Check the expression
        match &local.kind {
          LocalKind::Init(local,) => match &local.kind {
            ExpressionKind::Literal(lit,) => {
              assert_eq!(**lit, Literal::Integer(5));
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
              assert_eq!(**lit, Literal::Integer(6));
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
                        assert_eq!(**lit, Literal::Bool(true));
                      }
                      _ => panic!("Should be a literal"),
                    }
                    // Test op
                    assert_eq!(op, &BinOpKind::NOT_EQUAL);

                    // Test rhs
                    match &rhs.kind {
                      ExpressionKind::Literal(lit,) => {
                        assert_eq!(**lit, Literal::Bool(false));
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
                        assert_eq!(lookup(raw.symbol.idx), "a");
                      }
                      _ => panic!("Should be an identity"),
                    }

                    match &local.kind {
                      LocalKind::Init(local,) => match &local.kind {
                        ExpressionKind::Literal(lit,) => {
                          assert_eq!(**lit, Literal::Integer(5));
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
                        assert_eq!(lookup(raw.symbol.idx), "a");
                      }
                      _ => panic!("Should be an identity"),
                    }

                    match &local.kind {
                      LocalKind::Init(local,) => match &local.kind {
                        ExpressionKind::Literal(lit,) => {
                          assert_eq!(**lit, Literal::Integer(6));
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
              assert_eq!(**lit, Literal::Bool(true));
            }
            _ => panic!("Should be a literal"),
          }

          // Test the body
          match body.inner_block.clone()[0].kind.clone() {
            StatementKind::Let(local,) => {
              // Check mutability
              match local.pat.clone().kind.clone() {
                PatKind::Ident(raw,) => {
                  assert_eq!(lookup(raw.symbol.idx), "value_test");
                }
                _ => panic!("Should be an identity"),
              }

              // Check the expression
              match &local.kind {
                LocalKind::Init(local,) => match &local.kind {
                  ExpressionKind::Literal(lit,) => {
                    assert_eq!(**lit, Literal::Integer(5));
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
                    assert_eq!(**lit, Literal::Integer(6));
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
