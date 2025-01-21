use super::Parser;
use crate::{
  ast::{AssignOpKind, BinOpKind, Expression, ExpressionKind, IdentInner, Literal, UnOp, P},
  token::TokenKind,
};

impl Parser {
  ///Returns an [`Expression`].
  pub fn parse_expression(&mut self, rbp:u32,) -> Expression {
    // Get the left hand expression
    let mut left = self.parse_expression_null_detonation();

    // Keep eating tokens as long as the next token's lbp is greater than
    // the current token -- lower lbp will cause the loop to terminate
    while self.next_is_less_precedence(rbp,) {
      left = self.parse_expression_left_denotation(left,)
    }
    left
  }

  ///Converts the current [`Token`] into an [`Option<Expression>`].
  ///
  /// If the `Token` can be parsed as a prefix, returns an [`Expression`].
  ///
  /// Otherwise returns `None`.
  fn parse_expression_null_detonation(&mut self,) -> Expression {
    // Should be a prefix
    match self.peek().kind {
      TokenKind::INT(_,) | TokenKind::FLOAT(_,) | TokenKind::BOOL(_,) => self.parse_literal_expression(),
      TokenKind::IDENTIFIER(_,) => self.parse_ident_expression(),
      TokenKind::IF => self.parse_if_expression(),
      TokenKind::LEFT_PAREN => self.parse_tuple_expression(),
      TokenKind::WHILE => self.parse_while_expression(),
      TokenKind::FOR => self.parse_for_expression(),
      TokenKind::BREAK => {
        let token = self.next();
        Expression {
          id:0,
          kind:ExpressionKind::Break,
          loc:token.loc,
        }
      }
      TokenKind::CONTINUE => {
        let token = self.next();
        Expression {
          id:0,
          kind:ExpressionKind::Continue,
          loc:token.loc,
        }
      }
      TokenKind::NOT | TokenKind::MINUS => {
        let token = self.next();
        let unop = UnOp::from(&token,);
        let expresion = self.parse_expression(60,);
        Expression {
          id:0,
          kind:ExpressionKind::Unary(unop, P::new(expresion,),),
          loc:token.loc,
        }
      }
      TokenKind::RETURN => self.parse_return(),
      _ => panic!("Cannot begin an expression with {:?}", self.peek().kind),
    }
  }

  ///Returns an [`Expression`] consisting of the currently parsed
  /// [`Token`](crate::token::Token)s and the next `Token`.
  fn parse_expression_left_denotation(&mut self, left:Expression,) -> Expression {
    let loc = left.loc;
    match self.peek().kind {
      // BinOps
      TokenKind::MINUS
      | TokenKind::PLUS
      | TokenKind::STAR
      | TokenKind::SLASH
      | TokenKind::EQUAL_EQUAL
      | TokenKind::NOT_EQUAL
      | TokenKind::GREATER
      | TokenKind::GREATER_EQUAL
      | TokenKind::LESS
      | TokenKind::AND
      | TokenKind::OR
      | TokenKind::LESS_EQUAL => {
        let token = self.next();
        let binop_kind = BinOpKind::from(token,);
        let right = self.parse_expression(token.lbp(),);
        let kind = ExpressionKind::BinOp(P::new(left,), binop_kind, P::new(right,),);
        Expression { id:0, kind, loc, }
      }
      // Assignment
      TokenKind::EQUAL => {
        let token = self.next();
        let right = self.parse_expression(token.lbp(),);
        let kind = ExpressionKind::Assign(P::new(left,), P::new(right,),);
        Expression { id:0, kind, loc, }
      }
      // AssignOps
      TokenKind::STAR_EQUAL | TokenKind::SLASH_EQUAL | TokenKind::PLUS_EQUAL | TokenKind::MINUS_EQUAL => {
        let token = self.next();
        let assignop_kind = AssignOpKind::from(token,);
        let right = self.parse_expression(token.lbp(),);
        let kind = ExpressionKind::AssignOp(P::new(left,), assignop_kind, P::new(right,),);
        Expression { id:0, kind, loc, }
      }
      // Range expression
      TokenKind::RANGE_RIGHT_EX | TokenKind::RANGE_RIGHT_IN => {
        let token = self.next();
        let right = self.parse_expression(token.lbp(),);
        let kind = ExpressionKind::Range {
          left:P::new(left,),
          inclusive:matches!(token.kind, TokenKind::RANGE_RIGHT_IN),
          right:P::new(right,),
        };
        Expression { id:0, kind, loc, }
      }
      _ => {
        let token = self.next();
        panic!("{:?}", token.kind)
      }
    }
  }

  fn parse_tuple_expression(&mut self,) -> Expression {
    let token = self.next();

    let mut elements = Vec::new();

    // parse an expression until a comma is encountered then push it to the
    // elements vec end when a ] is encountered
    while self.peek().kind != TokenKind::RIGHT_PAREN {
      elements.push(P::new(self.parse_expression(0,),),);
      // Skip over the comma
      self.eat_token_if_match(TokenKind::COMMA,);
    }

    // Error if the array is not closed
    self.eat_token_expect(TokenKind::RIGHT_PAREN, "Must close the array.",).unwrap();

    //Return the expression
    Expression {
      id:0,
      kind:ExpressionKind::Tuple(elements,),
      loc:token.loc,
    }
  }

  ///Return an [`Expression`] containing a [`Literal`].
  fn parse_literal_expression(&mut self,) -> Expression {
    let token = self.next();
    let val = match token.kind {
      TokenKind::INT(i,) => Literal::Integer(i,),
      TokenKind::FLOAT(f,) => Literal::Float(f,),
      TokenKind::BOOL(b,) => Literal::Bool(b,),
      _ => unreachable!(),
    };

    Expression {
      id:0,
      kind:ExpressionKind::Literal(P::new(val,),),
      loc:token.loc,
    }
  }

  fn parse_ident_expression(&mut self,) -> Expression {
    let token = self.peek();
    match token.kind {
      TokenKind::IDENTIFIER(_,) => Expression {
        id:0,
        kind:ExpressionKind::Ident(IdentInner::Raw(self.parse_raw_ident(),),),
        loc:token.loc,
      },
      _ => unreachable!(),
    }
  }

  fn parse_if_expression(&mut self,) -> Expression {
    let token = self.next();
    // Parse the conditional expression
    let condition = self.parse_expression(0,);
    let loc = token.loc;

    // Parse the if block
    let if_body = self.parse_block();

    // Check for an else statment and parse it if so
    let else_body = if self.eat_token_if_match(TokenKind::ELSE,) {
      Some(P::new(self.parse_block(),),)
    }
    else {
      None
    };

    Expression {
      id:0,
      kind:ExpressionKind::If(P::new(condition,), P::new(if_body,), else_body,),
      loc,
    }
  }

  fn parse_while_expression(&mut self,) -> Expression {
    let token = self.next();

    Expression {
      id:0,
      kind:ExpressionKind::WhileLoop(P::new(self.parse_expression(0,),), P::new(self.parse_block(),),),
      loc:token.loc,
    }
  }

  fn parse_for_expression(&mut self,) -> Expression {
    let token = self.next();
    let pat = self.parse_pattern();

    self.eat_token_expect(TokenKind::IN, "Missing `in` in `for` loop",).unwrap();

    // Should be a range expression
    let iter = self.parse_expression(0,);

    Expression {
      id:0,
      kind:ExpressionKind::ForLoop {
        pat:P::new(pat,),
        iter:P::new(iter,),
        body:P::new(self.parse_block(),),
      },
      loc:token.loc,
    }
  }

  fn parse_return(&mut self,) -> Expression {
    let token = self.next();

    //If there is more tokens treat that as the expression to return
    let inner = match self.peek().kind {
      TokenKind::SEMICOLON => None,
      _ => Some(P::new(self.parse_expression(0,),),),
    };

    Expression {
      id:0,
      kind:ExpressionKind::Return(inner,),
      loc:token.loc,
    }
  }
}
