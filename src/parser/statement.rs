use super::Parser;
use crate::{
  ast::{Local, LocalKind, Statement, StatementKind, P},
  token::TokenKind,
};
use eyre::Result;

impl Parser {
  ///Returns a [`Statement`].
  pub fn parse_statement(&mut self,) -> Result<Statement,> {
    let statement = match self.peek().kind {
      TokenKind::LET => self.parse_let_statement(),
      _ => self.parse_expression_statement(),
    };
    self.eat_token_expect(TokenKind::SEMICOLON, "Statements must end with a semicolon.",).unwrap();
    Ok(statement,)
  }

  ///Parse an [`Expression`](crate::ast::Expression) as a [`Statement`].
  pub fn parse_expression_statement(&mut self,) -> Statement {
    Statement {
      id:0,
      loc:self.peek().loc,
      kind:StatementKind::Expression(self.parse_expression(0,),),
    }
  }

  ///Parse a `let` [`Statement`] (`let <pat>::<ty> = <expr>`).
  pub fn parse_let_statement(&mut self,) -> Statement {
    let token = self.next();
    //Location of the first element in the statement
    let id = 0;
    let loc = token.loc;

    let pat = self.parse_pattern();

    //Check for type
    let ty = self.parse_type();

    let local = Local {
      id,
      loc,
      ty,
      pat:P::new(pat,),
      kind:self.parse_local_kind(),
    };

    let stmt_kind = StatementKind::Let(P::new(local,),);

    Statement::new(loc, stmt_kind,)
  }

  pub fn parse_local_kind(&mut self,) -> LocalKind {
    // If equals sign, parse the output of the next statement, expect an Expression.
    if self.eat_token_if_match(TokenKind::EQUAL,) {
      let expression = self.parse_expression(0,);
      return LocalKind::Init(P::new(expression,),);
    }

    LocalKind::Decl
  }
}
