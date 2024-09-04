use super::Parser;
use crate::{
  ast::{Pat, PatKind, P},
  token::TokenKind,
};

impl Parser {
  pub fn parse_pattern(&mut self,) -> Pat {
    if let TokenKind::MUT | TokenKind::IDENTIFIER(_,) = self.peek().kind {
      let token = self.peek();
      let kind = PatKind::Ident(self.parse_raw_ident(),);
      Pat { id:0, kind, loc:token.loc, }
    }
    else if let TokenKind::LEFT_PAREN = self.peek().kind {
      todo!()
    }
    else {
      self.parse_range_pattern()
    }
  }

  pub fn parse_range_pattern(&mut self,) -> Pat {
    let token = self.next();
    //Parse the expression on the left side of the range indicator
    let start = self.parse_expression(0,);

    //Eat the range icon, panic if it is not there
    match self.peek().kind {
      TokenKind::RANGE_RIGHT_EX | TokenKind::RANGE_RIGHT_IN => {}
      _ => {
        let token = self.next();
        panic!("{:?}", token.kind)
      }
    }

    //Parse the expression on the left side of the range indicator
    let end = self.parse_expression(0,);

    let kind = PatKind::Range {
      start:P::new(start,),
      end:P::new(end,),
      inclusive:matches!(token.kind, TokenKind::RANGE_RIGHT_IN),
    };
    Pat { id:0, kind, loc:token.loc, }
  }
}
