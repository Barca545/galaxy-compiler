use super::Parser;
use crate::{
  ast::{FnSig, Item, ItemKind, Statement, StatementKind, Ty, P},
  interner::lookup,
  token::TokenKind,
};

// TODO
// - Figure out how to add type verification
// - Param loop should work even if the code tuple does not have a trailing
//   comma
// - If there are no user defined types, the AST and token types can be merged
//   closer

// How does the Fn Item map the params to the calls in the block?
// I assume this is where the symbol table comes in?

impl Parser {
  //Create an item node type/statement
  //functions are a type of item
  //is it easier to just make functions a type of statement or unique type
  // might do it since I might need enums and arguably structs
  pub fn parse_item(&mut self,) -> Statement {
    let token = self.next();
    match token.kind {
      TokenKind::FN => Statement {
        id:0,
        loc:token.loc,
        kind:StatementKind::Item(self.parse_fn(),),
      },
      _ => unreachable!(),
    }
  }

  pub fn parse_fn(&mut self,) -> Item {
    //Parse the identity (funtion name)
    let name = self.parse_raw_ident();

    //Parse the args
    let sig = self.parse_fn_signature();

    //Parse the block that makes up the body
    let body = P::new(self.parse_block(),);

    Item {
      id:0,
      loc:name.loc,
      name:name.symbol,
      kind:ItemKind::Fn { sig, body, },
    }
  }

  /// Parses the parameter list, including the `(` and `)`
  /// delimiters, and result type of a function declaration..
  pub fn parse_fn_signature(&mut self,) -> FnSig {
    self
      .eat_token_expect(TokenKind::LEFT_PAREN, "Syntax Error: expected function arguments",)
      .unwrap();

    let mut params = Vec::new();
    //Loop over the param pairs in the tuple and append them to the params
    while let TokenKind::IDENTIFIER(param,) = self.peek().kind {
      self.next();
      self.eat_token_expect(TokenKind::COLON, "Expected a colon",).unwrap();

      if let TokenKind::TYPE(ty,) = self.next().kind {
        params.push(Ty::new(lookup(ty.idx,),).unwrap(),);
      }
      else {
        panic!("Parameter \"{}\" must be a recognized type!", lookup(param.idx))
      };

      // Eat the comma / also eats trailing commas
      self.eat_token_if_match(TokenKind::COMMA,);
    }

    self
      .eat_token_expect(
        TokenKind::RIGHT_PAREN,
        "Syntax Error: expected function
    arguments",
      )
      .unwrap();

    //Eat if there's an arrow and expect a result type declaration
    let mut result = None;

    //Eat if there's an arrow and expect a result type declaration
    if self.eat_token_if_match(TokenKind::ARROW,) {
      if let TokenKind::TYPE(ty,) = self.next().kind {
        result = Ty::new(lookup(ty.idx,),)
      }
      else {
        panic!("Expected a type!")
      };
    }

    FnSig { params, result, }
  }
}
