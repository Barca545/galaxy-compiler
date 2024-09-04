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
// - next has higher precedence might need a less than sign
// - BinOps should error if both sides are not the same type
// - Panics need to be turned into actual errors
// - I don't think I need an else if token since else takes an expression, the
//   next expression can just be another if
// - Should if statements error if they reachpu an EOF without another brace?
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
    if self.peek().kind != token {
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
    let mutable = self.parse_mutability();

    let token = self
      .eat_token_expect(TokenKind::IDENTIFIER(Symbol::from(0,),), "Expected an identifier",)
      .unwrap();
    match token.kind {
      TokenKind::IDENTIFIER(symbol,) => RawIdent {
        symbol,
        mutable,
        loc:token.loc,
      },
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
      self.eat_token_expect(TokenKind::TYPE(Symbol::from(0,),), "Expected type declaration",).unwrap();

      let token = self.next();

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
