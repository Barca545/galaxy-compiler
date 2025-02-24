use super::token::{Chunk, Token};
use crate::token::Location;
use std::{iter::Peekable, vec::IntoIter};

// Refactor:
// - Move catching types out of tokenization? This will make it easier to add
//   types later if desired.
// - I think I need to rework how chunks work/remove them from the tokenizer.
// - Consider making the next function part of push[ing] a ch to the chunk
// - Create a read_string function that just reads a full string and then
//   interns it before returning that for the string literals to use
// - Fix errors to pretty print
// - Add the len > 0 check into the chunk to token somehow
// - Add multiline comments
// - Do I need the length checks?

pub type TokenStream = Vec<Token,>;

pub struct Tokenizer {
  source:Peekable<IntoIter<char,>,>,
  ///Current [`Chunk`].
  current:Chunk,
  ///[`TokenStream`] to return.
  tokens:TokenStream,
  /// Current [`Location`] in the source file.
  loc:Location,
}

impl Tokenizer {
  pub fn new() -> Self {
    Tokenizer {
      source:"".chars().collect::<Vec<char,>>().into_iter().peekable(),
      current:Chunk::new(),
      tokens:Vec::new(),
      loc:Location::new(),
    }
  }

  ///Increment the [`Tokenizer`]'s [`Location`].
  fn next(&mut self,) -> Option<char,> {
    let next = self.source.next();
    let newline = if next == Some('\n',) { true } else { false };
    self.loc.next(newline,);
    next
  }

  fn eat_whitespace(&mut self,) {
    while let Some(ch,) = self.source.peek() {
      match ch.is_whitespace() {
        true => self.next(),
        false => break,
      };
    }
  }

  fn eat_comment(&mut self,) {
    while let Some(ch,) = self.source.peek() {
      match *ch == '\n' {
        true => break,
        false => self.next(),
      };
    }
  }

  ///Turn the current [`Chunk`] into a [`Token`]. Add it to the [`TokenStream`]
  /// and ready a new [`Chunk`].
  fn new_chunk(&mut self, ch:char,) {
    // Set the new Token's location
    if self.current.len() == 0 {
      self.current.start.col = self.loc.col - 1;
      self.current.start.line = self.loc.line;
    }
    self.current.push(ch,);
  }

  fn read_string(&mut self,) {
    self.new_chunk('\"',);
    while let Some(ch,) = self.source.next() {
      //Append to chunk
      self.current.push(ch,);
      if ch == '\"' {
        self.tokens.push(self.current.to_token(),);
        break;
      }
    }
  }

  pub fn tokenize(&mut self, source:&str,) -> TokenStream {
    //Plug in the incoming source file
    self.source = source.chars().collect::<Vec<char,>>().into_iter().peekable();

    while let Some(ch,) = self.next() {
      match ch {
        // The following block controls "reading" comments
        '/' => {
          if *self.source.peek().unwrap() == '/' {
            self.eat_comment()
          }
          //If not a comment just push
          else {
            self.current.push(ch,)
          }
        }
        // The following block controls "reading" strings
        '\"' => self.read_string(),
        ' ' | '\n' | '\r' | '\t' => {
          if self.current.len() > 0 {
            self.tokens.push(self.current.to_token(),)
          }
          self.eat_whitespace()
        }
        ',' | ':' | ';' | '(' | ')' | '{' | '}' | '[' | ']' => {
          if self.current.len() > 0 {
            self.tokens.push(self.current.to_token(),);
          }
          self.new_chunk(ch,);
          self.tokens.push(self.current.to_token(),);
        }
        // Handle unary operators
        '!' | '-' => {
          if self.current.len() > 0 {
            self.tokens.push(self.current.to_token(),);
          }

          self.current.push(ch,);
          //Check for a not equal
          if ch == '!' && Some(&'=',) == self.source.peek() {
            let ch = self.next().unwrap();
            self.current.push(ch,);
          }
          //Check for an arrow
          if ch == '-' && Some(&'>',) == self.source.peek() {
            let ch = self.next().unwrap();
            self.current.push(ch,);
          }
          self.tokens.push(self.current.to_token(),);
        }
        '.' => {
          // Break tokenizing when it encounters a ".."
          // Check if it encounters a period outside a comment if the next char is
          // a period and if it a period tokenize the old chunk, then make a new chunk
          if *self.source.peek().unwrap() == '.' {
            // Tokenize the current chunk and start a new one
            if self.current.len() > 0 {
              self.tokens.push(self.current.to_token(),);
            }
            if self.current.len() == 0 {
              self.current.start.col = self.loc.col - 1;
              self.current.start.line = self.loc.line;
            }
            self.current.push(ch,);
            let next = self.next().unwrap();
            self.current.push(next,);

            if *self.source.peek().unwrap() == '=' {
              let next = self.next().unwrap();
              self.current.push(next,);
            }
            self.tokens.push(self.current.to_token(),);
          }
          else {
            self.current.push(ch,);
          }
        }
        _ => self.new_chunk(ch,),
      }
    }
    //Add the EOF token
    self.tokens.push(Token::new(None, self.loc,),);

    self.tokens.clone()
  }
}

#[cfg(test)]
mod test {
  use crate::{symbol_table::Symbol, token::TokenKind, tokenizer::Tokenizer};
  use std::fs;

  // Figure out the problem with the text file not having a paragraph break at the
  // end currently skips the last character
  #[test]
  fn tokens_are_generated() {
    let source = fs::read_to_string("src/tests/tokenizing_test.txt",).unwrap();

    let mut tokenizer = Tokenizer::new();
    let tokens = tokenizer.tokenize(&source,);

    assert_eq!(tokens[0].kind, TokenKind::LEFT_PAREN);
    assert_eq!(tokens[1].kind, TokenKind::RIGHT_PAREN);
    assert_eq!(tokens[2].kind, TokenKind::LEFT_BRACKET);
    assert_eq!(tokens[3].kind, TokenKind::RIGHT_BRACKET);
    assert_eq!(tokens[4].kind, TokenKind::LEFT_BRACE);
    assert_eq!(tokens[5].kind, TokenKind::RIGHT_BRACE);
    assert_eq!(tokens[6].kind, TokenKind::LET);
    assert_eq!(tokens[7].kind, TokenKind::MUT);
    assert_eq!(tokens[8].kind, TokenKind::IDENTIFIER(Symbol::from("a"),));
    assert_eq!(tokens[9].kind, TokenKind::EQUAL);
    assert_eq!(tokens[10].kind, TokenKind::STRING(Symbol::from("\"test string\"",),));
    assert_eq!(tokens[11].kind, TokenKind::LET);
    assert_eq!(tokens[12].kind, TokenKind::IDENTIFIER(Symbol::from("b"),));
    assert_eq!(tokens[13].kind, TokenKind::EQUAL);
    assert_eq!(tokens[14].kind, TokenKind::FLOAT(4.5,));
    assert_eq!(tokens[15].kind, TokenKind::IDENTIFIER(Symbol::from("b",),),);
    assert_eq!(tokens[16].kind, TokenKind::EQUAL);
    assert_eq!(tokens[17].kind, TokenKind::FLOAT(5.0,));
    assert_eq!(tokens[18].kind, TokenKind::INT(1,),);
    assert_eq!(tokens[19].kind, TokenKind::RANGE_RIGHT_EX);
    assert_eq!(tokens[20].kind, TokenKind::INT(2,));
    assert_eq!(tokens[21].kind, TokenKind::INT(1,));
    assert_eq!(tokens[22].kind, TokenKind::RANGE_RIGHT_IN);
    assert_eq!(tokens[23].kind, TokenKind::INT(2,),);
  }

  #[test]
  fn token_location_is_correct() {
    let source = fs::read_to_string("src/tests/tokenizing_test.txt",).unwrap();

    let mut tokenizer = Tokenizer::new();
    let tokens = tokenizer.tokenize(&source,);

    // Check the locations
    assert_eq!((tokens[0].loc.line, tokens[0].loc.col), (3, 1));
    assert_eq!((tokens[1].loc.line, tokens[1].loc.col), (3, 2));
    assert_eq!((tokens[2].loc.line, tokens[2].loc.col), (3, 5));
    assert_eq!((tokens[3].loc.line, tokens[3].loc.col), (3, 6));
    assert_eq!((tokens[4].loc.line, tokens[4].loc.col), (3, 8));
    assert_eq!((tokens[5].loc.line, tokens[5].loc.col), (3, 9));
    assert_eq!((tokens[6].loc.line, tokens[6].loc.col), (4, 1));
    assert_eq!((tokens[7].loc.line, tokens[7].loc.col), (4, 5));
    assert_eq!((tokens[8].loc.line, tokens[8].loc.col), (4, 9));
    assert_eq!((tokens[9].loc.line, tokens[9].loc.col), (4, 11));
    assert_eq!((tokens[10].loc.line, tokens[10].loc.col), (4, 13));
    assert_eq!((tokens[11].loc.line, tokens[11].loc.col), (6, 1));
    assert_eq!((tokens[12].loc.line, tokens[12].loc.col), (6, 5));
    assert_eq!((tokens[13].loc.line, tokens[13].loc.col), (6, 7));
    assert_eq!((tokens[11].loc.line, tokens[11].loc.col), (6, 1));
    assert_eq!((tokens[14].loc.line, tokens[14].loc.col), (6, 9));
    assert_eq!((tokens[15].loc.line, tokens[15].loc.col), (7, 1));
    assert_eq!((tokens[16].loc.line, tokens[16].loc.col), (7, 3));
    assert_eq!((tokens[11].loc.line, tokens[11].loc.col), (6, 1));
    assert_eq!((tokens[17].loc.line, tokens[17].loc.col), (7, 5));
    assert_eq!((tokens[18].loc.line, tokens[18].loc.col), (8, 1));
    assert_eq!((tokens[19].loc.line, tokens[19].loc.col), (8, 2));
    assert_eq!((tokens[20].loc.line, tokens[20].loc.col), (8, 4));
    assert_eq!((tokens[21].loc.line, tokens[21].loc.col), (8, 6));
    assert_eq!((tokens[22].loc.line, tokens[22].loc.col), (8, 7));
    assert_eq!((tokens[23].loc.line, tokens[23].loc.col), (8, 10));
  }
}
