use super::{errors::ParsingError, interner::intern};
use crate::symbol_table::Symbol;
use std::fmt::{Debug, Display};

// TODO:
// - Not sure I need the double colon operator
// - How do tokenizers actually handle type declarations
// - switch from passing in an interner to using the INTERNER directly

// Refactor:
// - Use spans (the range of indices in the source code a token covers) instead
//   of locations? Or some combo of start location and the token's span?

#[derive(Clone, Copy,)]
pub(super) struct Location {
  pub(super) line:u32,
  pub(super) col:u32,
  pub(super) index:usize,
}

impl Debug for Location {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    Display::fmt(&self, f,)
  }
}
impl Display for Location {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    write!(f, "Ln {} Col {} Index {}", self.line, self.col, self.index)
  }
}

impl Location {
  /// Create a new [`Location`].
  pub fn new() -> Self {
    Location { line:1, col:1, index:1, }
  }

  pub fn next(&mut self, newline:bool,) {
    match newline {
      true => {
        self.line += 1;
        self.col = 1;
        self.index += 1;
      }
      false => {
        self.col += 1;
        self.index += 1;
      }
    }
  }
}

// pub struct Span {
//   start:Location,
//   end:Location,
// }

#[non_exhaustive]
#[derive(Debug, Eq, Clone, Copy)]
#[rustfmt::skip]
#[allow(non_camel_case_types)]
pub(super) enum TokenKind {
  // Single-character tokens
  LEFT_PAREN, RIGHT_PAREN,
  LEFT_BRACE, RIGHT_BRACE,
  LEFT_BRACKET, RIGHT_BRACKET,
  COMMA, DOT, SEMICOLON,COLON, DOUBLE_COLON,
  // Math
  MINUS, PLUS, SLASH, STAR,
  // Assignment
  EQUAL, MINUS_EQUAL, PLUS_EQUAL, SLASH_EQUAL, STAR_EQUAL,
  // Equality
  EQUAL_EQUAL,
  NOT, NOT_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL, 
  // Literals
  IDENTIFIER(Symbol), STRING(Symbol), INT(Symbol), FLOAT(Symbol),
  TYPE(Symbol), BOOL(Symbol),
  // Range
  RANGE_RIGHT_IN, RANGE_RIGHT_EX,
  // Keywords
  AND, OR, 
  TRUE, FALSE,
  IN,
  IF, ELSE, ELSE_IF,
  FN, RETURN, PRINT, 
  LET, MUT, 
  // Loops
  FOR, WHILE, 
  BREAK, CONTINUE,
  // Terminators
  EOF,
}

impl TokenKind {
  ///Create a new token type.
  fn new(value:&str,) -> Self {
    // Check if it is an string and return early if it is
    if value.starts_with('"',) {
      // Check if the string terminates, return a ERROR if it does not
      let sym = Symbol::from(intern(value,),);
      return match value.ends_with('"',) {
        true => TokenKind::STRING(sym,),
        false => panic!("{}", &ParsingError::StringNotTerminated.to_string()),
      };
    }
    // Check if it is an int and return early if it is
    else if value.parse::<u32>().is_ok() {
      let sym = Symbol::from(intern(value,),);
      return TokenKind::INT(sym,);
    }
    // Check if it is an float and return early if it is
    else if is_float(&value,) {
      let sym = Symbol::from(intern(value,),);
      return TokenKind::FLOAT(sym,);
    }

    // If the token is a type it is a type declaration
    if value == "int"
      || value == "float"
      || value == "usize"
      || value == "char"
      || value == "string"
      || value == "bool"
      || value == "int[]"
      || value == "float[]"
      || value == "usize[]"
      || value == "string[]"
      || value == "bool[]"
    {
      let sym = Symbol::from(intern(value,),);
      return TokenKind::TYPE(sym,);
    }

    //If the token is "true" or "false" it is a boolean
    if value == "true" || value == "false" {
      let sym = Symbol::from(intern(value,),);
      return TokenKind::BOOL(sym,);
    }

    match value {
      // Single-character tokens
      "(" => TokenKind::LEFT_PAREN,
      ")" => TokenKind::RIGHT_PAREN,
      "{" => TokenKind::LEFT_BRACE,
      "}" => TokenKind::RIGHT_BRACE,
      "[" => TokenKind::LEFT_BRACKET,
      "]" => TokenKind::RIGHT_BRACKET,
      ";" => TokenKind::SEMICOLON,
      ":" => TokenKind::COLON,
      "::" => TokenKind::DOUBLE_COLON,
      "," => TokenKind::COMMA,
      "." => TokenKind::DOT,
      // Math
      "-" => TokenKind::MINUS,
      "+" => TokenKind::PLUS,
      "/" => TokenKind::SLASH,
      "*" => TokenKind::STAR,
      // Assignment
      "=" => TokenKind::EQUAL,
      "-=" => TokenKind::MINUS_EQUAL,
      "+=" => TokenKind::PLUS_EQUAL,
      "/=" => TokenKind::SLASH_EQUAL,
      "*=" => TokenKind::STAR_EQUAL,
      // Equality
      "==" => TokenKind::EQUAL_EQUAL,
      "!" => TokenKind::NOT,
      "!=" => TokenKind::NOT_EQUAL,
      ">" => TokenKind::GREATER,
      ">=" => TokenKind::GREATER_EQUAL,
      "<" => TokenKind::LESS,
      "<=" => TokenKind::LESS_EQUAL,
      // Range
      ".." => TokenKind::RANGE_RIGHT_EX,
      "..=" => TokenKind::RANGE_RIGHT_IN,
      // Keywords
      "and" => TokenKind::AND,
      "or" => TokenKind::OR,
      "true" => TokenKind::TRUE,
      "false" => TokenKind::FALSE,
      "in" => TokenKind::IN,
      "if" => TokenKind::IF,
      "else" => TokenKind::ELSE,
      "else if" => TokenKind::ELSE_IF,
      "fn" => TokenKind::FN,
      "return" => TokenKind::RETURN,
      "print" => TokenKind::PRINT,
      "let" => TokenKind::LET,
      "mut" => TokenKind::MUT,
      // Loops
      "for" => TokenKind::FOR,
      "while" => TokenKind::WHILE,
      "break" => TokenKind::BREAK,
      "continue" => TokenKind::CONTINUE,
      _ => TokenKind::IDENTIFIER(Symbol::from(intern(value,),),),
    }
  }
}

impl PartialEq for TokenKind {
  fn eq(&self, other:&Self,) -> bool {
    match (self, other,) {
      (Self::IDENTIFIER(_,), Self::IDENTIFIER(_,),) => true,
      (Self::STRING(_,), Self::STRING(_,),) => true,
      (Self::INT(_,), Self::INT(_,),) => true,
      (Self::FLOAT(_,), Self::FLOAT(_,),) => true,
      (Self::BOOL(_,), Self::BOOL(_,),) => true,
      _ => core::mem::discriminant(self,) == core::mem::discriminant(other,),
    }
  }
}

///Checks whether a [`String`] is a float.
fn is_float(val:&str,) -> bool {
  val.parse::<f32>().is_ok()
}

#[derive(Debug,)]
///Intermediary struct for storing the data needed to create a [`Token`].
pub struct Chunk {
  pub start:Location,
  val:String,
}

impl Chunk {
  ///Create a new [`Chunk`].
  pub fn new() -> Self {
    Chunk {
      start:Location::new(),
      val:String::new(),
    }
  }

  pub fn push(&mut self, ch:char,) {
    self.val.push(ch,);
  }

  pub fn len(&self,) -> usize {
    self.val.len()
  }

  ///Emit a [`Token`] and ready a new [`Chunk`].
  pub fn to_token(&mut self,) -> Token {
    let token = Token::new(Some(self.val.clone().as_str(),), self.start,);
    self.val = String::new();
    token
  }
}

#[derive(Debug, Clone, Copy,)]
pub(super) struct Token {
  pub kind:TokenKind,
  pub loc:Location,
}

impl Token {
  /// Create a [`Token`] from a [`String`].
  pub fn new(value:Option<&str,>, loc:Location,) -> Token {
    match value {
      Some(str,) => Token {
        kind:TokenKind::new(str,),
        loc,
      },
      None => Token { kind:TokenKind::EOF, loc, },
    }
  }

  ///Returns the left binding power of the [`Token`]. For an infix `Token`, it
  /// tells us how strongly the `Token` binds to the `Token` at its left.
  pub fn lbp(&self,) -> u32 {
    // Need to add Or and And?

    // pub const ASSIGNMENT: i32  = 1;
    // pub const CONDITIONAL: i32 = 2;
    // pub const SUM: i32         = 3;
    // pub const PRODUCT: i32     = 4;
    // pub const EXPONENT: i32    = 5;
    // pub const PREFIX: i32      = 6;
    // pub const POSTFIX: i32     = 7;
    // pub const CALL: i32        = 8;

    match self.kind {
      // Wrappers
      // TokenKind::LEFT_PAREN | TokenKind::RIGHT_PAREN => 0,
      // Assign expressions
      TokenKind::EQUAL | TokenKind::MINUS_EQUAL | TokenKind::PLUS_EQUAL | TokenKind::SLASH_EQUAL | TokenKind::STAR_EQUAL => 10,
      // Conditional expressions
      TokenKind::IF | TokenKind::ELSE => 20,
      // Math expressions
      TokenKind::MINUS | TokenKind::PLUS => 30,
      TokenKind::STAR | TokenKind::SLASH => 40,
      // Comparison expressions
      TokenKind::EQUAL_EQUAL | TokenKind::NOT_EQUAL | TokenKind::GREATER | TokenKind::GREATER_EQUAL | TokenKind::LESS | TokenKind::LESS_EQUAL => 60,
      TokenKind::RANGE_RIGHT_IN | TokenKind::RANGE_RIGHT_EX => 50,
      // Anything else should cause the expression parsing to terminate
      _ => 0,
    }
  }
}

#[cfg(test)]
mod test {
  use super::is_float;

  #[test]
  fn check_float() {
    assert_eq!(is_float("4..3",), false);
    assert_eq!(is_float("4.3",), true);
  }
}
