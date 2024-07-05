use super::{errors::ParsingError, interner::intern};

// TODO:
// - Not sure I need the double colon operator
// - How do tokenizers actually handle type declarations
// - switch from passing in an interner to using the INTERNER directly
// - Remove the

// Refactor:
// - Debating getting rid of the difference between floats and ints and just
//   using floats only

#[derive(Debug, Clone, Copy,)]
pub(super) struct Location {
  pub(super) line:u32,
  pub(super) col:u32,
  pub(super) index:usize,
}

impl Location {
  /// Create a new [`Location`].
  pub fn new() -> Self {
    Location { line:1, col:1, index:0, }
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
  IDENTIFIER(u32), STRING(u32), INT(u32), FLOAT(u32),
  TYPE(u32), BOOL(u32),

  // Keywords
  AND, OR, 
  TRUE, FALSE,
  FOR, WHILE, LOOP,
  IF, ELSE, ELSE_IF,
  FN, RETURN, PRINT, 
  LET, MUT, 
  // Terminators
  EOF,
}

impl TokenKind {
  ///Create a new token type.
  fn new(value:&str,) -> Self {
    // Return early if the token kind is a string
    if value.starts_with('\"',) {
      // Check if the string terminates, return a ERROR if it does not
      let idx = intern(value,);
      return match value.chars().last().unwrap() == '"' {
        true => TokenKind::STRING(idx,),
        false => panic!("{}", &ParsingError::StringNotTerminated.to_string()),
      };
    }

    // Return early if the token kind is a number
    if value.chars().nth(0,).unwrap().is_numeric() {
      let idx = intern(value,);
      // Check if the token is an int or a float
      if is_float(&value,) {
        return TokenKind::FLOAT(idx,);
      }
      else if value.parse::<u32>().is_ok() {
        return TokenKind::INT(idx,);
      }
      else {
        panic!("{}", &ParsingError::NotValidNumber(value.to_string()).to_string())
      }
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
      let idx = intern(value,);
      return TokenKind::TYPE(idx,);
    }

    //If the token is "true" or "false" it is a boolean
    if value == "true" || value == "false" {
      let idx = intern(value,);
      return TokenKind::BOOL(idx,);
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
      // Keywords
      "and" => TokenKind::AND,
      "or" => TokenKind::OR,
      "true" => TokenKind::TRUE,
      "false" => TokenKind::FALSE,
      "for" => TokenKind::FOR,
      "while" => TokenKind::WHILE,
      "loop" => TokenKind::LOOP,
      "if" => TokenKind::IF,
      "else" => TokenKind::ELSE,
      "else if" => TokenKind::ELSE_IF,
      "fn" => TokenKind::FN,
      "return" => TokenKind::RETURN,
      "print" => TokenKind::PRINT,
      "let" => TokenKind::LET,
      "mut" => TokenKind::MUT,
      _ => TokenKind::IDENTIFIER(intern(value,),),
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

fn is_float(val:&str,) -> bool {
  // Check string only has one period and otherwise only contains numbers
  let mut period_found = false;
  for ch in val.chars() {
    if !ch.is_numeric() {
      if ch == '.' {
        if period_found {
          return false;
        }
        else {
          period_found = true;
        }
      }
      else {
        return false;
      }
    }
  }

  // If the first and last chars are numeric and it has not other elements other
  // than a period, it is a float
  val.chars().nth(0,).unwrap().is_numeric() && val.chars().last().unwrap().is_numeric() && period_found
}

#[derive(Debug,)]
///Intermediary struct for storing the data needed to create a [`Token`].
pub struct Chunk {
  loc:Location,
  val:String,
  pub newline:bool,
}

impl Chunk {
  ///Create a new [`Chunk`].
  pub fn new() -> Self {
    Chunk {
      loc:Location::new(),
      val:String::new(),
      newline:false,
    }
  }

  pub fn push(&mut self, ch:char,) {
    self.val.push(ch,);
    if ch == '\n' {
      self.newline = true
    }
    else {
      self.newline = false
    }
  }

  pub fn len(&self,) -> usize {
    self.val.len()
  }

  ///Emit a [`Token`] and ready a new [`Chunk`].
  pub fn to_token(&mut self,) -> Token {
    //Ensure the pointer is to the front of the token.
    let mut loc = self.loc;
    loc.col -= self.val.chars().count() as u32;
    loc.index -= self.val.chars().count();
    let token = Token::new(Some(self.val.clone().as_str(),), loc,);
    self.val = String::new();
    token
  }

  ///Create a new [`Token`] from a [`String`].
  pub fn new_token(&mut self, val:&str,) -> Token {
    //Ensure the pointer is to the front of the token.
    let mut loc = self.loc;
    loc.col -= self.val.chars().count() as u32;
    loc.index -= self.val.chars().count();
    let token = Token::new(Some(&String::from(val,),), loc,);
    token
  }

  ///Increment the [`Chunk`]'s [`Location`]
  pub fn next(&mut self,) {
    self.loc.next(self.newline,);
    self.newline = false;
  }

  pub fn loc(&self,) -> Location {
    self.loc
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

  pub fn precedence(&self,) -> u32 {
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
      // Assign expressions
      TokenKind::EQUAL | TokenKind::MINUS_EQUAL | TokenKind::PLUS_EQUAL | TokenKind::SLASH_EQUAL | TokenKind::STAR_EQUAL => 10,
      // Conditional expressions
      TokenKind::IF | TokenKind::ELSE => 20,
      // Math expressions
      TokenKind::STAR | TokenKind::SLASH => 30,
      TokenKind::MINUS | TokenKind::PLUS => 40,
      // Comparison expressions
      TokenKind::EQUAL_EQUAL | TokenKind::NOT_EQUAL | TokenKind::GREATER | TokenKind::GREATER_EQUAL | TokenKind::LESS | TokenKind::LESS_EQUAL => 60,
      // Anything else should cause the expression parsing to terminate
      _ => 0,
    }
  }
}
