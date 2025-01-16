use crate::{interner::lookup, symbol_table::Symbol};

use super::token::TokenKind;
use thiserror::Error;

// TODO:
// - Separate lexing and parsing errors

#[derive(Debug, Clone, Error, PartialEq,)]
pub enum ParsingError {
  #[error("{0:?} is not recognized as a token.")]
  ChunkNotRecognized(String,),
  #[error("Strings must be closed with \"\" ")]
  StringNotTerminated,
  #[error("{0:?} is not an integer or a float.")]
  NotValidNumber(String,),
  #[error("Expected variable declaration.")]
  VarNotDeclared,
  #[error("{0:?} is not a valid variable name. Variable names must begin with a letter")]
  InvalidVarName(String,),
  #[error("Unexpected token. {recieved:?} is not {expected:?}")]
  UnexpectedToken { expected:TokenKind, recieved:TokenKind, },
  #[error("{0:?} is not a valid type. Accepted types are int, float, usize, and str or their corresponding arrays int[], float[], usize[], and str[]")]
  InvalidType(String,),
  #[error("No statement match")]
  NoStatementMatch,
}

#[derive(Debug, Error,)]
pub enum SymbolGenerationErrors {
  #[error("cannot find value `{}` in this scope.", lookup(symbol.idx))]
  UnrecognizedIdentifier { symbol:Symbol, },
}
