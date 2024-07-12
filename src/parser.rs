use super::{
  ast::{AbstractSyntaxTree, BinOpKind, Expression, ExpressionKind, Ident, Literal, LiteralKind, Local, LocalKind, Pat, PatKind, Statement, Ty},
  errors::ParsingError,
  interner::lookup,
  symbol_table::Symbol,
  token::{Location, Token, TokenKind},
  tokenizer::tokenize,
};
use crate::ast::{AssignOpKind, StatementKind, P};
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
    let tokens = tokenize(source,).into_iter().peekable();

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
}

//Actual Parsing rules
impl Parser {
  ///Returns a [`Statement`].
  fn parse_statement(&mut self,) -> Result<Statement,> {
    let statement = match self.peek().kind {
      TokenKind::LET => self.parse_let(),
      _ => self.parse_expression_statement(),
    };
    self.eat_token_expect(TokenKind::SEMICOLON, "Statements must end with a semicolon.",).unwrap();
    Ok(statement,)
  }

  ///Returns an [`Expression`].
  fn parse_expression(&mut self, rbp:u32,) -> Expression {
    // Get the left hand expression
    let mut left = self.parse_expression_null_detonation();

    // Keep eating tokens as long as the next token's lbp is greater than
    // the current token -- lower lbp will cause the loop to terminate
    while self.next_is_less_precedence(rbp,) {
      left = self.parse_expression_left_denotation(left,)
    }
    left
  }

  ///Parse an [`Expression`] as a [`Statement`].
  fn parse_expression_statement(&mut self,) -> Statement {
    Statement {
      id:0,
      loc:self.peek().loc,
      kind:StatementKind::Expression(self.parse_expression(0,),),
    }
  }

  ///Converts the current [`Token`] into an [`Option<Expression>`].
  ///
  /// If the `Token` can be parsed as a prefix, returns an [`Expression`].
  ///
  /// Otherwise returns `None`.
  fn parse_expression_null_detonation(&mut self,) -> Expression {
    // Should be a prefix
    match self.peek().kind {
      TokenKind::INT(_,) | TokenKind::FLOAT(_,) | TokenKind::BOOL(_,) => self.parse_literal(),
      TokenKind::IF => self.parse_if_expression(),
      TokenKind::IDENTIFIER(_,) => self.parse_ident_expression(),
      TokenKind::LEFT_PAREN => self.parse_tuple_expression(),
      _ => panic!("Should not be {:?}", self.peek().kind),
    }
  }

  ///Returns an [`Expression`] consisting of the currently parsed [`Token`]s
  /// and the next `Token`.
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
      _ => {
        let token = self.next();
        panic!("{:?}", token.kind)
      }
    }
  }

  ///Return an expression containing a [`Literal`].
  fn parse_literal(&mut self,) -> Expression {
    let token = self.next();
    let val = match token.kind {
      TokenKind::INT(idx,) => Literal {
        kind:LiteralKind::Integer,
        symbol:Symbol { idx, },
      },
      TokenKind::FLOAT(idx,) => Literal {
        kind:LiteralKind::Float,
        symbol:Symbol { idx, },
      },
      TokenKind::BOOL(idx,) => Literal {
        kind:LiteralKind::Bool,
        symbol:Symbol { idx, },
      },
      _ => unreachable!(),
    };

    Expression {
      id:0,
      kind:ExpressionKind::Literal(P::new(val,),),
      loc:token.loc,
    }
  }

  fn parse_ident_expression(&mut self,) -> Expression {
    let token = self.next();
    if let TokenKind::IDENTIFIER(idx,) = token.kind {
      Expression {
        id:0,
        kind:ExpressionKind::Ident(Symbol { idx, },),
        loc:token.loc,
      }
    }
    else {
      unreachable!()
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

  /// Parses a block (`{Vec<Statemnent>}`).
  fn parse_block(&mut self,) -> Vec<Statement,> {
    self
      .eat_token_expect(TokenKind::LEFT_BRACE, "Must follow if statement condition with a brace `{`",)
      .unwrap();

    let mut block = Vec::new();

    while self.peek().kind != TokenKind::RIGHT_BRACE {
      block.push(self.parse_statement().unwrap(),);
    }

    self.eat_token_expect(TokenKind::RIGHT_BRACE, "Blocks must end with a right brace",).unwrap();
    block
  }

  ///Parse a `let` [`Statement`] (`let <pat>::<ty> = <expr>`).
  fn parse_let(&mut self,) -> Statement {
    let token = self.next();
    //Location of the first element in the statement
    let id = 0;
    let loc = token.loc;

    //Check for mutability
    let mutable = self.parse_mutability();

    //Check for an ident and error if none
    let ident = self.parse_ident();

    //Check for type
    let ty = self.parse_type();

    //The pattern cannot be found until the full expression is parsed
    let pat = Pat {
      id,
      loc,
      kind:PatKind::Ident { mutable, ident, },
    };

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

  ///Checks if the next [`Token`] is mutable and consumes it if so.
  fn parse_mutability(&mut self,) -> bool {
    self.eat_token_if_match(TokenKind::MUT,)
  }

  ///Checks if the next [`Token`]s are `:<ty>` and consumes them if so.
  fn parse_type(&mut self,) -> Option<P<Ty,>,> {
    //Check for a colon if a colon is found continue otherwise return false
    if self.eat_token_if_match(TokenKind::COLON,) {
      //Check for a type if no type is found print an error
      self.eat_token_expect(TokenKind::TYPE(0,), "Expected type declaration",).unwrap();

      let token = self.next();

      //If the type is not a valid type, error
      if let TokenKind::TYPE(idx,) = token.kind {
        let val = lookup(idx,);
        match Ty::new(val.clone(),) {
          Some(ty,) => return Some(P::new(ty,),),
          None => panic!("{} is not an accepted type", val),
        };
      }
    }
    None
  }

  fn parse_ident(&mut self,) -> Ident {
    // Eat the token expecting an ident
    let token = self
      .eat_token_expect(TokenKind::IDENTIFIER(0,), &ParsingError::VarNotDeclared.to_string(),)
      .unwrap();
    if let TokenKind::IDENTIFIER(idx,) = token.kind {
      Ident { name:idx, loc:token.loc, }
    }
    else {
      unreachable!()
    }
  }

  fn parse_local_kind(&mut self,) -> LocalKind {
    // If equals sign, parse the output of the next statement, expect an Expression.
    if self.eat_token_if_match(TokenKind::EQUAL,) {
      let expression = self.parse_expression(0,);
      return LocalKind::Init(P::new(expression,),);
    }

    LocalKind::Decl
  }

  fn parse_array_expression(&mut self,) -> Expression {
    todo!()
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
}

//Error reporting
impl Parser {
  ///Print an error message indicating where in the source file the error
  /// occurred.
  fn err_detected(&mut self, loc:Location, err:&ParsingError,) {
    //Surpress errors if the parser is in an error state because they will likely
    // be unhelpful
    if !self.erroring {
      self.had_err = true;
      //Print the error
      Parser::print_error(loc, &self.source, err,);

      //Enter panic mode and keep consuming tokens until a synchronization point is
      // reached
      self.erroring = true;
    }
  }

  ///Reads through the whole source code until it locates the target line.
  /// Print a string pointing to where in the line the error occured.
  fn print_error(loc:Location, raw:&String, err:&ParsingError,) {
    let mut line = 0;
    let mut line_string = String::new();

    // Find the whole line of original source
    for char in raw.chars() {
      if char == '\n' {
        line += 1;

        // If a linebreak was reached and the line is not empty than we have finished
        // searching the line.
        if !line_string.is_empty() {
          break;
        }
        continue;
      }

      if loc.line == line {
        line_string.push(char,);
      }
    }

    //Create the indicator to the error
    let indicator = "_".repeat(loc.col as usize,);
    print!(
      "{}\n\n{}\n{}^ Panicked near Ln:{} Col:{} \n",
      err.to_string(),
      line_string,
      indicator,
      loc.line,
      loc.col
    );
  }
}

#[cfg(test)]
mod tests {
  use super::Parser;
  use crate::ast::{BinOpKind, ExpressionKind, LiteralKind, LocalKind, StatementKind};

  #[test]
  fn parse_works() {
    // C:\Users\Jamari\.cargo\bin\cargo.exe 'test', '--package', 'galaxy-compiler',
    // '--bin', 'galaxy-compiler', '--', 'parser::tests::parse_works', '--exact',
    // '--show-output' '--nocapture'

    // Add tuples
    // Add loops
    // Add function calls
    // Add returns to if statements and loops
    // Add arrays
    // - Unclear if I want to do this in the compiler or in the std or something
    // Add back the error checking for the AST
    // - Eat token expect is not working
    // - Correct statement erroring so expression-statements do not require
    //   semicolons
    // - Erroring if tuples are not closed

    // Switch how equal is handled here?

    // https://craftinginterpreters.com/global-variables.html#assignment
    // To fix this, variable() should look for and consume the = only if it’s in the
    // context of a low-precedence expression. The code that knows the current
    // precedence is, logically enough, parsePrecedence(). The variable() function
    // doesn’t need to know the actual level. It just cares that the precedence is
    // low enough to allow assignment, so we pass that fact in as a Boolean.

    let source = r#"
    //Check the binops work with nums
    // let mut value_test = 5 + 6 * 7 / 8;

    //Check the binops work with boolean statements
    // let value = true != false == true;
    
    //Check if expressions work
    // let test = if true != false {
    //   let a = 90;
    // }
    // else {
    //   let a = 50;
    // };
    
    // Check assignment works
    // value_test = 5;

    // value_test += 5;

    // Does not parse if the numbers are connected to commas
    // Causes a stack overlow - run with nocapture
    let value_2 = (2 + 19,6,1);
  "#;

    let mut parser = Parser::new(source,);
    let ast = parser.parse();

    //Check the AST is correct
    // match ast.statements.clone()[0].kind.clone() {
    //   //Check the let statement
    //   StatementKind::Let(local,) => {
    //     match &local.kind {
    //       LocalKind::Init(expr,) => match &expr.kind {
    //         ExpressionKind::BinOp(lhs, op, rhs,) => {
    //           //Check the value of the lhs
    //           match &lhs.kind {
    //             ExpressionKind::Literal(lit,) => {
    //               assert_eq!(&lit.kind, &LiteralKind::Integer)
    //             }
    //             _ => panic!(),
    //           }

    //           //Check the value of the rhs
    //           match &rhs.kind {
    //             ExpressionKind::Literal(lit,) => {
    //               assert_eq!(&lit.kind, &LiteralKind::Integer)
    //             }
    //             _ => panic!(),
    //           }

    //           //Check the value of the op
    //           assert_eq!(*op, BinOpKind::PLUS);
    //         }
    //         _ => panic!(),
    //       },
    //       _ => panic!(),
    //     };
    //   }
    //   _ => panic!(),
    // }

    // dbg!(ast.statements.clone()[0].kind.clone());
  }
}
