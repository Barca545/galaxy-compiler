use super::{
  ast::{AbstractSyntaxTree, BinOpKind, Expression, ExpressionKind, Ident, Literal, LiteralKind, Local, LocalKind, Pat, PatKind, Statement, Ty},
  errors::ParsingError,
  interner::lookup,
  symbol_table::Symbol,
  token::{Location, Token, TokenKind},
  tokenizer::tokenize,
};
use crate::ast::{StatementKind, P};
use eyre::Result;
use std::{iter::Peekable, vec::IntoIter};

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
    dbg!(self.peek().kind);
    match self.peek().kind {
      TokenKind::LET => Ok(self.parse_let(),),
      _ => Ok(self.parse_expression_statement(),),
      // _ => panic!(),
      // Err(ParsingError::NoStatementMatch.into(),)
    }
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

  ///Converts the current [`Token`] into an [`Option<Expression>`].
  ///
  /// If the `Token` can be parsed as a prefix, returns an [`Expression`].
  ///
  /// Otherwise returns `None`.
  fn parse_expression_null_detonation(&mut self,) -> Expression {
    // Should be some kind of literal
    let token = self.peek();

    match token.kind {
      TokenKind::INT(_,) => self.parse_integer(),
      TokenKind::FLOAT(_,) => self.parse_float(),
      TokenKind::BOOL(_,) => self.parse_bool(),
      TokenKind::IDENTIFIER(idx,) => panic!("Have not set up handling identifiers here yet"),
      // If expression
      _ => panic!("Should not be {:?}", token.kind),
      // //If statement
      // TokenKind::IF => {
      //   dbg!("reached 5");
      //   self.parse_if_expression(&token,)
      // }

      // This should probably enter an error state
      // I think it should error that it was expecting some kind of literal
      // _ => {
      //   // self.err_detected(token.loc, &ParsingError::UnexpectedToken(token.kind,),);
      //   if let TokenKind::IDENTIFIER(idx,) = token.kind {
      //     panic!("{} is not an accepted identifier", lookup(idx));
      //   }
      //   panic!();
      // }
    }
  }

  ///Returns an [`Expression`] consisting of the currently parsed [`Token`]s
  /// and the next `Token`.
  fn parse_expression_left_denotation(&mut self, left:Expression,) -> Expression {
    match self.peek().kind {
      //BinOps
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
        let loc = left.loc;
        let token = self.next();
        let binop_kind = BinOpKind::from(token,);
        let right = self.parse_expression(token.lbp(),);
        let kind = ExpressionKind::BinOp(P::new(left,), binop_kind, P::new(right,),);
        Expression { id:0, kind, loc, }
      }
      _ => {
        let token = self.next();
        panic!("{:?}", token.kind)
      }
    }
  }

  ///Return an expression containing an integer [`Literal`].
  fn parse_integer(&mut self,) -> Expression {
    let token = self.next();
    if let TokenKind::INT(idx,) = token.kind {
      let val = Literal {
        kind:LiteralKind::Integer,
        symbol:Symbol { idx, },
      };

      Expression {
        id:0,
        kind:ExpressionKind::Literal(P::new(val,),),
        loc:token.loc,
      }
    }
    else {
      unreachable!()
    }
  }

  ///Return an expression containing a float [`Literal`].
  fn parse_float(&mut self,) -> Expression {
    let token = self.next();
    if let TokenKind::FLOAT(idx,) = token.kind {
      let val = Literal {
        kind:LiteralKind::Float,
        symbol:Symbol { idx, },
      };

      Expression {
        id:0,
        kind:ExpressionKind::Literal(P::new(val,),),
        loc:token.loc,
      }
    }
    else {
      unreachable!()
    }
  }

  ///Return an expression containing a boolean [`Literal`].
  fn parse_bool(&mut self,) -> Expression {
    let token = self.next();
    if let TokenKind::BOOL(idx,) = token.kind {
      let val = Literal {
        kind:LiteralKind::Bool,
        symbol:Symbol { idx, },
      };

      Expression {
        id:0,
        kind:ExpressionKind::Literal(P::new(val,),),
        loc:token.loc,
      }
    }
    else {
      unreachable!()
    }
  }

  fn parse_if_expression(&mut self, token:&Token,) -> Expression {
    dbg!("reached here 1");
    // Parse the conditional expression
    let condition = self.parse_expression(0,);
    let loc = token.loc;

    dbg!("reached here 2");

    // Parse the statement making up its body
    self
      .eat_token_expect(TokenKind::LEFT_BRACE, "Must follow if statement with a brace `{`",)
      .unwrap();

    dbg!("reached here 3");

    let mut if_body = Vec::new();

    while self.peek().kind != TokenKind::RIGHT_BRACE {
      if_body.push(self.parse_statement().unwrap(),);
    }

    // Check for an else statment and parse it if so
    let else_body = if self.eat_token_if_match(TokenKind::ELSE,) {
      let else_expr = self.parse_expression(0,);
      Some(P::new(else_expr,),)
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

  ///Parse an [`Expression`] as a [`Statement`].
  fn parse_expression_statement(&mut self,) -> Statement {
    let token = self.next();
    let loc = token.loc;
    let expression = self.parse_expression(0,);
    dbg!(expression.clone());

    Statement {
      id:0,
      loc,
      kind:StatementKind::Expression(expression,),
    }
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
      panic!("This should not be reachable!")
    }
  }

  fn parse_local_kind(&mut self,) -> LocalKind {
    // If equals sign, parse the output of the next statement, expect an Expression.
    if self.eat_token_if_match(TokenKind::EQUAL,) {
      let expression = self.parse_expression(0,);
      //Line must end after the expression so expect a semicolon
      self.eat_token_expect(TokenKind::SEMICOLON, "Must end Let statement with a semicolon",).unwrap();
      return LocalKind::Init(P::new(expression,),);
    }
    // If equals no sign error it's a delayed assignment so expect a semicolon
    self.eat_token_expect(TokenKind::SEMICOLON, "Must end Let statement with a semicolon",).unwrap();

    LocalKind::Decl
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
    // Precedence seems to be wrong as it spits out ((5+6)*7) not (5+(6*7))
    let source = r#"
    //Check the binops work with nums
    // let mut value_test = 5 + 6 * 7 / 8;

    //Check the binops work with boolean statements
    // let value = true != false == true;
    
    //Check if expressions work
    // if true {
    //   // I think it wants an identifier here, not an expression
    //   // Need to revisit how the null detonation works 
    //   let a = 90;
    // }
  "#;

    // I think I need to define what a block is so it knows the if expresion
    // terminates when it encounters brackets

    // doublecheck if that is how rust does it

    // erroring in the loop a few problems
    // -Seems to be expecting the inside of an if expression to be an expression and
    // not a vec of statements  -> it tries to go into the rest of the
    // expression parsing so it expects let to be an expression because it's
    // treating it life the lefthand side
    // - Still have a precedence problem with the if's inner

    //Look into what the null detonation is supposed to be, because currently the
    // if has to be there instead of the left

    //might also just never be reaching the if branch for some reason
    //seems like it thinks the { begins a new statement

    //it skips past the if because of the next in the parse expressions function
    // the problem is for *just* parsing expressions it needs the next()
    // but for parsing  expression statements it can't have the next

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

    dbg!(ast.statements.clone()[0].kind.clone());
  }
}
