use super::token::{Chunk, Token};
// TODO:
// - Update so it takes a &str. Will probably require changes to the tokens as
//   well.

// Refactor:
// - Add catching types to the filter pass and move it out of the token
//   formation. This will make it easier to add types later if desired.

pub type TokenStream = Vec<Token,>;

pub fn tokenize(source:&str,) -> TokenStream {
  let mut tokens = Vec::new();
  let mut chunk = Chunk::new();
  let mut is_string = false;
  let mut is_comment = false;

  let chars = source.chars().collect::<Vec<char,>>();

  for i in 0..chars.len() {
    let ch = chars[i];

    // The following 2 blocks control reading comments

    // This block identifies a comment has begun
    if ch == '/' && chars[i + 1] == '/' {
      is_comment = true;
    }

    // This block identifies a comment has ended
    if is_comment && ch == '\n' {
      is_comment = false;
    }

    if !is_comment {
      match ch {
        '\"' => {
          if is_string {
            chunk.push('\"',);
            tokens.push(chunk.to_token(),)
          }
          else {
            chunk.push('\"',)
          }
          is_string = !is_string;
        }
        ' ' => {
          if is_string {
            chunk.push(' ',);
          }
          else if chunk.len() > 0 {
            tokens.push(chunk.to_token(),)
          }
        }
        '\r' => {
          if is_string {
            chunk.push('\r',);
          }
          else if chunk.len() > 0 {
            tokens.push(chunk.to_token(),)
          }
        }
        '\t' => {
          if is_string {
            chunk.push('\t',);
          }
          else if chunk.len() > 0 {
            tokens.push(chunk.to_token(),)
          }
        }
        '\n' => {
          chunk.newline = true;
          if is_string {
            chunk.push('\n',);
          }
          else if chunk.len() > 0 {
            tokens.push(chunk.to_token(),);
          }
        }
        ';' => {
          if chunk.len() > 0 {
            tokens.push(chunk.to_token(),);
          }
          tokens.push(chunk.new_token(";",),);
        }
        '(' => {
          if chunk.len() > 0 {
            tokens.push(chunk.to_token(),);
          }
          tokens.push(chunk.new_token("(",),);
        }
        ')' => {
          if chunk.len() > 0 {
            tokens.push(chunk.to_token(),);
          }
          tokens.push(chunk.new_token(")",),);
        }
        '{' => {
          if chunk.len() > 0 {
            tokens.push(chunk.to_token(),);
          }
          tokens.push(chunk.new_token("{",),);
        }
        '}' => {
          if chunk.len() > 0 {
            tokens.push(chunk.to_token(),);
          }
          tokens.push(chunk.new_token("}",),);
        }
        '[' => {
          if chunk.len() > 0 {
            tokens.push(chunk.to_token(),);
          }
          tokens.push(chunk.new_token("[",),);
        }
        ']' => {
          if chunk.len() > 0 {
            tokens.push(chunk.to_token(),);
          }
          tokens.push(chunk.new_token("]",),);
        }
        _ => chunk.push(ch,),
      }
    }
    chunk.next()
  }
  //Add the EOF token
  tokens.push(Token::new(None, chunk.loc(),),);

  // filter(&mut tokens,);
  tokens
}

#[cfg(test)]
mod test {
  use crate::{interner::lookup, token::TokenKind};

  use super::tokenize;

  #[test]
  fn tokens_are_generated() {
    let source = r#"//Confirming the comments are ignored

    ()  [] {}
    let mut a = "test string"
    //Confirm it catches number literals
    let b = 4.5
    b = 5.0
    "#;
    let tokens = tokenize(source,);

    // Check the token kinds
    assert_eq!(tokens[0].kind, TokenKind::LEFT_PAREN);
    assert_eq!(tokens[1].kind, TokenKind::RIGHT_PAREN);
    assert_eq!(tokens[2].kind, TokenKind::LEFT_BRACKET);
    assert_eq!(tokens[3].kind, TokenKind::RIGHT_BRACKET);
    assert_eq!(tokens[4].kind, TokenKind::LEFT_BRACE);
    assert_eq!(tokens[5].kind, TokenKind::RIGHT_BRACE);
    assert_eq!(tokens[6].kind, TokenKind::LET);
    assert_eq!(tokens[7].kind, TokenKind::MUT);
    if let TokenKind::IDENTIFIER(idx,) = tokens[8].kind {
      let str = lookup(idx,);
      assert_eq!("a", str)
    }
    else {
      panic!("{:?}", tokens[8].kind)
    };
    assert_eq!(tokens[9].kind, TokenKind::EQUAL);
    if let TokenKind::STRING(idx,) = tokens[10].kind {
      let str = lookup(idx,);
      assert_eq!("\"test string\"", str)
    }
    else {
      panic!("{:?}", tokens[10].kind)
    };
    assert_eq!(tokens[11].kind, TokenKind::LET);
    if let TokenKind::IDENTIFIER(idx,) = tokens[12].kind {
      let str = lookup(idx,);
      assert_eq!("b", str)
    }
    else {
      panic!("{:?}", tokens[12].kind)
    };
    assert_eq!(tokens[13].kind, TokenKind::EQUAL);
    if let TokenKind::FLOAT(idx,) = tokens[14].kind {
      let str = lookup(idx,);
      assert_eq!("4.5", str)
    }
    else {
      panic!("{:?}", tokens[14].kind)
    };
    if let TokenKind::IDENTIFIER(idx,) = tokens[15].kind {
      let str = lookup(idx,);
      assert_eq!("b", str)
    }
    else {
      panic!("{:?}", tokens[15].kind)
    };

    //Check the locations
    assert_eq!(tokens[0].loc.line, 3);
    assert_eq!(tokens[0].loc.col, 1 + 4);
    assert_eq!(tokens[1].loc.line, 3);
    assert_eq!(tokens[1].loc.col, 2 + 4);
    assert_eq!(tokens[2].loc.line, 3);
    assert_eq!(tokens[2].loc.col, 5 + 4);
    assert_eq!(tokens[3].loc.line, 3);
    assert_eq!(tokens[3].loc.col, 6 + 4);
    assert_eq!(tokens[4].loc.line, 3);
    assert_eq!(tokens[4].loc.col, 8 + 4);
    assert_eq!(tokens[5].loc.line, 3);
    assert_eq!(tokens[5].loc.col, 9 + 4);
    assert_eq!(tokens[6].loc.line, 4);
    assert_eq!(tokens[6].loc.col, 1 + 4);
    assert_eq!(tokens[7].loc.line, 4);
    assert_eq!(tokens[7].loc.col, 5 + 4);
    assert_eq!(tokens[8].loc.line, 4);
    assert_eq!(tokens[8].loc.col, 9 + 4);
    assert_eq!(tokens[9].loc.line, 4);
    assert_eq!(tokens[9].loc.col, 11 + 4);
    assert_eq!(tokens[10].loc.line, 4);
    assert_eq!(tokens[10].loc.col, 12 + 4);
    assert_eq!(tokens[11].loc.line, 6);
    assert_eq!(tokens[11].loc.col, 1 + 4);
    assert_eq!(tokens[12].loc.line, 6);
    assert_eq!(tokens[12].loc.col, 5 + 4);
    assert_eq!(tokens[13].loc.line, 6);
    assert_eq!(tokens[13].loc.col, 7 + 4);
    assert_eq!(tokens[14].loc.line, 6);
    assert_eq!(tokens[14].loc.col, 9 + 4);
    assert_eq!(tokens[15].loc.line, 7);
    assert_eq!(tokens[15].loc.col, 1 + 4);
  }
}
