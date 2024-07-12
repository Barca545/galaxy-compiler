use super::token::{Chunk, Token};
use crate::{interner::intern, token::TokenKind};
use std::{iter::Peekable, vec::IntoIter};

// Refactor:
// - Add catching types to the filter pass and move it out of the token
//   formation. This will make it easier to add types later if desired.
// - I think I need to rework how chunks work/remove them from the tokenizer.
// - Create a read_string function that just reads a full string and then
//   interns it before returning that for the string literals to use
// - Fix errors to pretty print
// - Key might need to be "Symbol"
// - Number tokenization really needs to be redone

pub type TokenStream = Vec<Token,>;

pub fn tokenize(source:&str,) -> TokenStream {
  let mut tokens = Vec::new();
  let mut chunk = Chunk::new();
  let mut is_comment = false;

  // let mut chars =
  // source.chars().collect::<Vec<char,>>().into_iter().peekable();

  // while chars.peek().is_some() {
  //   let ch = chars.next().unwrap();

  //   // This block identifies a comment has begun
  //   if ch == '/' && *chars.peek().unwrap() == '/' {
  //     eat_comment(&mut chars,);
  //     continue;
  //   }

  //   match ch {
  //     '\"' => {
  //       let str = read_string(&mut chars,);
  //       for ch in str.chars() {
  //         chunk.push(ch,)
  //       }
  //       tokens.push(chunk.to_token(),)
  //     }
  //     ' ' | '\r' | '\t' => {
  //       if chunk.len() > 0 {
  //         tokens.push(chunk.to_token(),)
  //       }
  //     }
  //     '\n' => {
  //       if chunk.len() > 0 {
  //         tokens.push(chunk.to_token(),);
  //       }
  //       chunk.newline = true;
  //     }
  //     ';' | '(' | ')' | '{' | '}' | '[' | ']' => {
  //       if chunk.len() > 0 {
  //         tokens.push(chunk.to_token(),);
  //       }
  //       tokens.push(chunk.new_token(ch.to_string(),),);
  //     }
  //     '1'..='9' => {
  //       let mut num = String::from(ch,);
  //       loop {
  //         let c = chars.peek().unwrap();
  //         let t = format!("{num}{c}");
  //         if t.parse::<f32>().is_ok() || t.parse::<u32>().is_ok() {
  //           num.push(chars.next().unwrap(),);
  //         }
  //         else {
  //           break;
  //         }
  //       }
  //     }
  //     _ => chunk.push(ch,),
  //   }

  //   chunk.next()
  // }

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
          if chunk.is_string {
            chunk.push('\"',);
            tokens.push(chunk.to_token(),)
          }
          else {
            chunk.push('\"',)
          }
          chunk.is_string = !chunk.is_string;
        }
        ' ' | '\r' | '\t' => {
          if chunk.is_string {
            chunk.push(ch,);
          }
          else if chunk.len() > 0 {
            tokens.push(chunk.to_token(),)
          }
        }
        '\n' => {
          if chunk.is_string {
            chunk.push(ch,);
          }
          else if chunk.len() > 0 {
            tokens.push(chunk.to_token(),);
          }
          chunk.newline = true;
        }
        ',' | ';' | '(' | ')' | '{' | '}' | '[' | ']' => {
          if chunk.len() > 0 {
            tokens.push(chunk.to_token(),);
          }
          tokens.push(chunk.new_token(ch.to_string(),),);
        }
        _ => chunk.push(ch,),
      }
    }
    chunk.next()
  }
  //Add the EOF token
  tokens.push(Token::new(None, chunk.loc(),),);

  tokens
}

/// Reads a string from the source code, interns it and returns its key.
fn read_string(chars:&mut Peekable<IntoIter<char,>,>,) -> String {
  let mut str = String::from("\"",);

  // Push chars to the string
  while chars.peek().is_some() {
    let ch = chars.next().unwrap();
    if ch != '\"' {
      str.push(ch,)
    }
    else {
      str.push(ch,);
      break;
    }
  }

  // Confirm the string ends with a \"
  if str.ends_with(|ch| ch != '\"',) {
    panic!("Tokenizing Error: Must terminate string with \".")
  }

  str
}

/// Eats a comment from the source code.
fn eat_comment(chars:&mut Peekable<IntoIter<char,>,>,) {
  // Push chars to the string
  while chars.peek().is_some() {
    let ch = chars.next().unwrap();
    if ch == '\n' {
      break;
    }
  }
}

#[cfg(test)]
mod test {
  use super::tokenize;
  use crate::{interner::lookup, token::TokenKind};

  #[test]
  fn tokenize_number() {
    let source = "223.5+9*=15.5";
    let mut chars = source.chars().enumerate().collect::<Vec<(usize, char,),>>().into_iter().peekable();

    let mut tokens = Vec::new();
    while chars.peek().is_some() {
      let ch = chars.next().unwrap().1;
      // this is the actual code

      match ch {
        '0'..='9' => {
          let mut num = String::from(ch,);
          while chars.peek().is_some() && (num.clone() + &chars.peek().unwrap().1.to_string()).parse::<f32>().is_ok() {
            num += &chars.next().unwrap().1.to_string();
          }
          tokens.push(num,)
        }
        '*' | '/' | '+' | '-' => {
          let mut tok = String::from(ch,);
          if chars.peek().unwrap().1 == '=' {
            tok.push(chars.next().unwrap().1,);
          }
          tokens.push(tok,);
        }
        _ => unreachable!(),
      }
      // end of actual code

      // break;
    }
    dbg!(tokens[0].clone());
    dbg!(tokens[1].clone());
    dbg!(tokens[2].clone());
    dbg!(tokens[3].clone());
    dbg!(tokens[4].clone());
    // Confirm it splits into 2.4, PLUS, 3.5
  }

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
