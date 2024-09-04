use super::Parser;
use crate::{errors::ParsingError, token::Location};

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
