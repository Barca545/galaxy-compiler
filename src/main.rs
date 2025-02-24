mod ast;
mod errors;
mod interner;
mod ir;
mod name_resolution;
mod parser;
mod reg_allocator;
mod symbol_table;
mod token;
mod tokenizer;
mod type_checking;
mod visitor;

use parser::Parser;

// Switch how equal signs are handled?
// https://craftinginterpreters.com/global-variables.html#assignment
// To fix this, variable() should look for and consume the = only if it’s in
// the context of a low-precedence expression. The code that knows the
// current precedence is, logically enough, parsePrecedence(). The
// variable() function doesn’t need to know the actual level. It just
// cares that the precedence is low enough to allow assignment, so we
// pass that fact in as a Boolean.

// TODO
// - Remove this from the engine file.
// - Finish the Parser
// - Add a way to compile the AST generated by the parser to bytecode
// - Add a symbol table
// - Add type checking
// - Check return type matches the signature
// - Add register allocation
// - Add linking?
// - Add memory allocator to the VM: https://discord.com/channels/816484635273199626/974884085606518824/1273574064266678315
// - Set up some way to switch out the file it's compiling and interact with the
//   compiler.
// - Add back the error checking for the Parser
//    - Eat token expect is not working because it has a hardcoded error message
//      and does not print the input error
//    - Erroring if tuples are not closed
// - Track never used stuff for optimizing out

// Refactor:
// - Break the ast module into an Expression and statement module as well.
// - let const instead of let mut? -> no mut keyword for declarations only for
//   references?
// - Remove range pattern?
// - Only one number type which can be either an int or a float
// - Add some sort of type table?
// - Blocks should only require semicolons if they are also part of a
//   declaration
// - Special error reporting for returns in the wrong place
// - Should not require blocks to have semicolons after them
//    - Correct statement erroring so expression-statements do not require
//      semicolons
// - I don't love the array types being built in? There's no real problem with
//   it per se but I want to revisit it before finalizing
// - Do I need "Item"s or can it just be a function? What other Items might I
//   need in addition to functions?
// - weird hacky thing where tokens can be checked for equality but token
//   *kinds* are not checked for the equality of their interiors
// - If an expression fails to end with a semicolon, say that as an error.
// - All errors should bubble up not be unwrapped where they occur. I want them
//   to store information about the code and try to recover so everything can be
//   reported at once.

// Lang Features
// - Type checking
// - No custom types.
// - Default args -> This could work have functions either accept an `name:type`
//   or `name:value` where it then registers the type of the value as the type
//   of `name`
// - Type inference
// - No importing outside of from the header file.
// - Documenting comments that generate HTML -> I don't think there is any need
//   for documentation because there are no structures and the code does not get
//   that complex. Comments should suffice. If I am wrong I will revisit this.
//   The database for the scripts themselves might want the whole script
//   documented tho.
// - Only dynamic arrays (vectors) defined in std?
//    - I kind of do not want to have an std because I want to avoid imports.
// - Only pass by value
// - Delayed variable declarations -> built into the parser already
// - I kind of want pattern matching. I am unsure in what capacity. If it is too
//   complex might have to settle for if/then statements

fn main() {
  let mut parser = Parser::new("fdfd",);
  let _ast = parser.parse();
}
