mod ast;
mod errors;
mod interner;
mod parser;
mod reg_allocator;
mod symbol_table;
mod tests;
mod token;
mod tokenizer;

use parser::Parser;

// TODO
// - Remove this from the engine file.
// - Finish the Parser
// - Add a way to compile the AST generated by the parser to bytecode
// - Add a symbol table
// - Add type checking
// - Add register allocation
// - Add linking?
// - Add memory allocator to the VM: https://discord.com/channels/816484635273199626/974884085606518824/1273574064266678315
// - Set up some way to switch out the file it's compiling and interact with the
//   compiler.

// Refactor:
// - Move the tests into their own module.
// - let const instead of let mut? -> no mut keyword for declarations only for
//   references?
// - Remove range pattern?
// - Only one number type which can be either an int or a float

// Lang Features
// - Type checking
// - Default args
// - Custom/semantic types?
// - Type inference
// - Importing?
// - Documenting comments that generate HTML?
// - Only dynamic arrays (vectors) defined in std?

fn main() {
  let mut parser = Parser::new("fdfd",);
  let _ast = parser.parse();
}
