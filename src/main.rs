use parser::Parser;

mod ast;
mod errors;
mod interner;
mod parser;
mod reg_allocator;
mod symbol_table;
mod token;
mod tokenizer;

// TODO
// - Finish the Parser
// - Add a symbol table
// - Add register allocation
// - Add a way to compile the AST generated by the parser to bytecode
// - Remove this from the engine file.
// - Set up some way to switch out the file it's compiling and interact with the
//   compiler.

// Refactor:
// - Move the tests into their own module.

fn main() {
  let mut parser = Parser::new("fdfd",);
  let _ast = parser.parse();
}
