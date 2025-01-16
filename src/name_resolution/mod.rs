use crate::symbol_table::Symbol;
use std::ops::{Index, IndexMut};

// I have created the AST but no `VarDecl` or anything has been created.
// The next step seems to be going to an IR, Rust has two IRs. I only want the
// one. Rust breaks everything into a series of recursive functions (i.e. `main`
// holds other functions etc. I had not envisioned my language having a `main`
// function so I will call my top level structure a `Script`. I will first
// implement the ability to handle variables and then move onto handling
// functions after I got that working.)

// My current debate is how to build out the "symbol table" after some
// consideration I decided storing all variables in one vector and then having
// them indicate which scope they are in makes sense because then other things
// (e.g. 3 address code) can reference variables by their id (index) instead of
// having to locate them by finding their scope and then finding them in the
// symbol table for their scope. (This may be a bad call?)

// I am unclear when to generate the declaration information. Should it be as
// part of the AST generation whenever a variable is located? That makes the
// most sense to me but is not what compilers seem to do?

// So tldr the current process seems to be:
// - Parse tokens into the AST
// - Traverse the AST using a visitor pattern (what's that?) and turn each
//   `Local` declaration node into a `VarDecl` in the script.
// - Turn each expression in the AST into 3AC (indirect triples)
// - Optimization???
// - 3AC becomes bytecode
// - Profit
