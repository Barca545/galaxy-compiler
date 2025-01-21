use super::{
  symbol_table::Symbol,
  token::{Location, Token, TokenKind},
};
use crate::{interner::lookup, symbol_table::symbol_table::DefId};
use std::{
  fmt::{write, Debug, Display},
  ops::{Deref, DerefMut},
  slice::IterMut,
  vec::IntoIter,
};

// Refactor:
// - Add location info to the block
// - Rename `Pat` pattern?
// - Do the blocks needs idx (indices) and stuff?
// - Confirm I need the P around stuff with static sizes / need P at all
// - Does statement actually need to be Clone
// - Consider breaking in to smaller modules
// - Block's inner shouldn't be public

/// A Trait indicating the implementor can be used as the inside of
/// [`ExpressionKind::Ident`].

#[derive(Debug, Clone, PartialEq,)]
pub struct RawIdent {
  pub symbol:Symbol,
  pub loc:Location,
}

#[derive(Debug, Clone, PartialEq,)]
pub enum IdentInner {
  Raw(RawIdent,),
  DefId(DefId,),
}

impl IdentInner {
  pub fn symbol(&self,) -> Symbol {
    match self {
      IdentInner::Raw(raw_ident,) => raw_ident.symbol,
      IdentInner::DefId(def_id,) => panic!("DefId does not contain a Symbol"),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq,)]
///Owned smart pointer on the heap.
pub struct P<T,> {
  ptr:Box<T,>,
}

impl<T,> Deref for P<T,> {
  type Target = T;

  fn deref(&self,) -> &Self::Target {
    &*self.ptr
  }
}

impl<T,> DerefMut for P<T,> {
  fn deref_mut(&mut self,) -> &mut Self::Target {
    &mut *self.ptr
  }
}

impl<T,> P<T,> {
  pub fn new(val:T,) -> Self {
    P { ptr:Box::new(val,), }
  }
}

#[derive(Debug, Clone, PartialEq,)]
pub enum Literal {
  Bool(bool,),
  Integer(i32,),
  Float(f32,),
  Str(Symbol,),
}

impl Display for Literal {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    match self {
      Literal::Bool(b,) => write!(f, "BOOL({})", b),
      Literal::Integer(i,) => write!(f, "INT({})", i),
      Literal::Float(fl,) => write!(f, "FLOAT({})", fl),
      Literal::Str(symbol,) => write!(f, "STR({})", lookup(symbol.idx)),
    }
  }
}

#[allow(non_camel_case_types)]
#[derive(Clone, PartialEq, Eq,)]
pub enum BinOpKind {
  MINUS,
  PLUS,
  SLASH,
  STAR,
  EQUAL_EQUAL,
  NOT_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,
  AND,
  OR,
}

impl Debug for BinOpKind {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    Display::fmt(&self, f,)
  }
}

impl Display for BinOpKind {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    match self {
      BinOpKind::MINUS => write!(f, "-"),
      BinOpKind::PLUS => write!(f, "+"),
      BinOpKind::SLASH => write!(f, "/"),
      BinOpKind::STAR => write!(f, "*"),
      BinOpKind::EQUAL_EQUAL => write!(f, "=="),
      BinOpKind::NOT_EQUAL => write!(f, "!="),
      BinOpKind::GREATER => write!(f, ">"),
      BinOpKind::GREATER_EQUAL => write!(f, ">="),
      BinOpKind::LESS => write!(f, "<"),
      BinOpKind::LESS_EQUAL => write!(f, "<="),
      BinOpKind::AND => write!(f, "&&"),
      BinOpKind::OR => write!(f, "||"),
    }
  }
}

impl From<Token,> for BinOpKind {
  fn from(token:Token,) -> Self {
    match token.kind {
      TokenKind::STAR => BinOpKind::STAR,
      TokenKind::SLASH => BinOpKind::SLASH,
      TokenKind::PLUS => BinOpKind::PLUS,
      TokenKind::MINUS => BinOpKind::MINUS,
      TokenKind::EQUAL_EQUAL => BinOpKind::EQUAL_EQUAL,
      TokenKind::NOT_EQUAL => BinOpKind::NOT_EQUAL,
      TokenKind::AND => BinOpKind::AND,
      TokenKind::OR => BinOpKind::OR,
      TokenKind::GREATER => BinOpKind::GREATER,
      TokenKind::GREATER_EQUAL => BinOpKind::GREATER_EQUAL,
      TokenKind::LESS => BinOpKind::LESS,
      TokenKind::LESS_EQUAL => BinOpKind::LESS_EQUAL,
      _ => unreachable!("Cannot create a BinOpKind from {:?}", token.kind),
    }
  }
}

#[allow(non_camel_case_types)]
#[derive(Clone, PartialEq, Eq,)]
pub enum AssignOpKind {
  STAR_EQUAL,
  SLASH_EQUAL,
  PLUS_EQUAL,
  MINUS_EQUAL,
}

impl From<Token,> for AssignOpKind {
  fn from(token:Token,) -> Self {
    match token.kind {
      TokenKind::STAR_EQUAL => AssignOpKind::STAR_EQUAL,
      TokenKind::SLASH_EQUAL => AssignOpKind::SLASH_EQUAL,
      TokenKind::PLUS_EQUAL => AssignOpKind::PLUS_EQUAL,
      TokenKind::MINUS_EQUAL => AssignOpKind::MINUS_EQUAL,
      _ => unreachable!("Cannot create a AssignOpKind from {:?}", token.kind),
    }
  }
}

impl Debug for AssignOpKind {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    Display::fmt(&self, f,)
  }
}

impl Display for AssignOpKind {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    match self {
      AssignOpKind::STAR_EQUAL => write!(f, "*="),
      AssignOpKind::SLASH_EQUAL => write!(f, "/="),
      AssignOpKind::PLUS_EQUAL => write!(f, "+="),
      AssignOpKind::MINUS_EQUAL => write!(f, "-="),
    }
  }
}

#[derive(Debug, Clone, PartialEq,)]
pub struct Block {
  pub(super) inner_block:Vec<Statement,>,
  // loc:Location,
}

impl IntoIterator for Block {
  type Item = Statement;

  type IntoIter = IntoIter<Statement,>;

  fn into_iter(self,) -> Self::IntoIter {
    self.inner_block.into_iter()
  }
}

impl Block {
  pub fn iter_mut(&mut self,) -> IterMut<'_, Statement,> {
    self.inner_block.iter_mut()
  }
}

#[derive(Clone, Debug, PartialEq,)]
pub enum UnOp {
  Not,
  Minus,
}

impl From<&Token,> for UnOp {
  fn from(value:&Token,) -> Self {
    match value.kind {
      TokenKind::NOT => UnOp::Not,
      TokenKind::MINUS => UnOp::Minus,
      _ => unreachable!(),
    }
  }
}

impl Display for UnOp {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    match self {
      UnOp::Not => write!(f, "!"),
      UnOp::Minus => write!(f, "-"),
    }
  }
}

#[derive(Clone, PartialEq,)]
pub enum ExpressionKind {
  Literal(P<Literal,>,),
  ///A binary operation (e.g.`true == false`).
  BinOp(P<Expression,>, BinOpKind, P<Expression,>,),
  ///A binary operation (e.g.`!true`).
  Unary(UnOp, P<Expression,>,),
  /// An `if` block, with an optional `else` block (e.g. `if expr {
  /// Vec<Statement> } else { expr }`).
  If(P<Expression,>, P<Block,>, Option<P<Block,>,>,),
  /// Variable reference.
  Ident(IdentInner,),
  /// An assignment (`a = foo()`) .
  Assign(P<Expression,>, P<Expression,>,),
  /// An assignment with an operator (e.g.`a += 1`).
  AssignOp(P<Expression,>, AssignOpKind, P<Expression,>,),
  // /// An array (e.g, `[a, b, c, d]`).
  // Array(Vec<P<Expression,>,>,),
  /// A tuple (e.g., `(a, b, c, d)`).
  Tuple(Vec<P<Expression,>,>,),
  /// A while loop.
  WhileLoop(P<Expression,>, P<Block,>,),
  /// A `for` loop.
  ///
  /// A `for` expression extracts values from an iterator, looping until the
  /// iterator is empty This is desugared to a combination of `loop` and
  /// `match` expressions (e.g. `for <pat> in <expr> <block>`). ()
  ForLoop {
    pat:P<Pat,>,
    iter:P<Expression,>,
    body:P<Block,>,
  },
  Range {
    left:P<Expression,>,
    inclusive:bool,
    right:P<Expression,>,
  },
  Break,
  Continue,
  Return(Option<P<Expression,>,>,),
}

impl Debug for ExpressionKind {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    Display::fmt(&self, f,)
  }
}

impl Display for ExpressionKind {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    match self {
      Self::Literal(lit,) => write!(f, "LITERAL({})", **lit,),
      Self::BinOp(left, op, right,) => write!(f, "BINOP({} {:?} {})", left.kind, op, right.kind),
      Self::Unary(op, expr,) => write!(f, "UNARY({:?} {:?})", op, expr.kind),
      Self::Ident(ident,) => match ident {
        IdentInner::Raw(raw_ident,) => write!(f, "IDENT(RAW({}))", lookup(raw_ident.symbol.idx)),
        IdentInner::DefId(def_id,) => write!(f, "IDENT(DEF({}))", def_id.0),
      },
      Self::Assign(left, right,) => write!(f, "ASSIGN({} = {})", left.kind, right.kind),
      Self::AssignOp(left, op, right,) => write!(f, "ASSIGNOP({} {} {})", left.kind, op, right.kind),
      Self::Tuple(elements,) => write!(f, "TUPLE({:?})", elements),
      Self::If(con, _, _,) => write!(f, "IF({})", con.kind),
      Self::WhileLoop(con, _,) => write!(f, "WHILE({}{{BLOCK PRINTING UNIMPLEMENTED}})", con.ptr.kind,),
      Self::ForLoop { pat, iter, body, } => todo!(),
      Self::Range { left, inclusive, right, } => {
        let range = match inclusive {
          true => "..=",
          false => "..",
        };
        write!(f, "RANGE({:?}{}{:?})", left.ptr.kind, range, right.ptr.kind)
      }
      Self::Break => write!(f, "BREAK"),
      Self::Continue => write!(f, "CONTINUE"),
      Self::Return(ret,) => write!(f, "RETURN {:?}", ret),
    }
  }
}

#[derive(Debug, Clone, PartialEq,)]
pub struct Expression {
  pub id:usize,
  pub kind:ExpressionKind,
  pub loc:Location,
}

#[derive(Debug, Clone, PartialEq, Eq,)]
pub enum Ty {
  Int,
  Float,
  Usize,
  Str,
  Bool,
  Array(P<Ty,>,),
}

impl Ty {
  ///Creates a new [`Ty::Array`] containing the passed `Ty`.
  pub fn new_array(ty:Ty,) -> Self {
    Ty::Array(P::new(ty,),)
  }

  pub fn copy(&self,) -> Self {
    self.clone()
  }
}

impl Ty {
  pub fn new(val:String,) -> Option<Self,> {
    match val.as_str() {
      "int" => Some(Ty::Int,),
      "float" => Some(Ty::Float,),
      "usize" => Some(Ty::Usize,),
      "str" => Some(Ty::Str,),
      "bool" => Some(Ty::Bool,),
      "int[]" => Some(Ty::Array(P::new(Ty::Int,),),),
      "float[]" => Some(Ty::Array(P::new(Ty::Float,),),),
      "usize[]" => Some(Ty::Array(P::new(Ty::Usize,),),),
      "str[]" => Some(Ty::Array(P::new(Ty::Str,),),),
      //shouldn't return none should return an error that it is not a valid type?
      _ => None,
    }
  }

  ///Returns `true` if the [`Ty`] is a [`Ty::Int`].
  pub fn is_int(&self,) -> bool {
    self == &Ty::Int
  }

  ///Returns `true` if the [`Ty`] is a [`Ty::Float`].
  pub fn is_float(&self,) -> bool {
    self == &Ty::Float
  }

  ///Returns `true` if the [`Ty`] is a [`Ty::Str`].
  pub fn is_string(&self,) -> bool {
    self == &Ty::Str
  }
}

/// A function signature.
#[derive(Debug, Clone, PartialEq,)]
pub struct FnSig {
  pub params:Vec<(Symbol, Ty,),>,
  pub result:Option<Ty,>,
}

#[derive(Clone, PartialEq,)]
pub struct Func {
  pub id:usize,
  pub loc:Location,
  pub name:RawIdent,
  pub sig:FnSig,
  pub body:P<Block,>,
}

impl Display for Func {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    write!(f, "([PARAMS UNIMPLEMENTED]) {{{:?}}}", self.body)
  }
}

impl Debug for Func {
  fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
    Display::fmt(&self, f,)
  }
}

// #[derive(Clone, PartialEq,)]
// pub enum ItemKind {
//   /// A function definition.
//   Fn { sig:FnSig, body:P<Block,>, },
// }

// impl Display for ItemKind {
//   fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
//     match self {
//       ItemKind::Fn { sig, body, } => write!(f, "([PARAMS UNIMPLEMENTED])
// {{{:?}}}", body),     }
//   }
// }

// impl Debug for ItemKind {
//   fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
//     Display::fmt(&self, f,)
//   }
// }

// #[derive(Clone, PartialEq,)]
// pub struct Item {
//   pub id:usize,
//   pub loc:Location,
//   pub name:Symbol,
//   pub kind:ItemKind,
// }

// impl Display for Item {
//   fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
//     match &self.kind {
//       ItemKind::Fn { sig, body, } => write!(f, "Fn {:?} ([PARAMS
// UNIMPLEMENTED]) {{{:?}}}", lookup(self.name.idx), body),     }
//   }
// }

// impl Debug for Item {
//   fn fmt(&self, f:&mut std::fmt::Formatter<'_,>,) -> std::fmt::Result {
//     Display::fmt(&self, f,)
//   }
// }

#[derive(Debug, Clone, PartialEq,)]
pub enum PatKind {
  Ident(IdentInner,),
  Range {
    start:P<Expression,>,
    end:P<Expression,>,
    inclusive:bool,
  },
  Tuple(P<Vec<Pat,>,>,),
}

#[derive(Debug, Clone, PartialEq,)]
pub struct Pat {
  pub id:usize,
  ///[`Location`] of the first element in the statement
  pub loc:Location,
  pub kind:PatKind,
}

#[derive(Debug, Clone, PartialEq,)]
pub enum LocalKind {
  ///Local declaration with an initializer. Example: `let x = y;`
  Init(P<Expression,>,),
  ///Local declaration. Example: `let x;`
  Decl,
}

#[derive(Debug, Clone, PartialEq,)]
/// A local variable. May or may not be variable.
pub struct Local {
  pub id:usize,
  ///[`Location`] of the first element in the statement
  pub loc:Location,
  ///Type of the local.
  pub ty:Option<P<Ty,>,>,
  pub mutable:bool,
  pub pat:P<Pat,>,
  pub kind:LocalKind,
}

#[derive(Debug, Clone, PartialEq,)]
pub enum StatementKind {
  Let(P<Local,>,),
  Expression(Expression,),
  // Item(Item,),
  Func(Func,),
}

#[derive(Debug, Clone, PartialEq,)]
pub struct Statement {
  pub id:usize,
  ///[`Location`] of the first element in the statement
  pub loc:Location,
  pub kind:StatementKind,
}

impl Statement {
  pub fn new(loc:Location, kind:StatementKind,) -> Self {
    Statement { id:0, loc, kind, }
  }
}

#[derive(Debug,)]
pub struct AbstractSyntaxTree {
  pub statements:Vec<Statement,>,
}

impl AbstractSyntaxTree {
  pub fn new() -> Self {
    AbstractSyntaxTree { statements:Vec::new(), }
  }

  pub fn push(&mut self, statement:Statement,) {
    self.statements.push(statement,);
  }
}

// pub enum PatKind {
//   Wild,
//   Ident(BindingMode, Ident, Option<P<Pat>>),
//   Struct(Option<P<QSelf>>, Path, ThinVec<PatField>, PatFieldsRest),
//   TupleStruct(Option<P<QSelf>>, Path, ThinVec<P<Pat>>),
//   Or(ThinVec<P<Pat>>),
//   Path(Option<P<QSelf>>, Path),
//   Tuple(ThinVec<P<Pat>>),
//   Box(P<Pat>),
//   Deref(P<Pat>),
//   Ref(P<Pat>, Mutability),
//   Lit(P<Expr>),
//   Range(Option<P<Expr>>, Option<P<Expr>>, Spanned<RangeEnd>),
//   Slice(ThinVec<P<Pat>>),
//   Rest,
//   Never,
//   Paren(P<Pat>),
//   MacCall(P<MacCall>),
//   Err(ErrorGuaranteed),
// }

// #[derive(Clone, Encodable, Decodable, Debug)]
// pub struct Expr {
//     pub id: NodeId,
//     pub kind: ExprKind,
//     pub span: Span,
//     pub attrs: AttrVec,
//     pub tokens: Option<LazyAttrTokenStream>,
// }

// #[derive(Clone, Encodable, Decodable, Debug)]
// pub enum ExprKind {

//     /// Allow anonymous constants from an inline `const` block
//     ConstBlock(AnonConst),
//     /// A function call
//     ///
//     /// The first field resolves to the function itself,
//     /// and the second field is the list of arguments.
//     /// This also represents calling the constructor of
//     /// tuple-like ADTs such as tuple structs and enum variants.
//     Call(P<Expr>, ThinVec<P<Expr>>),
//     /// A method call (e.g., `x.foo::<Bar, Baz>(a, b, c)`).
//     MethodCall(Box<MethodCall>),
//     /// A tuple (e.g., `(a, b, c, d)`).
//     Tup(ThinVec<P<Expr>>),
//     /// A binary operation (e.g., `a + b`, `a * b`).
//     Binary(BinOp, P<Expr>, P<Expr>),
//     /// A unary operation (e.g., `!x`, `*x`).
//     Unary(UnOp, P<Expr>),
//     /// A literal (e.g., `1`, `"foo"`).
//     Lit(token::Lit),
//     /// A cast (e.g., `foo as f64`).
//     Cast(P<Expr>, P<Ty>),
//     /// A type ascription (e.g., `builtin # type_ascribe(42, usize)`).
//     ///
//     /// Usually not written directly in user code but
//     /// indirectly via the macro `type_ascribe!(...)`.
//     Type(P<Expr>, P<Ty>),
//     /// A `let pat = expr` expression that is only semantically allowed in
// the condition     /// of `if` / `while` expressions. (e.g., `if let 0 = x {
// .. }`).     ///
//     /// `Span` represents the whole `let pat = expr` statement.
//     Let(P<Pat>, P<Expr>, Span, Recovered),
//     /// An `if` block, with an optional `else` block.
//     ///
//     /// `if expr { block } else { expr }`
//     If(P<Expr>, P<Block>, Option<P<Expr>>),
//     /// A while loop, with an optional label.
//     ///
//     /// `'label: while expr { block }`
//     While(P<Expr>, P<Block>, Option<Label>),
//     /// A `for` loop, with an optional label.
//     ///
//     /// `'label: for await? pat in iter { block }`
//     ///
//     /// This is desugared to a combination of `loop` and `match` expressions.
//     ForLoop { pat: P<Pat>, iter: P<Expr>, body: P<Block>, label:
// Option<Label>, kind: ForLoopKind },     /// Conditionless loop (can be exited
// with `break`, `continue`, or `return`).     ///
//     /// `'label: loop { block }`
//     Loop(P<Block>, Option<Label>, Span),
//     /// A `match` block.
//     Match(P<Expr>, ThinVec<Arm>, MatchKind),
//     /// A closure (e.g., `move |a, b, c| a + b + c`).
//     Closure(Box<Closure>),
//     /// A block (`'label: { ... }`).
//     Block(P<Block>, Option<Label>),
//     /// An `async` block (`async move { ... }`),
//     /// or a `gen` block (`gen move { ... }`)
//     Gen(CaptureBy, P<Block>, GenBlockKind),
//     /// An await expression (`my_future.await`). Span is of await keyword.
//     Await(P<Expr>, Span),

//     /// A try block (`try { ... }`).
//     TryBlock(P<Block>),

//     /// An assignment (`a = foo()`).
//     /// The `Span` argument is the span of the `=` token.
//     Assign(P<Expr>, P<Expr>, Span),
//     /// An assignment with an operator.
//     ///
//     /// E.g., `a += 1`.
//     AssignOp(BinOp, P<Expr>, P<Expr>),
//     /// Access of a named (e.g., `obj.foo`) or unnamed (e.g., `obj.0`) struct
// field.     Field(P<Expr>, Ident),
//     /// An indexing operation (e.g., `foo[2]`).
//     /// The span represents the span of the `[2]`, including brackets.
//     Index(P<Expr>, P<Expr>, Span),
//     /// A range (e.g., `1..2`, `1..`, `..2`, `1..=2`, `..=2`; and `..` in
// destructuring assignment).     Range(Option<P<Expr>>, Option<P<Expr>>,
// RangeLimits),     /// An underscore, used in destructuring assignment to
// ignore a value.     Underscore,

//     /// Variable reference, possibly containing `::` and/or type
//     /// parameters (e.g., `foo::bar::<baz>`).
//     ///
//     /// Optionally "qualified" (e.g., `<Vec<T> as SomeTrait>::SomeType`).
//     Path(Option<P<QSelf>>, Path),

//     /// A referencing operation (`&a`, `&mut a`, `&raw const a` or `&raw mut
// a`).     AddrOf(BorrowKind, Mutability, P<Expr>),
//     /// A `break`, with an optional label to break, and an optional
// expression.     Break(Option<Label>, Option<P<Expr>>),
//     /// A `continue`, with an optional label.
//     Continue(Option<Label>),
//     /// A `return`, with an optional value to be returned.
//     Ret(Option<P<Expr>>),

//     /// Output of the `asm!()` macro.
//     InlineAsm(P<InlineAsm>),

//     /// An `offset_of` expression (e.g., `builtin # offset_of(Struct,
// field)`).     ///
//     /// Usually not written directly in user code but
//     /// indirectly via the macro `core::mem::offset_of!(...)`.
//     OffsetOf(P<Ty>, P<[Ident]>),

//     /// A macro invocation; pre-expansion.
//     MacCall(P<MacCall>),

//     /// A struct literal expression.
//     ///
//     /// E.g., `Foo {x: 1, y: 2}`, or `Foo {x: 1, .. rest}`.
//     Struct(P<StructExpr>),

//     /// An array literal constructed from one repeated element.
//     ///
//     /// E.g., `[1; 5]`. The expression is the element to be
//     /// repeated; the constant is the number of times to repeat it.
//     Repeat(P<Expr>, AnonConst),

//     /// No-op: used solely so we can pretty-print faithfully.
//     Paren(P<Expr>),

//     /// A try expression (`expr?`).
//     Try(P<Expr>),

//     /// A `yield`, with an optional value to be yielded.
//     Yield(Option<P<Expr>>),

//     /// A `do yeet` (aka `throw`/`fail`/`bail`/`raise`/whatever),
//     /// with an optional value to be returned.
//     Yeet(Option<P<Expr>>),

//     /// A tail call return, with the value to be returned.
//     ///
//     /// While `.0` must be a function call, we check this later, after
// parsing.     Become(P<Expr>),

//     /// Bytes included via `include_bytes!`
//     /// Added for optimization purposes to avoid the need to escape
//     /// large binary blobs - should always behave like [`ExprKind::Lit`]
//     /// with a `ByteStr` literal.
//     IncludedBytes(Lrc<[u8]>),

//     /// A `format_args!()` expression.
//     FormatArgs(P<FormatArgs>),

//     /// Placeholder for an expression that wasn't syntactically well formed
// in some way.     Err(ErrorGuaranteed),

//     /// Acts as a null expression. Lowering it will always emit a bug.
//     Dummy,
// }

#[cfg(test)]
mod tests {
  #[test]
  fn expressions_display() {}
}
