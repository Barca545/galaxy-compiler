use std::{cell::RefCell, collections::HashMap};

// Refactor:
// - Use the final implementation from here: https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html
// - Key might need to be "Symbol"

pub struct Interner {
  map:HashMap<String, u32,>,
  vec:Vec<String,>,
}

impl Interner {
  pub fn new() -> Self {
    Interner {
      map:HashMap::new(),
      vec:Vec::new(),
    }
  }
  pub fn intern(&mut self, str:&str,) -> u32 {
    match self.map.get(str,) {
      Some(idx,) => *idx,
      None => {
        let idx = self.map.len() as u32;
        self.map.insert(str.to_owned(), idx,);
        self.vec.push(str.to_owned(),);
        idx
      }
    }
  }

  pub fn lookup(&self, idx:u32,) -> &str {
    self.vec[idx as usize].as_str()
  }
}

thread_local! {pub static INTERNER:RefCell<Interner,> = RefCell::new(Interner::new(),)}

/// Adds a string to the global string [`Interner`] and returns its key.
pub fn intern(str:&str,) -> u32 {
  INTERNER.with_borrow_mut(|interner| interner.intern(str,),)
}

/// Retrieve a string from the global string [`Interner`] by its key.
pub fn lookup<I,>(idx:I,) -> String
where u32: From<I,> {
  INTERNER.with_borrow(|interner| interner.lookup(idx.into(),).to_owned(),)
}

#[cfg(test)]
mod test {
  use super::{intern, lookup};

  #[test]
  fn insertion_and_lookup_works() {
    let idx = intern("test string",);
    let string = lookup(idx,);
    assert_eq!(string, "test string");
  }
}
