use std::{collections::HashMap, mem::size_of};

//should data type be shared with the parser
//Symbol should be a u32 maybe make it an struct in the Interner

// struct SymbolTable{
//   //a normal Rc instead of a Weak might be fine here.
//    parent:Option<Weak<SymbolTable>>,
//    //the u32 is an index into my string interner
//    table: HashMap<u32, SymbolData>
//  }

//  struct SymbolData{
//    ty: Option<Ty>,
//    //The register it is allocated
//    reg: Option<usize>,
//    children:Vec<SymbolTable>
//  }

//  impl SymbolTable{
//    fn lookup(&self, name:u32) -> Option<SymbolData> {
//      match self.table.get(name){
//        //might be a more elegant way to do this
//        Some(sym) => Some(sym),
//        None => self.parent.lookup(name)
//      }
//    }
//  }

enum SymanticType {}

enum DataType {
  Int,
  Float,
  Usize,
}
#[derive(Debug, Clone,)]
pub struct Symbol {
  pub idx:u32,
}

struct AttributeTable {}

struct SymbolTable {
  hashtable:HashMap<u32, Symbol,>,
}

impl SymbolTable {
  fn hash(&self, string:String,) -> u32 {
    let len = string.len();
    let num_shifts = len.min(8 * size_of::<u32,>() - 8,);
    let start_char = (len - num_shifts) % 2;

    let mut code = 0;
    for i in start_char..start_char + num_shifts {
      code = (code << 1) + string.chars().nth(i,).unwrap() as u32;
    }
    code % self.hashtable.len() as u32
  }

  //insert checks if a key is already in and errors saying can't reuse names
}

#[cfg(test)]
mod tests {
  #[test]
  fn hash_works() {}
}
