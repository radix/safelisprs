//! The interpreter implementation of a list. This is a purely functional data
//! structure that tries to share data where possible between "generations" of a
//! list. This is sometimes called a "persistent" data structure.

use std::sync::Arc;

const CHUNK_MAX: usize = 64;
const BRANCH_MAX: usize = 8;

// should we just put a T: Clone bound on or should we explicitly wrap T in Gc?
pub struct List<T> {
  // TODO: GC
  node: Arc<Node<T>>,
  len: usize,
}

enum Node<T> {
  Leaf(Vec<Arc<T>>),
  Branch(Vec<Arc<Node<T>>>),
}

impl<T> List<T> {
  pub fn new() -> List<T> {
    List {
      node: Arc::new(Node::Leaf(vec![])),
      len: 0,
    }
  }

  pub fn append(&self, v: T) -> List<T> {
    let node = self.node.clone();
    let new_node = Arc::new(node.append(v));

    List {
      node: new_node,
      len: self.len + 1,
    }
  }

  pub fn len(&self) -> usize {
    self.len
  }

  pub fn iter(&self) -> IterList<T> {
    let list = self.clone();
    IterList { list, idx: 0 }
  }

  pub fn get(&self, idx: usize) -> Option<Arc<T>> {
    self.node.arc_index(idx)
  }
}

impl<T> Clone for List<T> {
  fn clone(&self) -> Self {
    Self {
      node: self.node.clone(),
      len: self.len.clone(),
    }
  }
}

impl<T> Node<T> {
  fn len(&self) -> usize {
    match self {
      Node::Leaf(items) => items.len(),
      Node::Branch(nodes) => nodes.iter().map(|n| n.len()).sum(),
    }
  }

  fn arc_index(&self, index: usize) -> Option<Arc<T>> {
    // TODO: this sucks because we aren't keeping track of sizes higher up,
    // so we have to walk everything instead of doing a binary search?
    match &*self {
      Node::Leaf(items) => items.get(index).cloned(),
      Node::Branch(nodes) => {
        let mut cur = 0;
        for node in nodes {
          let v = node.arc_index(index - cur);
          if v.is_some() {
            return v;
          }
          cur += node.len();
        }
        None
      }
    }
  }
  fn append(self: Arc<Node<T>>, v: T) -> Node<T> {
    match &*self {
      Node::Leaf(items) => {
        if items.len() > CHUNK_MAX {
          Node::Branch(vec![self.clone(), Arc::new(Node::Leaf(vec![Arc::new(v)]))])
        } else {
          let mut items = items.clone();
          items.push(Arc::new(v));
          Node::Leaf(items)
        }
      }
      Node::Branch(nodes) => {
        let Some(last_node) = nodes.last() else {
          todo!();
        };
        let nodes = &nodes[0..nodes.len() - 1];
        let mut new_nodes = nodes.to_vec();
        new_nodes.push(Arc::new(last_node.clone().append(v)));
        Node::Branch(new_nodes)
      }
    }
  }
}

pub struct IterList<T> {
  list: List<T>,
  idx: usize,
}

impl<T> Iterator for IterList<T> {
  type Item = Arc<T>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.idx >= self.list.len {
      return None;
    }
    let v = self.list.node.arc_index(self.idx);
    self.idx += 1;
    v
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn empty_iter() {
    let l: List<usize> = List::new();
    let v: Vec<Arc<usize>> = l.iter().collect();
    assert_eq!(v, vec![]);
  }
  #[test]
  fn append() {
    let l: List<usize> = List::new();
    let l2 = l.append(1);
    let v2: Vec<Arc<usize>> = l2.iter().collect();
    assert_eq!(v2, vec![Arc::new(1)]);
    let v: Vec<Arc<usize>> = l.iter().collect();
    assert_eq!(v, vec![]);
  }

  #[test]
  fn index() {
    let l: List<usize> = List::new();
    assert_eq!(l.get(0), None);
  }

  #[test]
  fn over_chunk_max() {
    let mut l: List<usize> = List::new();
    let items: Vec<usize> = (0..100).into_iter().collect();
    for i in &items {
      l = l.append(*i);
    }
    let v: Vec<Arc<usize>> = l.iter().collect();
    let expected_items: Vec<Arc<usize>> = items.into_iter().map(Arc::new).collect();
    assert_eq!(v, expected_items)
  }
}
