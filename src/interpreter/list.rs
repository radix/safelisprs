//! The interpreter implementation of a list. This is a purely functional data
//! structure that tries to share data where possible between "generations" of a
//! list. This is sometimes called a "persistent" data structure.

use gc_arena::{Collect, Gc, Mutation};

const CHUNK_MAX: usize = 64;
const BRANCH_MAX: usize = 8;

#[derive(Collect)]
#[collect(no_drop)]
pub struct List<'gc, T: 'gc> {
  node: Gc<'gc, Node<'gc, T>>,
  len: usize,
}

#[derive(Collect)]
#[collect(no_drop)]
enum Node<'gc, T: 'gc> {
  Leaf(Vec<T>),
  Branch(Vec<Gc<'gc, Node<'gc, T>>>),
}

impl<'gc, T: Clone + Collect<'gc>> List<'gc, T> {
  pub fn new(mc: &Mutation<'gc>) -> List<'gc, T> {
    List {
      node: Gc::new(mc, Node::Leaf(vec![])),
      len: 0,
    }
  }

  pub fn append(&self, mc: &Mutation<'gc>, v: T) -> List<'gc, T> {
    let new_node = Gc::new(mc, Node::append(self.node, mc, v));

    List {
      node: new_node,
      len: self.len + 1,
    }
  }

  pub fn len(&self) -> usize {
    self.len
  }

  pub fn iter(&self) -> IterList<'gc, T> {
    let list = self.clone();
    IterList { list, idx: 0 }
  }

  pub fn get(&self, idx: usize) -> Option<T> {
    self.node.get(idx)
  }
}

impl<'gc, T> Clone for List<'gc, T> {
  fn clone(&self) -> Self {
    Self {
      node: self.node.clone(),
      len: self.len,
    }
  }
}

impl<'gc, T: Clone + Collect<'gc>> Node<'gc, T> {
  fn len(&self) -> usize {
    match self {
      Node::Leaf(items) => items.len(),
      Node::Branch(nodes) => nodes.iter().map(|n| n.len()).sum(),
    }
  }

  fn get(&self, index: usize) -> Option<T> {
    // TODO: this sucks because we aren't keeping track of sizes higher up,
    // so we have to walk everything instead of doing a binary search?
    match &*self {
      Node::Leaf(items) => items.get(index).cloned(),
      Node::Branch(nodes) => {
        let mut cur = 0;
        for node in nodes {
          let v = node.get(index - cur);
          if v.is_some() {
            return v;
          }
          cur += node.len();
        }
        None
      }
    }
  }
  fn append(node: Gc<'gc, Node<'gc, T>>, mc: &Mutation<'gc>, v: T) -> Node<'gc, T> {
    match &*node {
      Node::Leaf(items) => {
        if items.len() > CHUNK_MAX {
          Node::Branch(vec![node, Gc::new(mc, Node::Leaf(vec![v]))])
        } else {
          let mut items = items.clone();
          items.push(v);
          Node::Leaf(items)
        }
      }
      Node::Branch(nodes) => {
        let Some(last_node) = nodes.last() else {
          todo!();
        };
        let nodes = &nodes[0..nodes.len() - 1];
        let mut new_nodes = nodes.to_vec();
        new_nodes.push(Gc::new(mc, Node::append(*last_node, mc, v)));
        Node::Branch(new_nodes)
      }
    }
  }
}

pub struct IterList<'gc, T: 'gc> {
  list: List<'gc, T>,
  idx: usize,
}

impl<'gc, T: Clone + Collect<'gc>> Iterator for IterList<'gc, T> {
  type Item = T;

  fn next(&mut self) -> Option<Self::Item> {
    if self.idx >= self.list.len {
      return None;
    }
    let v = self.list.node.get(self.idx);
    self.idx += 1;
    v
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use gc_arena::arena::rootless_mutate;
  use gc_arena::{Arena, GcWeak, Rootable};

  #[derive(Collect)]
  #[collect(no_drop)]
  struct GcItemListRoot<'gc> {
    list: List<'gc, Gc<'gc, usize>>,
    listed_item: GcWeak<'gc, usize>,
    unlisted_item: GcWeak<'gc, usize>,
  }

  #[test]
  fn empty_iter() {
    rootless_mutate(|mc| {
      let l: List<'_, usize> = List::new(mc);
      let v: Vec<usize> = l.iter().collect();
      assert_eq!(v, vec![]);
    });
  }
  #[test]
  fn append() {
    rootless_mutate(|mc| {
      let l: List<'_, usize> = List::new(mc);
      let l2 = l.append(mc, 1);
      let v2: Vec<usize> = l2.iter().collect();
      assert_eq!(v2, vec![1]);
      let v: Vec<usize> = l.iter().collect();
      assert_eq!(v, vec![]);
    });
  }

  #[test]
  fn index() {
    rootless_mutate(|mc| {
      let l: List<'_, usize> = List::new(mc);
      assert_eq!(l.get(0), None);
    });
  }

  #[test]
  fn over_chunk_max() {
    rootless_mutate(|mc| {
      let mut l: List<'_, usize> = List::new(mc);
      let items: Vec<usize> = (0..100).collect();
      for i in &items {
        l = l.append(mc, *i);
      }
      let v: Vec<usize> = l.iter().collect();
      assert_eq!(v, items)
    });
  }

  #[test]
  fn gc_items_are_traced_through_the_list() {
    let mut arena = Arena::<Rootable![GcItemListRoot<'_>]>::new(|mc| {
      let listed_item = Gc::new(mc, 42);
      let unlisted_item = Gc::new(mc, 99);
      GcItemListRoot {
        list: List::new(mc).append(mc, listed_item),
        listed_item: Gc::downgrade(listed_item),
        unlisted_item: Gc::downgrade(unlisted_item),
      }
    });

    arena.finish_cycle();

    arena.mutate(|mc, root| {
      let item = root
        .listed_item
        .upgrade(mc)
        .expect("the list should keep its GCed item alive");
      assert_eq!(*item, 42);
      assert!(Gc::ptr_eq(item, root.list.get(0).unwrap()));
      assert!(root.unlisted_item.upgrade(mc).is_none());
    });
  }
}
