//! The interpreter implementation of a list. This is a purely functional data
//! structure that tries to share data where possible between "generations" of a
//! list. This is sometimes called a "persistent" data structure.

use std::fmt;

use gc_arena::{Collect, Gc, Mutation};

use super::SharedTracker;

const CHUNK_MAX: usize = 64;
const BRANCH_MAX: usize = 8;

#[derive(Collect)]
#[collect(no_drop)]
pub struct List<'gc, T: 'gc> {
  node: Gc<'gc, Node<'gc, T>>,
  len: usize,
  #[collect(require_static)]
  tracker: Option<SharedTracker>,
}

#[derive(Collect)]
#[collect(no_drop)]
struct Node<'gc, T: 'gc> {
  kind: NodeKind<'gc, T>,
  len: usize,
  #[allow(dead_code)]
  #[collect(require_static)]
  charge: NodeCharge,
}

#[derive(Collect)]
#[collect(no_drop)]
enum NodeKind<'gc, T: 'gc> {
  Leaf(Vec<T>),
  Branch(Vec<Gc<'gc, Node<'gc, T>>>),
}

struct NodeCharge {
  tracker: Option<SharedTracker>,
  bytes: usize,
}

impl NodeCharge {
  fn new(tracker: Option<SharedTracker>, bytes: usize) -> Self {
    if let Some(tracker) = &tracker {
      tracker.charge(bytes);
    }
    NodeCharge { tracker, bytes }
  }
}

impl Drop for NodeCharge {
  fn drop(&mut self) {
    if let Some(tracker) = &self.tracker {
      tracker.release(self.bytes);
    }
  }
}

impl<'gc, T: Clone + Collect<'gc>> List<'gc, T> {
  pub fn new(mc: &Mutation<'gc>) -> List<'gc, T> {
    Self::new_with_tracker(mc, None)
  }

  pub(super) fn new_tracked(mc: &Mutation<'gc>, tracker: SharedTracker) -> List<'gc, T> {
    Self::new_with_tracker(mc, Some(tracker))
  }

  fn new_with_tracker(mc: &Mutation<'gc>, tracker: Option<SharedTracker>) -> List<'gc, T> {
    List {
      node: Node::new(mc, NodeKind::Leaf(vec![]), tracker.clone()),
      len: 0,
      tracker,
    }
  }

  pub fn from_vec(mc: &Mutation<'gc>, values: Vec<T>) -> List<'gc, T> {
    Self::from_vec_with_tracker(mc, values, None)
  }

  pub(super) fn from_vec_tracked(
    mc: &Mutation<'gc>,
    values: Vec<T>,
    tracker: SharedTracker,
  ) -> List<'gc, T> {
    Self::from_vec_with_tracker(mc, values, Some(tracker))
  }

  fn from_vec_with_tracker(
    mc: &Mutation<'gc>,
    values: Vec<T>,
    tracker: Option<SharedTracker>,
  ) -> List<'gc, T> {
    let len = values.len();
    if values.is_empty() {
      return Self::new_with_tracker(mc, tracker);
    }

    let mut values = values.into_iter();
    let mut nodes = Vec::with_capacity(len.div_ceil(CHUNK_MAX));
    loop {
      let items: Vec<T> = values.by_ref().take(CHUNK_MAX).collect();
      if items.is_empty() {
        break;
      }
      nodes.push(Node::new(mc, NodeKind::Leaf(items), tracker.clone()));
    }

    while nodes.len() > 1 {
      let mut parents = Vec::with_capacity(nodes.len().div_ceil(BRANCH_MAX));
      for children in nodes.chunks(BRANCH_MAX) {
        parents.push(Node::new(
          mc,
          NodeKind::Branch(children.to_vec()),
          tracker.clone(),
        ));
      }
      nodes = parents;
    }

    List {
      node: nodes[0],
      len,
      tracker,
    }
  }

  pub fn append(&self, mc: &Mutation<'gc>, v: T) -> List<'gc, T> {
    let (node, overflow) = Node::append(self.node, mc, v, self.tracker.clone());
    let node = match overflow {
      Some(overflow) => Node::new(
        mc,
        NodeKind::Branch(vec![node, overflow]),
        self.tracker.clone(),
      ),
      None => node,
    };

    List {
      node,
      len: self.len + 1,
      tracker: self.tracker.clone(),
    }
  }

  pub fn len(&self) -> usize {
    self.len
  }

  pub fn is_empty(&self) -> bool {
    self.len == 0
  }

  pub fn iter(&self) -> IterList<'gc, T> {
    let list = self.clone();
    IterList { list, idx: 0 }
  }

  pub fn get(&self, idx: usize) -> Option<T> {
    self.node.get(idx)
  }

  pub fn concat(&self, mc: &Mutation<'gc>, other: &Self) -> Self {
    let mut result = self.clone();
    for value in other.iter() {
      result = result.append(mc, value);
    }
    result
  }
}

impl<'gc, T> Clone for List<'gc, T> {
  fn clone(&self) -> Self {
    Self {
      node: self.node,
      len: self.len,
      tracker: self.tracker.clone(),
    }
  }
}

impl<'gc, T: fmt::Debug + Clone + Collect<'gc>> fmt::Debug for List<'gc, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

impl<'gc, T: PartialEq + Clone + Collect<'gc>> PartialEq for List<'gc, T> {
  fn eq(&self, other: &Self) -> bool {
    self.len == other.len && self.iter().eq(other.iter())
  }
}

impl<'gc, T: Eq + Clone + Collect<'gc>> Eq for List<'gc, T> {}

impl<'gc, T: Clone + Collect<'gc>> Node<'gc, T> {
  fn new(
    mc: &Mutation<'gc>,
    kind: NodeKind<'gc, T>,
    tracker: Option<SharedTracker>,
  ) -> Gc<'gc, Node<'gc, T>> {
    let len = match &kind {
      NodeKind::Leaf(items) => items.len(),
      NodeKind::Branch(nodes) => nodes.iter().map(|node| node.len).sum(),
    };
    let bytes = match &kind {
      NodeKind::Leaf(items) => items.capacity() * std::mem::size_of::<T>(),
      NodeKind::Branch(nodes) => nodes.capacity() * std::mem::size_of::<Gc<'gc, Node<'gc, T>>>(),
    };
    Gc::new(
      mc,
      Node {
        kind,
        len,
        charge: NodeCharge::new(tracker, bytes),
      },
    )
  }

  fn len(&self) -> usize {
    self.len
  }

  fn get(&self, index: usize) -> Option<T> {
    // Subtree lengths make each lookup logarithmic in the list length. The
    // scan at each level is bounded by BRANCH_MAX.
    match &self.kind {
      NodeKind::Leaf(items) => items.get(index).cloned(),
      NodeKind::Branch(nodes) => {
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
  fn append(
    node: Gc<'gc, Node<'gc, T>>,
    mc: &Mutation<'gc>,
    v: T,
    tracker: Option<SharedTracker>,
  ) -> (Gc<'gc, Node<'gc, T>>, Option<Gc<'gc, Node<'gc, T>>>) {
    match &node.kind {
      NodeKind::Leaf(items) => {
        if items.len() >= CHUNK_MAX {
          (node, Some(Node::new(mc, NodeKind::Leaf(vec![v]), tracker)))
        } else {
          let mut items = items.clone();
          items.push(v);
          (Node::new(mc, NodeKind::Leaf(items), tracker), None)
        }
      }
      NodeKind::Branch(nodes) => {
        let last_node = nodes.last().expect("branch nodes are never empty");
        let mut new_nodes = nodes.to_vec();
        let (appended, overflow) = Node::append(*last_node, mc, v, tracker.clone());
        *new_nodes.last_mut().unwrap() = appended;

        match overflow {
          None => (Node::new(mc, NodeKind::Branch(new_nodes), tracker), None),
          Some(overflow) if new_nodes.len() < BRANCH_MAX => {
            new_nodes.push(overflow);
            (Node::new(mc, NodeKind::Branch(new_nodes), tracker), None)
          }
          Some(overflow) => (
            Node::new(mc, NodeKind::Branch(new_nodes), tracker.clone()),
            Some(Node::new(mc, NodeKind::Branch(vec![overflow]), tracker)),
          ),
        }
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
  use std::rc::Rc;

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
  fn from_vec_and_append_span_multiple_branch_levels() {
    rootless_mutate(|mc| {
      let items: Vec<usize> = (0..10_000).collect();
      let list = List::from_vec(mc, items.clone());
      let appended = list.append(mc, 10_000);

      assert_eq!(list.len(), 10_000);
      assert_eq!(appended.len(), 10_001);
      assert_eq!(appended.get(0), Some(0));
      assert_eq!(appended.get(9_999), Some(9_999));
      assert_eq!(appended.get(10_000), Some(10_000));
      assert_eq!(
        appended.iter().collect::<Vec<_>>(),
        (0..=10_000).collect::<Vec<_>>()
      );
    });
  }

  #[test]
  fn concat_preserves_both_inputs() {
    rootless_mutate(|mc| {
      let left = List::from_vec(mc, vec![1, 2]);
      let right = List::from_vec(mc, vec![3, 4]);
      let combined = left.concat(mc, &right);

      assert_eq!(left.iter().collect::<Vec<_>>(), vec![1, 2]);
      assert_eq!(right.iter().collect::<Vec<_>>(), vec![3, 4]);
      assert_eq!(combined.iter().collect::<Vec<_>>(), vec![1, 2, 3, 4]);
    });
  }

  #[test]
  fn tracked_node_storage_is_released_when_collected() {
    let tracker = Rc::new(super::super::MemoryTracker::new());
    let arena_tracker = tracker.clone();
    let mut arena = Arena::<Rootable![List<'_, usize>]>::new(|mc| {
      List::from_vec_tracked(mc, (0..1_000).collect(), arena_tracker)
    });
    assert!(tracker.external_bytes() > 0);

    arena.mutate_root(|mc, root| {
      *root = List::new_tracked(mc, tracker.clone());
    });
    arena.finish_cycle();

    assert_eq!(tracker.external_bytes(), 0);
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
