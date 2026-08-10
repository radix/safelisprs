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

  pub(super) fn try_from_iter_tracked<I>(
    mc: &Mutation<'gc>,
    values: I,
    tracker: SharedTracker,
  ) -> Result<List<'gc, T>, String>
  where
    I: IntoIterator<Item = T>,
  {
    let mut values = values.into_iter();
    let prefix_len = values.size_hint().0;
    let mut list = if prefix_len == 0 {
      let node = Node::new(mc, NodeKind::Leaf(vec![]), Some(tracker.clone()));
      Node::<T>::ensure_memory_available(mc, &tracker, 0)?;
      List {
        node,
        len: 0,
        tracker: Some(tracker.clone()),
      }
    } else {
      let mut height = 0;
      let mut capacity = CHUNK_MAX;
      while prefix_len > capacity {
        height += 1;
        capacity = capacity.saturating_mul(BRANCH_MAX);
      }

      let node = Node::try_from_iter(mc, &mut values, prefix_len, height, &tracker)?;
      List {
        node,
        len: prefix_len,
        tracker: Some(tracker.clone()),
      }
    };

    // If the iterator is bigger than its size_hint, fall back to appending.
    for value in values {
      if list.len == usize::MAX {
        return Err("list: length overflow".to_string());
      }
      list = list.append(mc, value);
      Node::<T>::ensure_memory_available(mc, &tracker, 0)?;
    }
    Ok(list)
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

  pub fn remove(&self, mc: &Mutation<'gc>, idx: usize) -> Option<(T, List<'gc, T>)> {
    if idx >= self.len {
      return None;
    }
    let (node, item) = Node::remove(self.node, mc, idx, self.tracker.clone())?;
    let new_list = List {
      node,
      len: self.len - 1,
      tracker: self.tracker.clone(),
    };
    Some((item, new_list))
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
  fn capacity_at_height(height: usize) -> usize {
    (0..height).fold(CHUNK_MAX, |capacity, _| capacity.saturating_mul(BRANCH_MAX))
  }

  fn ensure_memory_available(
    mc: &Mutation<'gc>,
    tracker: &SharedTracker,
    additional_bytes: usize,
  ) -> Result<(), String> {
    let usage = mc
      .metrics()
      .total_gc_allocation()
      .checked_add(tracker.external_bytes())
      .and_then(|usage| usage.checked_add(additional_bytes))
      .ok_or_else(|| "memory accounting overflow while building list".to_string())?;
    if let Some(limit) = tracker.limit() {
      if usage > limit {
        return Err(format!(
          "memory limit exceeded: {} bytes live (limit {})",
          usage, limit
        ));
      }
    }
    Ok(())
  }

  fn try_from_iter<I>(
    mc: &Mutation<'gc>,
    values: &mut I,
    len: usize,
    height: usize,
    tracker: &SharedTracker,
  ) -> Result<Gc<'gc, Node<'gc, T>>, String>
  where
    I: Iterator<Item = T>,
  {
    if height == 0 {
      let bytes = len
        .checked_mul(std::mem::size_of::<T>())
        .ok_or_else(|| "list: leaf allocation size overflow".to_string())?;
      Self::ensure_memory_available(mc, tracker, bytes)?;

      let mut items = Vec::new();
      items
        .try_reserve_exact(len)
        .map_err(|_| format!("list: failed to allocate space for {len} values"))?;
      for _ in 0..len {
        let value = values
          .next()
          .ok_or_else(|| "list: iterator yielded fewer values than expected".to_string())?;
        items.push(value);
      }
      let node = Self::new(mc, NodeKind::Leaf(items), Some(tracker.clone()));
      Self::ensure_memory_available(mc, tracker, 0)?;
      return Ok(node);
    }

    let child_capacity = Self::capacity_at_height(height - 1);
    let child_count = len.div_ceil(child_capacity);
    debug_assert!(child_count <= BRANCH_MAX);
    let mut children = [None; BRANCH_MAX];
    let mut remaining = len;
    for child in children.iter_mut().take(child_count) {
      let child_len = remaining.min(child_capacity);
      *child = Some(Self::try_from_iter(
        mc,
        values,
        child_len,
        height - 1,
        tracker,
      )?);
      remaining -= child_len;
    }
    debug_assert_eq!(remaining, 0);

    let bytes = child_count
      .checked_mul(std::mem::size_of::<Gc<'gc, Node<'gc, T>>>())
      .ok_or_else(|| "list: branch allocation size overflow".to_string())?;
    Self::ensure_memory_available(mc, tracker, bytes)?;
    let mut nodes = Vec::new();
    nodes
      .try_reserve_exact(child_count)
      .map_err(|_| format!("list: failed to allocate space for {child_count} branches"))?;
    nodes.extend(
      children[..child_count]
        .iter()
        .map(|child| child.expect("all branch children are initialized")),
    );
    let node = Self::new(mc, NodeKind::Branch(nodes), Some(tracker.clone()));
    Self::ensure_memory_available(mc, tracker, 0)?;
    Ok(node)
  }

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
  fn remove(
    node: Gc<'gc, Node<'gc, T>>,
    mc: &Mutation<'gc>,
    idx: usize,
    tracker: Option<SharedTracker>,
  ) -> Option<(Gc<'gc, Node<'gc, T>>, T)> {
    match &node.kind {
      NodeKind::Leaf(items) => {
        let item = items.get(idx)?.clone();
        let mut new_items = items.clone();
        new_items.remove(idx);
        Some((Node::new(mc, NodeKind::Leaf(new_items), tracker), item))
      }
      NodeKind::Branch(nodes) => {
        let mut cur = 0;
        for (i, child) in nodes.iter().enumerate() {
          let child_len = child.len();
          if idx < cur + child_len {
            let (new_child, item) =
              Node::remove(*child, mc, idx - cur, tracker.clone())?;
            let mut new_nodes = nodes.to_vec();
            if new_child.len() == 0 {
              new_nodes.remove(i);
            } else {
              new_nodes[i] = new_child;
            }
            let kind = if new_nodes.is_empty() {
              NodeKind::Leaf(vec![])
            } else {
              NodeKind::Branch(new_nodes)
            };
            return Some((Node::new(mc, kind, tracker), item));
          }
          cur += child_len;
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

  fn size_hint(&self) -> (usize, Option<usize>) {
    let remaining = self.list.len - self.idx;
    (remaining, Some(remaining))
  }
}

impl<'gc, T: Clone + Collect<'gc>> ExactSizeIterator for IterList<'gc, T> {}

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
  fn tracked_iterator_builder_allocates_only_final_tree_nodes() {
    let tracker = Rc::new(super::super::MemoryTracker::new());
    let arena_tracker = tracker.clone();
    let arena = Arena::<Rootable![List<'_, usize>]>::new(|mc| {
      List::try_from_iter_tracked(mc, 0..1_000, arena_tracker).unwrap()
    });

    // 16 leaves, two intermediate branches, and one root branch.
    assert_eq!(arena.metrics().total_gc_count(), 19);
    arena.mutate(|_, list| {
      assert!(list.iter().eq(0..1_000));
    });
  }

  #[test]
  fn tracked_iterator_builder_appends_values_beyond_size_hint() {
    struct UnderreportedRange(std::ops::Range<usize>);

    impl Iterator for UnderreportedRange {
      type Item = usize;

      fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
      }

      fn size_hint(&self) -> (usize, Option<usize>) {
        let hinted = self.0.len().min(2);
        (hinted, Some(hinted))
      }
    }

    let tracker = Rc::new(super::super::MemoryTracker::new());
    let arena_tracker = tracker.clone();
    let arena = Arena::<Rootable![List<'_, usize>]>::new(|mc| {
      List::try_from_iter_tracked(mc, UnderreportedRange(0..100), arena_tracker).unwrap()
    });

    arena.mutate(|_, list| {
      assert_eq!(list.len(), 100);
      assert!(list.iter().eq(0..100));
    });
  }

  #[test]
  fn list_iterator_reports_exact_remaining_size() {
    rootless_mutate(|mc| {
      let list = List::from_vec(mc, vec![1, 2, 3]);
      let mut values = list.iter();

      assert_eq!(values.size_hint(), (3, Some(3)));
      assert_eq!(values.next(), Some(1));
      assert_eq!(values.size_hint(), (2, Some(2)));
      assert_eq!(values.len(), 2);
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
  fn remove_returns_item_and_new_list() {
    rootless_mutate(|mc| {
      let list = List::from_vec(mc, vec![1, 2, 3, 4, 5]);
      let (item, new_list) = list.remove(mc, 2).expect("index in range");
      assert_eq!(item, 3);
      assert_eq!(new_list.iter().collect::<Vec<_>>(), vec![1, 2, 4, 5]);
      assert_eq!(list.iter().collect::<Vec<_>>(), vec![1, 2, 3, 4, 5]);
      assert_eq!(list.len(), 5);
      assert_eq!(new_list.len(), 4);
    });
  }

  #[test]
  fn remove_out_of_range_returns_none() {
    rootless_mutate(|mc| {
      let list = List::from_vec(mc, vec![1, 2, 3]);
      assert_eq!(list.remove(mc, 3), None);
      assert_eq!(list.remove(mc, 100), None);
    });
  }

  #[test]
  fn remove_across_chunk_and_branch_boundaries() {
    rootless_mutate(|mc| {
      let items: Vec<usize> = (0..1_000).collect();
      let list = List::from_vec(mc, items.clone());

      let (item, removed) = list.remove(mc, 65).expect("index in range");
      assert_eq!(item, 65);
      let mut expected = items.clone();
      expected.remove(65);
      assert_eq!(removed.iter().collect::<Vec<_>>(), expected);
      assert_eq!(removed.len(), 999);

      let (item, removed) = removed.remove(mc, 0).expect("index in range");
      assert_eq!(item, 0);
      assert_eq!(removed.get(0), Some(1));
      assert_eq!(removed.len(), 998);

      let (item, removed) = removed.remove(mc, 997).expect("index in range");
      assert_eq!(item, 999);
      assert_eq!(removed.len(), 997);
    });
  }

  #[test]
  fn remove_last_element_leaves_empty_list() {
    rootless_mutate(|mc| {
      let list = List::from_vec(mc, vec![42]);
      let (item, new_list) = list.remove(mc, 0).expect("index in range");
      assert_eq!(item, 42);
      assert!(new_list.is_empty());
      assert_eq!(new_list.iter().collect::<Vec<_>>(), vec![]);
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
