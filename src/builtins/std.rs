//! The standard SafeLisp builtin library.

use gc_arena::{Gc, RefLock};

use crate::interpreter::{CellContents, HostCtx, HostPoll, MemoryReservation, SLVal, Value};
use crate::types::Signature;

use super::{sig, Builtin, Library, Trait};

/// Construct the standard builtin library.
pub fn library() -> Library {
  Library::new()
    .with_builtin(Builtin::binary(
      "std",
      "+",
      sig(
        &[("A", &[Trait::Add])],
        vec![Signature::var("A"), Signature::var("A")],
        None,
        Signature::var("A"),
      ),
      add,
    ))
    .with_builtin(Builtin::binary(
      "std",
      "-",
      sig(
        &[("A", &[Trait::Sub])],
        vec![Signature::var("A"), Signature::var("A")],
        None,
        Signature::var("A"),
      ),
      subtract,
    ))
    .with_builtin(Builtin::binary(
      "std",
      "==",
      sig(
        &[("A", &[Trait::Eq])],
        vec![Signature::var("A"), Signature::var("A")],
        None,
        Signature::Bool,
      ),
      equal,
    ))
    .with_builtin(Builtin::contextual(
      "std",
      "concat",
      Some(2),
      sig(
        &[("A", &[Trait::Concat])],
        vec![Signature::var("A"), Signature::var("A")],
        None,
        Signature::var("A"),
      ),
      concat,
    ))
    .with_builtin(Builtin::contextual(
      "std",
      "list",
      None,
      sig(
        &[("A", &[])],
        vec![],
        Some(Signature::var("A")),
        Signature::list(Signature::var("A")),
      ),
      list,
    ))
    .with_builtin(Builtin::contextual_value(
      "std",
      "cell",
      Some(1),
      sig(
        &[("A", &[])],
        vec![Signature::var("A")],
        None,
        Signature::cell(Signature::var("A")),
      ),
      cell,
    ))
    .with_builtin(Builtin::contextual_value(
      "std",
      "get",
      Some(1),
      sig(
        &[("A", &[])],
        vec![Signature::cell(Signature::var("A"))],
        None,
        Signature::var("A"),
      ),
      get,
    ))
    .with_builtin(Builtin::contextual_value(
      "std",
      "set!",
      Some(2),
      sig(
        &[("A", &[])],
        vec![Signature::cell(Signature::var("A")), Signature::var("A")],
        None,
        Signature::Void,
      ),
      set,
    ))
    .with_builtin(Builtin::unary(
      "std",
      "len",
      sig(
        &[("A", &[])],
        vec![Signature::list(Signature::var("A"))],
        None,
        Signature::Int,
      ),
      len,
    ))
    .with_builtin(Builtin::contextual_value(
      "std",
      "idx",
      Some(2),
      sig(
        &[("A", &[])],
        vec![Signature::list(Signature::var("A")), Signature::Int],
        None,
        Signature::var("A"),
      ),
      idx,
    ))
    .with_builtin(Builtin::contextual(
      "std",
      "push",
      Some(2),
      sig(
        &[("A", &[])],
        vec![Signature::list(Signature::var("A")), Signature::var("A")],
        None,
        Signature::list(Signature::var("A")),
      ),
      push,
    ))
    // (std::range start stop) -> List<Int>
    //   Like Python's `list(range(start, stop))`: half-open, `[start, stop)`.
    //   `start >= stop` yields the empty list.
    .with_builtin(Builtin::contextual(
      "std",
      "range",
      Some(2),
      sig(
        &[],
        vec![Signature::Int, Signature::Int],
        None,
        Signature::list(Signature::Int),
      ),
      range,
    ))
    // (std::map list fn) -> List
    //   Applies `fn` (a callable value: FunctionRef or Partial) to each element
    //   of `list` and collects the results into a new list. Implemented as a
    //   builtin with [`HostCtx`] access so callback execution is scheduled
    //   through the ordinary resumable interpreter loop.
    .with_builtin(Builtin::resumable(
      "std",
      "map",
      Some(2),
      sig(
        &[("A", &[]), ("B", &[])],
        vec![
          Signature::list(Signature::var("A")),
          Signature::function(vec![Signature::var("A")], Signature::var("B")),
        ],
        None,
        Signature::list(Signature::var("B")),
      ),
      map_start,
      map_resume,
    ))
    .with_builtin(Builtin::contextual(
      "std",
      "slice",
      Some(3),
      sig(
        &[("A", &[Trait::Slice])],
        vec![Signature::var("A"), Signature::Int, Signature::Int],
        None,
        Signature::var("A"),
      ),
      slice,
    ))
}

fn map_start<'gc, 'call>(ctx: &mut HostCtx<'gc, 'call>, args: &[Value<'gc>]) -> Result<(), String> {
  let (list, func) = (args[0], args[1]);
  match &list {
    Value::Heap(heap) if matches!(&heap.value, SLVal::List(_)) => {}
    _ => {
      return Err(format!(
        "std::map: expected a List, got {}",
        list.type_name()
      ))
    }
  }
  let results = ctx.empty_list();
  let results = ctx.alloc_heap(SLVal::List(results));
  ctx.push(list);
  ctx.push(func);
  ctx.push(Value::Int(0)); // index we're currently working on
  ctx.push(results);
  Ok(())
}

fn pop_map_state<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
) -> Result<(Value<'gc>, Value<'gc>, usize, Value<'gc>), String> {
  let results = ctx.pop()?;
  if !matches!(results, Value::Heap(heap) if matches!(&heap.value, SLVal::List(_))) {
    return Err(format!(
      "std::map: expected result List, got {}",
      results.type_name()
    ));
  }
  let index = match ctx.pop()? {
    Value::Int(value) if value >= 0 => {
      usize::try_from(value).map_err(|_| "std::map: index does not fit usize".to_string())?
    }
    other => {
      return Err(format!(
        "std::map: expected index Int, got {}",
        other.type_name()
      ))
    }
  };
  let func = ctx.pop()?;
  let source_list = ctx.pop()?;
  Ok((source_list, func, index, results))
}

fn push_map_state<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  source_list: Value<'gc>,
  func: Value<'gc>,
  index: usize,
  results: Value<'gc>,
) -> Result<(), String> {
  let index = i64::try_from(index).map_err(|_| "std::map: index overflow".to_string())?;
  ctx.push(source_list);
  ctx.push(func);
  ctx.push(Value::Int(index));
  ctx.push(results);
  Ok(())
}

fn append_map_result<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  results: Value<'gc>,
  value: Value<'gc>,
) -> Result<Value<'gc>, String> {
  let appended = match results {
    Value::Heap(heap) => match &heap.value {
      SLVal::List(items) => items.append(ctx.mc(), value),
      _ => {
        return Err(format!(
          "std::map: expected result List, got {}",
          results.type_name()
        ))
      }
    },
    _ => {
      return Err(format!(
        "std::map: expected result List, got {}",
        results.type_name()
      ))
    }
  };
  Ok(ctx.alloc_heap(SLVal::List(appended)))
}

fn map_resume<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  pending_result: Option<Value<'gc>>,
) -> Result<HostPoll<'gc>, String> {
  if let Some(callback_result) = pending_result {
    let (source_list, func, index, results) = pop_map_state(ctx)?;
    let next_index = index
      .checked_add(1)
      .ok_or_else(|| "std::map: index overflow".to_string())?;
    let results = append_map_result(ctx, results, callback_result)?;
    push_map_state(ctx, source_list, func, next_index, results)?;
    return Ok(HostPoll::Pending);
  }

  let (source_list, func, index, results) = pop_map_state(ctx)?;
  let (len, item) = match &source_list {
    Value::Heap(heap) => match &heap.value {
      SLVal::List(items) => {
        let item = items.get(index);
        (items.len(), item)
      }
      _ => {
        return Err(format!(
          "std::map: expected a List, got {}",
          source_list.type_name()
        ))
      }
    },
    _ => {
      return Err(format!(
        "std::map: expected a List, got {}",
        source_list.type_name()
      ))
    }
  };

  if index == len {
    Ok(HostPoll::Ready(results))
  } else {
    let item = item
      .ok_or_else(|| format!("std::map: index {index} out of range for list of length {len}"))?;
    push_map_state(ctx, source_list, func, index, results)?;
    ctx.call(func, &[item])?;
    Ok(HostPoll::Pending)
  }
}

fn add<'gc>(a: Value<'gc>, b: Value<'gc>) -> Result<Value<'gc>, String> {
  match (a, b) {
    (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
    (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
    _ => Err(format!(
      "Couldn't add {} and {}",
      a.type_name(),
      b.type_name()
    )),
  }
}

fn subtract<'gc>(a: Value<'gc>, b: Value<'gc>) -> Result<Value<'gc>, String> {
  match (a, b) {
    // `a` is the left operand, `b` is the right operand: left - right.
    (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x - y)),
    (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
    _ => Err(format!(
      "Couldn't sub {} and {}",
      a.type_name(),
      b.type_name()
    )),
  }
}

fn equal<'gc>(a: Value<'gc>, b: Value<'gc>) -> Result<Value<'gc>, String> {
  Ok(Value::Bool(a == b))
}

fn concat<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<SLVal<'gc>, String> {
  let (a, b) = (args[0], args[1]);
  match (&a, &b) {
    (Value::Heap(a), Value::Heap(b)) => match (&a.value, &b.value) {
      (SLVal::String(x), SLVal::String(y)) => {
        let len = x
          .len()
          .checked_add(y.len())
          .ok_or_else(|| "concat: string length overflow".to_string())?;
        let (mut combined, _reservation) = reserved_string(ctx, len, "concat")?;
        combined.push_str(x);
        combined.push_str(y);
        Ok(SLVal::String(combined))
      }
      (SLVal::List(x), SLVal::List(y)) => {
        let len = x
          .len()
          .checked_add(y.len())
          .ok_or_else(|| "concat: list length overflow".to_string())?;
        let _reservation = reserve_value_slots(ctx, len, "concat")?;
        Ok(SLVal::List(x.concat(ctx.mc(), y)))
      }
      _ => Err(format!(
        "Couldn't concat {} and {}",
        a.value.type_name(),
        b.value.type_name()
      )),
    },
    _ => Err(format!(
      "Couldn't concat {} and {}",
      a.type_name(),
      b.type_name()
    )),
  }
}

fn list<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<SLVal<'gc>, String> {
  Ok(SLVal::List(ctx.list_from_iter(args.iter().copied())?))
}

fn cell<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<Value<'gc>, String> {
  let contents = CellContents::new(args[0]);
  Ok(Value::Cell(Gc::new(ctx.mc(), RefLock::new(contents))))
}

fn get<'gc, 'call>(
  _ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<Value<'gc>, String> {
  match args[0] {
    Value::Cell(cell) => Ok(cell.borrow().value),
    other => Err(format!(
      "std::get: expected a Cell, got {}",
      other.type_name()
    )),
  }
}

fn set<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<Value<'gc>, String> {
  match args[0] {
    Value::Cell(cell) => {
      Gc::write(ctx.mc(), cell).unlock().borrow_mut().set(args[1]);
      Ok(Value::Void)
    }
    other => Err(format!(
      "std::set!: expected a Cell, got {}",
      other.type_name()
    )),
  }
}

fn len<'gc>(value: Value<'gc>) -> Result<Value<'gc>, String> {
  match value {
    Value::Heap(heap) => match &heap.value {
      SLVal::List(items) => Ok(Value::Int(items.len() as i64)),
      _ => Err(format!("len: expected a List, got {}", value.type_name())),
    },
    _ => Err(format!("len: expected a List, got {}", value.type_name())),
  }
}

fn idx<'gc, 'call>(
  _ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<Value<'gc>, String> {
  let (list, index_value) = (args[0], args[1]);
  match (&list, &index_value) {
    (Value::Heap(list), Value::Int(index)) => match &list.value {
      SLVal::List(items) => {
        let len = items.len() as i64;
        let normalized = if *index < 0 { *index + len } else { *index };
        if normalized < 0 || normalized >= len {
          Err(format!(
            "idx: index {} out of range for list of length {}",
            index, len
          ))
        } else {
          items
            .get(normalized as usize)
            .ok_or_else(|| "idx: normalized index is out of range".to_string())
        }
      }
      _ => Err(format!(
        "idx: expected (List, Int), got ({}, {})",
        list.value.type_name(),
        index_value.type_name()
      )),
    },
    _ => Err(format!(
      "idx: expected (List, Int), got ({}, {})",
      list.type_name(),
      index_value.type_name()
    )),
  }
}

fn push<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<SLVal<'gc>, String> {
  let (list, value) = (args[0], args[1]);
  match &list {
    Value::Heap(heap) => match &heap.value {
      SLVal::List(items) => {
        let len = items
          .len()
          .checked_add(1)
          .ok_or_else(|| "push: list length overflow".to_string())?;
        let _reservation = reserve_value_slots(ctx, len, "push")?;
        Ok(SLVal::List(items.append(ctx.mc(), value)))
      }
      _ => Err(format!("push: expected a List, got {}", list.type_name())),
    },
    _ => Err(format!("push: expected a List, got {}", list.type_name())),
  }
}

fn range<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<SLVal<'gc>, String> {
  let (start, stop) = (args[0], args[1]);
  match (start, stop) {
    (Value::Int(start), Value::Int(stop)) => {
      if stop <= start {
        Ok(SLVal::List(ctx.empty_list()))
      } else {
        let values = (start..stop).map(Value::Int);
        Ok(SLVal::List(ctx.list_from_iter(values)?))
      }
    }
    _ => Err(format!(
      "range: expected (Int, Int), got ({}, {})",
      start.type_name(),
      stop.type_name()
    )),
  }
}

fn slice<'gc, 'call>(
  ctx: &mut HostCtx<'gc, 'call>,
  args: &[Value<'gc>],
) -> Result<SLVal<'gc>, String> {
  let (value, start_value, stop_value) = (args[0], args[1], args[2]);
  match (&value, &start_value, &stop_value) {
    (Value::Heap(heap), Value::Int(start), Value::Int(stop)) => match &heap.value {
      SLVal::List(items) => {
        let len = items.len() as i64;
        let start = norm_index(*start, len).clamp(0, len);
        let stop = norm_index(*stop, len).clamp(0, len);
        if start >= stop {
          Ok(SLVal::List(ctx.empty_list()))
        } else {
          let result_len = (stop - start) as usize;
          let values = items.iter().skip(start as usize).take(result_len);
          Ok(SLVal::List(ctx.list_from_iter(values)?))
        }
      }
      SLVal::String(string) => {
        let len = string.chars().count() as i64;
        let start = norm_index(*start, len).clamp(0, len) as usize;
        let stop = norm_index(*stop, len).clamp(0, len) as usize;
        if start >= stop {
          Ok(SLVal::String(String::new()))
        } else {
          let start_byte = char_boundary(string, start);
          let end_byte = char_boundary(string, stop);
          let source = &string[start_byte..end_byte];
          let (mut result, _reservation) = reserved_string(ctx, source.len(), "slice")?;
          result.push_str(source);
          Ok(SLVal::String(result))
        }
      }
      _ => Err(format!(
        "slice: expected (List, Int, Int) or (String, Int, Int), got ({}, {}, {})",
        value.type_name(),
        start_value.type_name(),
        stop_value.type_name()
      )),
    },
    _ => Err(format!(
      "slice: expected (List, Int, Int) or (String, Int, Int), got ({}, {}, {})",
      value.type_name(),
      start_value.type_name(),
      stop_value.type_name()
    )),
  }
}

/// Return the UTF-8 byte offset for a character boundary. `char_index` may
/// equal the character count, in which case this returns `value.len()`.
fn char_boundary(value: &str, char_index: usize) -> usize {
  value
    .char_indices()
    .nth(char_index)
    .map_or(value.len(), |(byte_index, _)| byte_index)
}

/// Normalize a possibly-negative index into a non-negative clamped index, using
/// Python's `l[start:stop]` semantics: a negative index counts from the end.
/// The result is *not* clamped to `[0, len]`; the caller does that after both
/// bounds are normalized (so `l[2:100]` works on a length-3 list).
fn norm_index(i: i64, len: i64) -> i64 {
  if i < 0 {
    i + len
  } else {
    i
  }
}

fn reserve_value_slots(
  ctx: &HostCtx<'_, '_>,
  len: usize,
  operation: &str,
) -> Result<MemoryReservation, String> {
  let requested_bytes = len
    .checked_mul(::std::mem::size_of::<Value<'static>>())
    .ok_or_else(|| format!("{operation}: allocation size overflow"))?;
  ctx.reserve_memory(requested_bytes)
}

fn reserved_string(
  ctx: &HostCtx<'_, '_>,
  len: usize,
  operation: &str,
) -> Result<(String, MemoryReservation), String> {
  let mut reservation = ctx.reserve_memory(len)?;
  let mut value = String::new();
  value
    .try_reserve_exact(len)
    .map_err(|_| format!("{operation}: failed to allocate {len} string bytes"))?;
  ctx.reconcile_reservation(&mut reservation, value.capacity())?;
  Ok((value, reservation))
}
