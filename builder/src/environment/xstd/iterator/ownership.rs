use std::{collections::VecDeque, mem};
use silex_types::{RuntimeOpaqueWrapper, RuntimeValue, StackValue};
use super::iter::{BaseSource, PipeStep, XIterator};

impl Clone for XIterator {
    fn clone(&self) -> Self {
        enum Work<'a> {
            Iterator(&'a XIterator),
            Stack(&'a StackValue),
            Build(&'a XIterator),
            Wrap,
        }
        enum Copy {
            Iterator(XIterator),
            Stack(StackValue),
        }
        impl Copy {
            fn iterator(self) -> XIterator {
                match self {
                    Self::Iterator(value) => value,
                    Self::Stack(_) => unreachable!("iterator clone field"),
                }
            }

            fn stack(self) -> StackValue {
                match self {
                    Self::Stack(value) => value,
                    Self::Iterator(_) => unreachable!("stack clone field"),
                }
            }
        }
        let mut work = vec![Work::Iterator(self)];
        let mut copies = Vec::new();
        while let Some(item) = work.pop() {
            match item {
                Work::Iterator(iter) => {
                    work.push(Work::Build(iter));
                    for step in iter.pipe.iter().rev() {
                        match step {
                            PipeStep::Map(value) | PipeStep::Filter(value) => work.push(Work::Stack(value)),
                            PipeStep::Chain(child) | PipeStep::Zip(child) => work.push(Work::Iterator(child)),
                            _ => {}
                        }
                    }
                    if let BaseSource::Unfold { closure, state } = &iter.source {
                        if let Some(state) = state {
                            work.push(Work::Stack(state));
                        }
                        work.push(Work::Stack(closure));
                    }
                }
                Work::Stack(value) => {
                    if let StackValue::Owned(RuntimeValue::Opaque(value)) = value {
                        if let Ok(iter) = value.as_ref::<XIterator>() {
                            work.push(Work::Wrap);
                            work.push(Work::Iterator(iter));
                            continue;
                        }
                    }
                    copies.push(Copy::Stack(value.clone()));
                }
                Work::Wrap => {
                    let iter = copies.pop().expect("cloned inline iterator").iterator();
                    copies.push(Copy::Stack(RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(iter)).into()));
                }
                Work::Build(iter) => {
                    let source_fields = match &iter.source {
                        BaseSource::Unfold { state, .. } => 1 + usize::from(state.is_some()),
                        _ => 0,
                    };
                    let fields = source_fields + iter.pipe.iter().filter(|step| matches!(step,
                        PipeStep::Map(_) | PipeStep::Filter(_) | PipeStep::Chain(_) | PipeStep::Zip(_)
                    )).count();
                    let copy = {
                        let mut fields = copies.drain(copies.len() - fields..);
                        let source = match &iter.source {
                            BaseSource::Dequeue(items) => BaseSource::Dequeue(items.clone()),
                            BaseSource::Once(item) => BaseSource::Once(item.clone()),
                            BaseSource::Empty => BaseSource::Empty,
                            BaseSource::Unfold { state, .. } => BaseSource::Unfold {
                                closure: fields.next().expect("cloned unfold closure").stack(),
                                state: state.as_ref().map(|_| fields.next().expect("cloned unfold state").stack()),
                            },
                        };
                        let pipe = iter.pipe.iter().map(|step| match step {
                            PipeStep::Map(_) => PipeStep::Map(fields.next().expect("cloned mapper").stack()),
                            PipeStep::Filter(_) => PipeStep::Filter(fields.next().expect("cloned predicate").stack()),
                            PipeStep::Chain(_) => PipeStep::Chain(Box::new(fields.next().expect("cloned chain").iterator())),
                            PipeStep::Zip(_) => PipeStep::Zip(Box::new(fields.next().expect("cloned zip").iterator())),
                            step => step.clone(),
                        }).collect::<VecDeque<_>>();
                        Self { source, pipe }
                    };
                    copies.push(Copy::Iterator(copy));
                }
            }
        }
        copies.pop().expect("cloned iterator root").iterator()
    }
}

impl Drop for XIterator {
    fn drop(&mut self) {
        fn drain_stack(value: StackValue, work: &mut Vec<XIterator>) {
            if let StackValue::Owned(RuntimeValue::Opaque(value)) = value {
                if let Ok(iter) = value.into_inner::<XIterator>() {
                    work.push(iter);
                }
            }
        }

        fn drain(iter: &mut XIterator, work: &mut Vec<XIterator>) {
            if let BaseSource::Unfold { closure, state } = mem::take(&mut iter.source) {
                drain_stack(closure, work);
                if let Some(state) = state {
                    drain_stack(state, work);
                }
            }
            for step in iter.pipe.drain(..) {
                match step {
                    PipeStep::Chain(child) | PipeStep::Zip(child) => work.push(*child),
                    PipeStep::Map(value) | PipeStep::Filter(value) => drain_stack(value, work),
                    _ => {}
                }
            }
        }

        let mut work = Vec::new();
        drain(self, &mut work);
        while let Some(mut iter) = work.pop() {
            drain(&mut iter, &mut work);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread::Builder;
    use silex_environment::SysCallResult;
    use silex_types::{Heap, Primitive, PrimitiveInteger};
    use super::super::terminal::{lazy_collect, TerminalOp};

    const DEPTH: usize = if cfg!(miri) { 32 } else { 4096 };

    fn small_stack(operation: impl FnOnce() + Send + 'static) {
        Builder::new().stack_size(256 * 1024).spawn(operation).unwrap().join().unwrap();
    }

    #[test]
    fn deeply_nested_boxed_iterators_clone_detach_and_drop_iteratively() {
        small_stack(|| {
            let mut heap = unsafe { Heap::new() };
            {
                let mut iter = XIterator::once(PrimitiveInteger::U64(42).into(), &mut heap).unwrap();
                for _ in 0..DEPTH {
                    let mut parent = XIterator::empty();
                    parent.push_step(PipeStep::Chain(Box::new(iter))).unwrap();
                    iter = parent;
                }
                let mut copy = iter.clone();
                assert_eq!(copy.next_sync(&mut heap).unwrap().unwrap().as_ref(&heap).as_u64().unwrap(), 42);
                let value = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(iter));
                let detached = heap.detach(value).unwrap();
                let mut iter = detached.into_opaque_type::<XIterator>().unwrap();
                assert_eq!(iter.next_sync(&mut heap).unwrap().unwrap().as_ref(&heap).as_u64().unwrap(), 42);
            }
            assert!(heap.is_empty());
        });
    }

    #[test]
    fn deeply_nested_inline_iterators_clone_detach_and_drop_iteratively() {
        small_stack(|| {
            let mut heap = unsafe { Heap::new() };
            {
                let mut iter = XIterator::empty();
                for _ in 0..DEPTH {
                    iter = XIterator {
                        source: BaseSource::Unfold {
                            closure: Primitive::Null.into(),
                            state: Some(RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(iter)).into()),
                        },
                        pipe: VecDeque::new(),
                    };
                }
                let copy = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(iter.clone()));
                let value = heap.detach(copy).unwrap();
                assert!(value.as_opaque_type::<XIterator>().is_ok());
                assert!(!format!("{iter:?}").is_empty());
            }
            assert!(heap.is_empty());
        });
    }

    #[test]
    fn mixed_heap_and_opaque_edges_detach_iteratively() {
        small_stack(|| {
            let mut heap = unsafe { Heap::new() };
            {
                let mut value = RuntimeValue::from(PrimitiveInteger::U64(42));
                for _ in 0..DEPTH {
                    value = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(XIterator::once(value, &mut heap).unwrap()));
                }
                let copy = heap.detach(value.clone()).unwrap();
                assert!(copy.as_opaque_type::<XIterator>().is_ok());
            }
            assert!(heap.is_empty());
        });
    }

    #[test]
    fn deeply_nested_opaque_flatten_uses_the_explicit_iterator_stack() {
        small_stack(|| {
            let mut heap = unsafe { Heap::new() };
            {
                let mut iter = XIterator::once(PrimitiveInteger::U64(42).into(), &mut heap).unwrap();
                for _ in 0..DEPTH {
                    let value = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(iter));
                    iter = XIterator::once(value, &mut heap).unwrap();
                    iter.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
                }
                assert_eq!(iter.next_sync(&mut heap).unwrap().unwrap().as_ref(&heap).as_u64().unwrap(), 42);
                assert!(iter.next_sync(&mut heap).unwrap().is_none());
            }
            assert!(heap.is_empty());
        });
    }

    #[test]
    fn empty_callback_pipelines_do_not_recurse_through_collection_continuations() {
        small_stack(|| {
            let mut heap = unsafe { Heap::new() };
            {
                let mut iter = XIterator::empty();
                iter.push_step(PipeStep::Map(Primitive::Null.into())).unwrap();
                for _ in 0..DEPTH {
                    let mut parent = XIterator::empty();
                    parent.push_step(PipeStep::Chain(Box::new(iter))).unwrap();
                    iter = parent;
                }
                let result = lazy_collect::<()>(&mut iter, VecDeque::new(), TerminalOp::Count, &mut heap).unwrap();
                let SysCallResult::Return(value) = result else {
                    panic!("expected count");
                };
                assert_eq!(value.as_u32(&heap).unwrap(), 0);
            }
            assert!(heap.is_empty());
        });
    }
}

#[cfg(test)]
mod error_tests {
    use super::*;
    use silex_types::{Heap, PrimitiveInteger};

    #[test]
    fn an_error_restores_the_parent_and_remaining_child_state() {
        let mut heap = unsafe { Heap::new() };
        {
            let invalid = heap.insert(PrimitiveInteger::U64(1).into());
            let item = heap.insert(PrimitiveInteger::U64(42).into());
            let valid = heap.insert(RuntimeValue::Object(vec![item]));
            let mut child = XIterator::from_dequeue(VecDeque::from([invalid, valid]));
            child.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
            let mut parent = XIterator::empty();
            parent.push_step(PipeStep::Chain(Box::new(child))).unwrap();
            assert!(parent.next_sync(&mut heap).is_err());
            assert_eq!(parent.next_sync(&mut heap).unwrap().unwrap().as_ref(&heap).as_u64().unwrap(), 42);
            assert!(parent.next_sync(&mut heap).unwrap().is_none());
        }
        assert!(heap.is_empty());
    }
}
