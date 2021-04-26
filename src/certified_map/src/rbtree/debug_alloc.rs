use std::cell::RefCell;
use std::collections::HashSet;

thread_local! {
    static ALLOCATED_POINTERS: RefCell<HashSet<*const ()>> = RefCell::new(HashSet::new());
}

/// Marks the specified pointer as being reachable.
/// Should be invoked right after the memory is allocated.
pub fn mark_pointer_allocated<T>(ptr: *const T) {
    ALLOCATED_POINTERS.with(move |ptrs| {
        assert!(ptrs.borrow_mut().insert(ptr as *const ()));
    })
}

/// Marks the specified pointer as deleted.
/// Should be invoked right after the pointer is freed.
pub fn mark_pointer_deleted<T>(ptr: *const T) {
    ALLOCATED_POINTERS.with(move |ptrs| {
        assert!(
            ptrs.borrow_mut().remove(&(ptr as *const ())),
            "DOUBLE FREE: deleted pointer {:?} that is not allocated",
            ptr
        );
    })
}

/// Returns the number of pointer that were allocated and not yet
/// deleted.
pub fn count_allocated_pointers() -> usize {
    ALLOCATED_POINTERS.with(|ptrs| ptrs.borrow().len())
}

/// Returns true if the specified pointer was allocated and not yet
/// deleted.
pub fn is_live<T>(ptr: *const T) -> bool {
    ALLOCATED_POINTERS.with(move |ptrs| ptrs.borrow().contains(&(ptr as *const ())))
}
