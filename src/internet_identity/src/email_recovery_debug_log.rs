//! TEMPORARY (PR #3855): heap-only ring buffer for tracing the
//! email-recovery + DoH flow on staging. Exposed via the
//! `email_recovery_debug_logs` query — *anonymous, no authentication*
//! — so the debug surface is reachable without staging credentials.
//! Contents are lost on every upgrade and capped at [`MAX_LINES`] so
//! the heap can't grow without bound.
//!
//! Remove this module along with its query and all `er_dbg!`
//! call-sites once the staging outlook.com flow is confirmed working
//! end-to-end.
use std::cell::RefCell;
use std::collections::VecDeque;

const MAX_LINES: usize = 2000;

thread_local! {
    static LOG: RefCell<VecDeque<String>> = const { RefCell::new(VecDeque::new()) };
}

#[cfg(not(test))]
fn now_ms() -> u64 {
    ic_cdk::api::time() / 1_000_000
}

#[cfg(test)]
fn now_ms() -> u64 {
    0
}

/// Append one line. The canister wall-clock (ms since epoch) is
/// prepended so ordering is recoverable even when multiple flows
/// interleave their entries.
pub fn push(line: String) {
    LOG.with(|l| {
        let mut buf = l.borrow_mut();
        if buf.len() == MAX_LINES {
            buf.pop_front();
        }
        buf.push_back(format!("[{}ms] {}", now_ms(), line));
    });
}

pub fn snapshot() -> Vec<String> {
    LOG.with(|l| l.borrow().iter().cloned().collect())
}

pub fn clear() {
    LOG.with(|l| l.borrow_mut().clear());
}

/// Convenience macro: `er_dbg!("doh.fetch.start name={}", name);`
#[macro_export]
macro_rules! er_dbg {
    ($($arg:tt)*) => {
        $crate::email_recovery_debug_log::push(format!($($arg)*));
    };
}
