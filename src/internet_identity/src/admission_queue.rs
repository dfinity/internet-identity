//! A bounded queue shared by several senders. Each sender has a capacity limit
//! and gets a turn when items are taken.
//!
//! [`AdmissionQueue::admit`] removes the submitting sender's expired entries,
//! checks for duplicates and the group limit, then checks available capacity.
//! Each item is stored, folded into a queued duplicate, dropped for a full group,
//! or rejected with a retry delay.
//!
//! [`AdmissionQueue::take_batch`] takes one item per sender in turn. Priorities
//! take turns within a sender too: priority `p` of `n` runs for `n - p` turns before
//! the next one runs, so a busy high priority does not starve the rest. Within a
//! priority the nearest deadline goes first, and the item key breaks ties.
//!
//! Callers must supply trusted canister time as `now_ns`. An item may name its own
//! expiry, which the queue clamps to `discard_entries_after_ns` after admission, so
//! a sender can shorten an item's life but never extend it.
//! The queue forgets entries after taking them, so duplicate detection only covers
//! work still in this queue. A retry delay describes queue pressure, not delivery.
//!
//! This module does not authorize senders or deliver items.
// The submission endpoint and dispatcher will use this in later PRs.
#![allow(dead_code)]

use internet_identity_interface::internet_identity::types::Timestamp;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::hash::Hash;
use std::ops::Bound::{Excluded, Unbounded};

/// The key, group, and priority of a queued item.
pub(crate) trait QueueItem: Clone {
    /// Identifies an item for one sender. A duplicate key is not stored again.
    type Key: Clone + Eq + Hash + Ord;

    /// Identifies items that share a pending-item limit for one sender.
    type Group: Copy + Eq + Hash;

    /// Number of priority levels. Zero is treated as one.
    const PRIORITY_LEVELS: usize;

    fn key(&self) -> Self::Key;
    fn group(&self) -> Self::Group;

    /// Processing priority among this sender's items. Lower values are taken first
    /// and get more turns. Out-of-range values use the lowest priority.
    fn priority(&self) -> usize;

    /// When the item stops being worth delivering, as chosen by the submitter.
    /// `None` leaves the queue's own limit in charge.
    fn expires_at_ns(&self) -> Option<Timestamp> {
        None
    }
}

/// Consecutive turns a priority gets before the next one runs. Priority `p` of `n`
/// gets `n - p`, so the highest gets the most turns and the lowest still gets one.
const fn turns_at_priority(priority: usize, levels: usize) -> usize {
    if levels > priority {
        levels - priority
    } else {
        1
    }
}

/// Capacity limits, expiry time, and retry delays.
#[derive(Clone, Debug)]
pub(crate) struct QueueConfig {
    /// Maximum number of entries admitted across all senders. Callers must also
    /// bound item size if they need a memory limit.
    pub(crate) max_entries: usize,
    /// Maximum entries for one sender. Its limit may be lower when the queue is
    /// shared by several senders.
    pub(crate) max_entries_per_sender: usize,
    /// Maximum pending entries per group for one sender. Further items are dropped.
    pub(crate) max_pending_per_group: usize,
    /// Clear the pressure timer when occupancy falls below this value. A value
    /// below `max_entries` avoids resetting the timer whenever one slot opens.
    pub(crate) pressure_cleared_below: usize,
    /// Longest an entry may wait, measured from admission. An item's own expiry
    /// can only shorten this.
    pub(crate) discard_entries_after_ns: u64,
    pub(crate) retry: RetryPolicy,
}

impl QueueConfig {
    /// Checks the limits and time intervals. Also usable in a compile-time assertion.
    pub(crate) const fn is_coherent(&self) -> bool {
        self.max_entries > 0
            && self.max_entries_per_sender > 0
            && self.max_pending_per_group > 0
            && self.max_entries_per_sender <= self.max_entries
            && self.pressure_cleared_below > 0
            && self.pressure_cleared_below <= self.max_entries
            && self.discard_entries_after_ns > 0
            && self.retry.is_coherent()
    }
}

/// Retry delays returned when the queue or a sender reaches its limit.
#[derive(Clone, Debug)]
pub(crate) struct RetryPolicy {
    /// Initial retry delay.
    pub(crate) base_ms: u32,
    /// Maximum retry delay while entries are still being taken.
    pub(crate) ceiling_ms: u32,
    /// Longer retry delay when entries are waiting but none have been taken for
    /// `stalled_after_silence_ns`.
    pub(crate) when_stalled_ms: u32,
    /// Double the delay after each interval of queue pressure.
    pub(crate) doubles_every_ns: u64,
    /// Maximum number of doublings before hitting the ceiling.
    pub(crate) max_doublings: u32,
    /// How long entries may wait without a successful take before declaring a stall.
    pub(crate) stalled_after_silence_ns: u64,
}

impl RetryPolicy {
    const fn is_coherent(&self) -> bool {
        self.base_ms > 0
            && self.base_ms <= self.ceiling_ms
            && self.ceiling_ms <= self.when_stalled_ms
            && self.doubles_every_ns > 0
            && self.stalled_after_silence_ns > 0
            && self.max_doublings > 0
    }
}

/// What the queue did with one item.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Admission {
    /// Stored as a new entry.
    Accepted,
    /// Not stored because its key is already queued. The queued entry stands for it.
    Folded,
    /// Not stored and not delivered: the item had already expired, or its group is
    /// at its limit. Retrying repeats the outcome until the group drains.
    Dropped,
    /// Not stored because the queue or sender is full. Retry after this delay.
    Full { retry_after_ms: u32 },
}

/// One item's outcome, labelled with that item's key.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Admitted<Key> {
    pub(crate) key: Key,
    pub(crate) admission: Admission,
}

/// An item, when it was admitted, and when it stops being worth delivering.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Entry<Item> {
    pub(crate) received_at_ns: Timestamp,
    pub(crate) expires_at_ns: Timestamp,
    pub(crate) item: Item,
}

/// An entry removed from the queue, with its sender.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Taken<Sender, Item> {
    pub(crate) sender: Sender,
    pub(crate) entry: Entry<Item>,
}

/// One sender's saved entries.
#[derive(Clone, Debug, Eq, PartialEq)]
struct SenderSnapshot<Sender, Item> {
    sender: Sender,
    entries: Vec<Entry<Item>>,
    serving_priority: usize,
    turns_left: usize,
}

/// Saved entries and the next sender to serve. Private fields keep callers from
/// constructing snapshots with duplicate keys or inconsistent entries.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct QueueSnapshot<Sender, Item> {
    senders: Vec<SenderSnapshot<Sender, Item>>,
    next_sender: Option<Sender>,
}

/// Queue counts and timings for metrics. Expired entries count until removed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct QueueStats {
    pub(crate) stored_total: usize,
    pub(crate) active_senders: usize,
    /// Time until the nearest deadline among stored entries.
    pub(crate) next_expiry_in_ns: Option<u64>,
    /// Time since the queue filled, until occupancy falls below the reset threshold.
    pub(crate) at_capacity_for_ns: Option<u64>,
    /// Time since the last successful take, or since construction or restore.
    pub(crate) silence_ns: u64,
    pub(crate) discarded_expired: u64,
    pub(crate) folded_duplicates: u64,
    pub(crate) dropped_group_capped: u64,
    pub(crate) dropped_already_expired: u64,
}

/// One priority's entries, ordered by deadline, then key.
type PriorityBucket<Item> = BTreeMap<(Timestamp, <Item as QueueItem>::Key), Entry<Item>>;

/// One sender's entries, indexed for ordering, duplicate checks, and group limits.
struct SenderQueue<Item: QueueItem> {
    entries_by_priority: Vec<PriorityBucket<Item>>,
    /// Keys currently queued for this sender.
    present: HashSet<Item::Key>,
    pending_per_group: HashMap<Item::Group, usize>,
    /// Priority being served, and turns left before the next one is served.
    serving_priority: usize,
    turns_left: usize,
}

impl<Item: QueueItem> SenderQueue<Item> {
    fn new() -> Self {
        let levels = Item::PRIORITY_LEVELS.max(1);
        Self {
            entries_by_priority: (0..levels).map(|_| BTreeMap::new()).collect(),
            present: HashSet::new(),
            pending_per_group: HashMap::new(),
            serving_priority: 0,
            turns_left: turns_at_priority(0, levels),
        }
    }

    fn len(&self) -> usize {
        self.entries_by_priority.iter().map(BTreeMap::len).sum()
    }

    fn is_empty(&self) -> bool {
        self.entries_by_priority.iter().all(BTreeMap::is_empty)
    }

    fn pending(&self, group: &Item::Group) -> usize {
        self.pending_per_group.get(group).copied().unwrap_or(0)
    }

    fn insert(&mut self, entry: Entry<Item>) {
        let priority = entry
            .item
            .priority()
            .min(self.entries_by_priority.len().saturating_sub(1));
        let key = entry.item.key();
        *self
            .pending_per_group
            .entry(entry.item.group())
            .or_insert(0) += 1;
        self.present.insert(key.clone());
        // In range: the vector is never empty and `priority` is clamped to its last index.
        self.entries_by_priority[priority].insert((entry.expires_at_ns, key), entry);
    }

    fn remove(
        &mut self,
        priority: usize,
        expires_at_ns: Timestamp,
        key: &Item::Key,
    ) -> Option<Entry<Item>> {
        let entry = self
            .entries_by_priority
            .get_mut(priority)?
            .remove(&(expires_at_ns, key.clone()))?;
        self.present.remove(key);
        let group = entry.item.group();
        if let Some(pending) = self.pending_per_group.get_mut(&group) {
            *pending = pending.saturating_sub(1);
            if *pending == 0 {
                self.pending_per_group.remove(&group);
            }
        }
        Some(entry)
    }

    /// Locates the next entry to take and advances the priority cursor. A priority
    /// runs until its turns are spent; an empty one hands over immediately.
    fn next_to_take(&mut self) -> Option<(usize, Timestamp, Item::Key)> {
        let levels = self.entries_by_priority.len().max(1);
        // Each pass either returns or moves to the next priority, so one pass per
        // priority plus the one already being served covers them all.
        for _ in 0..=levels {
            if self.turns_left == 0 {
                self.serving_priority = (self.serving_priority + 1) % levels;
                self.turns_left = turns_at_priority(self.serving_priority, levels);
            }
            match self.first_at(self.serving_priority) {
                Some((expires_at_ns, key)) => {
                    self.turns_left = self.turns_left.saturating_sub(1);
                    return Some((self.serving_priority, expires_at_ns, key));
                }
                None => self.turns_left = 0,
            }
        }
        None
    }

    /// The nearest deadline at one priority.
    fn first_at(&self, priority: usize) -> Option<(Timestamp, Item::Key)> {
        self.entries_by_priority
            .get(priority)?
            .first_key_value()
            .map(|((expires_at_ns, key), _)| (*expires_at_ns, key.clone()))
    }

    fn next_expiry_ns(&self) -> Option<Timestamp> {
        self.entries_by_priority
            .iter()
            .filter_map(|entries| entries.first_key_value().map(|((ts, _), _)| *ts))
            .min()
    }
}

pub(crate) struct AdmissionQueue<Sender: Clone + Ord, Item: QueueItem> {
    config: QueueConfig,
    senders: BTreeMap<Sender, SenderQueue<Item>>,
    /// Where the next batch resumes taking turns between senders.
    next_sender: Option<Sender>,
    // Total current number of entries across all senders.
    stored_total: usize,

    /// When the queue first became nonempty. Reset when it empties. Used to avoid counting time spent empty as a stall.
    nonempty_since_ns: Option<Timestamp>,
    /// When the queue filled. Resets when occupancy falls below `pressure_cleared_below`. Used to compute retry delays.
    at_capacity_since_ns: Option<Timestamp>,
    /// Last successful take, or construction or restore time.
    last_taken_ns: Timestamp,

    /// Metrics for monitoring and testing. These are not persisted, so they reset on canister upgrade.
    discarded_expired: u64,
    /// When a group has a pending entry with the same key, further items fold. This counts how many times that happened.
    folded_duplicates: u64,
    /// When a group has too many pending entries, further items are dropped. This counts how many times that happened.
    dropped_group_capped: u64,
    /// Items whose own expiry had already passed when they were submitted.
    dropped_already_expired: u64,
}

impl<Sender: Clone + Ord, Item: QueueItem> AdmissionQueue<Sender, Item> {
    pub(crate) fn new(config: QueueConfig, now_ns: Timestamp) -> Self {
        Self {
            config,
            senders: BTreeMap::new(),
            next_sender: None,
            stored_total: 0,
            nonempty_since_ns: None,
            at_capacity_since_ns: None,
            last_taken_ns: now_ns,
            discarded_expired: 0,
            folded_duplicates: 0,
            dropped_group_capped: 0,
            dropped_already_expired: 0,
        }
    }

    /// Submits items for a sender and returns each item's outcome, in input order.
    /// Uses trusted `now_ns` for all new entries. Folding leaves existing entries,
    /// including their age and priority, unchanged.
    pub(crate) fn admit(
        &mut self,
        sender: Sender,
        items: Vec<Item>,
        now_ns: Timestamp,
    ) -> Vec<Admitted<Item::Key>> {
        let mut admissions = Vec::with_capacity(items.len());
        let mut swept = false;

        if !items.is_empty() {
            self.discard_expired_for(&sender, now_ns);
        }

        for item in items {
            let key = item.key();
            let expires_at_ns = self.expiry_for(now_ns, &item);

            let admission = if expires_at_ns <= now_ns {
                self.dropped_already_expired += 1;
                Admission::Dropped
            } else if self.holds(&sender, &key) {
                self.folded_duplicates += 1;
                Admission::Folded
            } else if self.pending_for(&sender, &item.group()) >= self.config.max_pending_per_group
            {
                self.dropped_group_capped += 1;
                Admission::Dropped
            } else {
                // Before rejecting for capacity, free expired entries across all senders.
                // Once per call is enough because now_ns does not change.
                if !swept && (self.is_full() || self.sender_at_cap(&sender)) {
                    self.discard_expired(now_ns);
                    swept = true;
                }

                if self.is_full() || self.sender_at_cap(&sender) {
                    Admission::Full {
                        retry_after_ms: self.retry_after_ms(now_ns),
                    }
                } else {
                    self.insert(
                        &sender,
                        Entry {
                            received_at_ns: now_ns,
                            expires_at_ns,
                            item,
                        },
                    );
                    Admission::Accepted
                }
            };

            admissions.push(Admitted { key, admission });
        }

        admissions
    }

    /// Removes up to `limit` live entries, taking one per sender in turn.
    /// The caller must have room to keep all returned entries. Zero does nothing.
    /// Expired entries are discarded and do not count towards the limit.
    pub(crate) fn take_batch(
        &mut self,
        limit: usize,
        now_ns: Timestamp,
    ) -> Vec<Taken<Sender, Item>> {
        let mut taken = Vec::new();

        while taken.len() < limit {
            let Some(sender) = self.advance_cursor() else {
                break;
            };
            // If nothing is live, pop_live removes the sender. Each iteration
            // therefore returns an entry or reduces the number of senders.
            if let Some(entry) = self.pop_live(&sender, now_ns) {
                taken.push(Taken { sender, entry });
            }
        }

        if !taken.is_empty() {
            self.last_taken_ns = now_ns;
        }
        taken
    }

    /// Copies stored entries and the round-robin cursor. Restore rebuilds the
    /// indexes. Persistence and serialization are the caller's responsibility.
    pub(crate) fn snapshot(&self) -> QueueSnapshot<Sender, Item> {
        let senders = self
            .senders
            .iter()
            .map(|(sender, queue)| SenderSnapshot {
                sender: sender.clone(),
                entries: queue
                    .entries_by_priority
                    .iter()
                    .flat_map(|entries| entries.values().cloned())
                    .collect(),
                serving_priority: queue.serving_priority,
                turns_left: queue.turns_left,
            })
            .collect();
        QueueSnapshot {
            senders,
            next_sender: self.next_sender.clone(),
        }
    }

    /// Restores saved entries with their original arrival times and deadlines, and
    /// each sender's place in the priority rotation. Retry timers and metrics restart
    /// at `now_ns`. Entries are kept even if the new configuration lowers capacity;
    /// admission waits for space to become available.
    pub(crate) fn restore(
        config: QueueConfig,
        snapshot: QueueSnapshot<Sender, Item>,
        now_ns: Timestamp,
    ) -> Self {
        let mut backlog = Self::new(config, now_ns);
        for sender in snapshot.senders {
            for entry in sender.entries {
                backlog.insert(&sender.sender, entry);
            }
            if let Some(queue) = backlog.senders.get_mut(&sender.sender) {
                let levels = queue.entries_by_priority.len();
                queue.serving_priority = sender.serving_priority.min(levels.saturating_sub(1));
                queue.turns_left = sender.turns_left;
            }
        }
        backlog.next_sender = snapshot.next_sender;
        backlog.at_capacity_since_ns = backlog.is_full().then_some(now_ns);
        backlog
    }

    pub(crate) fn stats(&self, now_ns: Timestamp) -> QueueStats {
        QueueStats {
            stored_total: self.stored_total,
            active_senders: self.senders.len(),
            next_expiry_in_ns: self
                .senders
                .values()
                .filter_map(SenderQueue::next_expiry_ns)
                .min()
                .map(|nearest| nearest.saturating_sub(now_ns)),
            at_capacity_for_ns: self
                .at_capacity_since_ns
                .map(|since| now_ns.saturating_sub(since)),
            silence_ns: now_ns.saturating_sub(self.last_taken_ns),
            discarded_expired: self.discarded_expired,
            folded_duplicates: self.folded_duplicates,
            dropped_group_capped: self.dropped_group_capped,
            dropped_already_expired: self.dropped_already_expired,
        }
    }

    /// When an item stops being worth delivering. A submitter may ask for less time
    /// than the queue's limit, never more.
    fn expiry_for(&self, received_at_ns: Timestamp, item: &Item) -> Timestamp {
        let limit = received_at_ns.saturating_add(self.config.discard_entries_after_ns);
        item.expires_at_ns()
            .map_or(limit, |chosen| chosen.min(limit))
    }

    fn holds(&self, sender: &Sender, key: &Item::Key) -> bool {
        self.senders
            .get(sender)
            .is_some_and(|queue| queue.present.contains(key))
    }

    fn pending_for(&self, sender: &Sender, group: &Item::Group) -> usize {
        self.senders
            .get(sender)
            .map_or(0, |queue| queue.pending(group))
    }

    fn is_full(&self) -> bool {
        self.stored_total >= self.config.max_entries
    }

    fn sender_at_cap(&self, sender: &Sender) -> bool {
        let held = self.senders.get(sender).map_or(0, SenderQueue::len);
        held >= self.sender_cap()
    }

    /// Divides capacity equally among active senders, up to the per-sender limit.
    /// Existing entries are not evicted when a new sender reduces this share.
    fn sender_cap(&self) -> usize {
        self.config
            .max_entries_per_sender
            .min(self.config.max_entries / self.senders.len().max(1))
    }

    fn insert(&mut self, sender: &Sender, entry: Entry<Item>) {
        let received_at_ns = entry.received_at_ns;
        if self.stored_total == 0 {
            self.nonempty_since_ns = Some(received_at_ns);
        }
        self.senders
            .entry(sender.clone())
            .or_insert_with(SenderQueue::new)
            .insert(entry);
        self.stored_total += 1;
        if self.is_full() {
            self.at_capacity_since_ns.get_or_insert(received_at_ns);
        }
    }

    /// Removes this sender's next live entry. Sweeping first keeps expired entries
    /// from spending the priority's turns.
    fn pop_live(&mut self, sender: &Sender, now_ns: Timestamp) -> Option<Entry<Item>> {
        self.discard_expired_for(sender, now_ns);
        self.pop_front(sender)
    }

    fn pop_front(&mut self, sender: &Sender) -> Option<Entry<Item>> {
        let queue = self.senders.get_mut(sender)?;
        let (priority, expires_at_ns, key) = queue.next_to_take()?;
        let entry = queue.remove(priority, expires_at_ns, &key)?;
        let emptied = queue.is_empty();
        self.after_removal(sender, emptied);
        Some(entry)
    }

    /// Removes expired entries from every sender. At each priority, entries are
    /// ordered by deadline, so scanning stops at the first live entry.
    fn discard_expired(&mut self, now_ns: Timestamp) {
        for sender in self.senders.keys().cloned().collect::<Vec<_>>() {
            self.discard_expired_for(&sender, now_ns);
        }
    }

    fn discard_expired_for(&mut self, sender: &Sender, now_ns: Timestamp) {
        for priority in 0..Item::PRIORITY_LEVELS.max(1) {
            loop {
                let Some(queue) = self.senders.get_mut(sender) else {
                    return;
                };
                let Some((expires_at_ns, key)) = queue.first_at(priority) else {
                    break;
                };
                if expires_at_ns > now_ns {
                    break;
                }
                queue.remove(priority, expires_at_ns, &key);
                let emptied = queue.is_empty();
                self.after_removal(sender, emptied);
                self.discarded_expired += 1;
            }
        }
    }

    /// Updates counts and timers after removal, and removes empty senders.
    fn after_removal(&mut self, sender: &Sender, emptied: bool) {
        if emptied {
            self.senders.remove(sender);
        }
        self.stored_total = self.stored_total.saturating_sub(1);
        if self.stored_total == 0 {
            self.nonempty_since_ns = None;
        }
        if self.stored_total < self.config.pressure_cleared_below {
            self.at_capacity_since_ns = None;
        }
    }

    /// Selects the next sender and advances the cursor. If that sender was removed,
    /// uses the next remaining sender, wrapping around at the end.
    fn advance_cursor(&mut self) -> Option<Sender> {
        if self.senders.is_empty() {
            self.next_sender = None;
            return None;
        }

        let picked = self
            .next_sender
            .as_ref()
            .and_then(|from| {
                self.senders
                    .range(from.clone()..)
                    .next()
                    .map(|(sender, _)| sender.clone())
            })
            .or_else(|| self.senders.keys().next().cloned())?;

        self.next_sender = self
            .senders
            .range((Excluded(picked.clone()), Unbounded))
            .next()
            .map(|(sender, _)| sender.clone())
            .or_else(|| self.senders.keys().next().cloned());

        Some(picked)
    }

    /// Chooses a retry delay from queue pressure and time since the last take.
    fn retry_after_ms(&self, now_ns: Timestamp) -> u32 {
        let retry = &self.config.retry;

        // Time spent empty does not count as a stall.
        let waiting_since_ns = self
            .last_taken_ns
            .max(self.nonempty_since_ns.unwrap_or(now_ns));
        if now_ns.saturating_sub(waiting_since_ns) > retry.stalled_after_silence_ns {
            return retry.when_stalled_ms;
        }

        let at_capacity_for_ns = now_ns.saturating_sub(self.at_capacity_since_ns.unwrap_or(now_ns));
        let doublings = (at_capacity_for_ns / retry.doubles_every_ns.max(1))
            .min(u64::from(retry.max_doublings)) as u32;
        retry
            .base_ms
            .saturating_mul(1u32.checked_shl(doublings).unwrap_or(u32::MAX))
            .min(retry.ceiling_ms)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A small test item with two priority levels.
    #[derive(Clone, Debug, Eq, PartialEq)]
    struct TestItem {
        group: u8,
        id: u8,
        priority: usize,
        expires_at_ns: Option<Timestamp>,
    }

    impl QueueItem for TestItem {
        type Key = (u8, u8);
        type Group = u8;
        const PRIORITY_LEVELS: usize = 2;

        fn key(&self) -> Self::Key {
            (self.group, self.id)
        }

        fn group(&self) -> Self::Group {
            self.group
        }

        fn priority(&self) -> usize {
            self.priority
        }

        fn expires_at_ns(&self) -> Option<Timestamp> {
            self.expires_at_ns
        }
    }

    type TestQueue = AdmissionQueue<u8, TestItem>;

    fn item(group: u8, id: u8) -> TestItem {
        TestItem {
            group,
            id,
            priority: 0,
            expires_at_ns: None,
        }
    }

    fn at_priority(group: u8, id: u8, priority: usize) -> TestItem {
        TestItem {
            priority,
            ..item(group, id)
        }
    }

    fn expiring(group: u8, id: u8, expires_at_ns: Timestamp) -> TestItem {
        TestItem {
            expires_at_ns: Some(expires_at_ns),
            ..item(group, id)
        }
    }

    fn config() -> QueueConfig {
        QueueConfig {
            max_entries: 8,
            max_entries_per_sender: 8,
            max_pending_per_group: 8,
            pressure_cleared_below: 6,
            discard_entries_after_ns: 100,
            retry: RetryPolicy {
                base_ms: 1_000,
                ceiling_ms: 8_000,
                when_stalled_ms: 60_000,
                doubles_every_ns: 10,
                max_doublings: 3,
                stalled_after_silence_ns: 50,
            },
        }
    }

    /// Admits items in order, one nanosecond apart.
    fn admit_each(backlog: &mut TestQueue, sender: u8, items: Vec<TestItem>, from_ns: Timestamp) {
        for (offset, item) in items.into_iter().enumerate() {
            backlog.admit(sender, vec![item], from_ns + offset as u64);
        }
    }

    fn keys_taken(taken: &[Taken<u8, TestItem>]) -> Vec<(u8, u8)> {
        taken.iter().map(|t| t.entry.item.key()).collect()
    }

    fn senders_taken(taken: &[Taken<u8, TestItem>]) -> Vec<u8> {
        taken.iter().map(|t| t.sender).collect()
    }

    /// Checks that counts, indexes, and timers agree with the stored entries.
    fn assert_consistent(backlog: &TestQueue) {
        let counted: usize = backlog.senders.values().map(SenderQueue::len).sum();
        assert_eq!(backlog.stored_total, counted, "stored_total drifted");

        for (sender, queue) in &backlog.senders {
            assert!(!queue.is_empty(), "sender {sender} kept after emptying");

            let keys: HashSet<(u8, u8)> = queue
                .entries_by_priority
                .iter()
                .flat_map(|entries| entries.keys().map(|(_, key)| *key))
                .collect();
            assert_eq!(queue.present, keys, "present drifted for sender {sender}");

            let mut groups: HashMap<u8, usize> = HashMap::new();
            for (group, _) in &keys {
                *groups.entry(*group).or_insert(0) += 1;
            }
            assert_eq!(
                queue.pending_per_group, groups,
                "pending_per_group drifted for sender {sender}"
            );
        }

        assert_eq!(
            backlog.nonempty_since_ns.is_some(),
            backlog.stored_total > 0,
            "nonempty clock disagrees with occupancy"
        );
        if backlog.stored_total < backlog.config.pressure_cleared_below {
            assert!(
                backlog.at_capacity_since_ns.is_none(),
                "pressure clock left running below the low mark"
            );
        }
    }

    // Expiry.

    #[test]
    fn an_item_may_ask_to_die_before_the_queues_own_limit() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![expiring(1, 1, 11)], 1);

        // The configured lifetime would have kept this until nanosecond 101.
        assert!(backlog.take_batch(10, 12).is_empty());
        assert_eq!(backlog.stats(12).discarded_expired, 1);
        assert_consistent(&backlog);
    }

    #[test]
    fn an_item_may_not_ask_to_outlive_the_queues_own_limit() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![expiring(1, 1, 10_000)], 1);

        assert_eq!(keys_taken(&backlog.take_batch(10, 100)), vec![(1, 1)]);

        backlog.admit(1, vec![expiring(2, 2, 10_000)], 1);
        assert!(backlog.take_batch(10, 101).is_empty());
        assert_consistent(&backlog);
    }

    #[test]
    fn an_item_that_has_already_expired_is_dropped_on_arrival() {
        let mut backlog = TestQueue::new(config(), 0);

        assert_eq!(
            results(backlog.admit(1, vec![expiring(1, 1, 5)], 5)),
            vec![Admission::Dropped]
        );
        assert_eq!(backlog.stored_total, 0);
        assert_eq!(backlog.stats(5).dropped_already_expired, 1);
        assert_consistent(&backlog);
    }

    #[test]
    fn the_nearest_deadline_leaves_first_whatever_the_arrival_order() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![expiring(1, 1, 90)], 1);
        backlog.admit(1, vec![expiring(2, 2, 20)], 2);

        assert_eq!(keys_taken(&backlog.take_batch(10, 3)), vec![(2, 2), (1, 1)]);
        assert_consistent(&backlog);
    }

    #[test]
    fn stats_report_the_time_left_on_the_nearest_deadline() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![expiring(1, 1, 90)], 1);
        backlog.admit(2, vec![expiring(2, 2, 20)], 1);

        assert_eq!(backlog.stats(5).next_expiry_in_ns, Some(15));
    }

    // Ordering.

    #[test]
    fn takes_in_arrival_order_at_the_same_priority() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(2, 2), item(3, 3)], 1);

        let taken = backlog.take_batch(10, 5);

        assert_eq!(keys_taken(&taken), vec![(1, 1), (2, 2), (3, 3)]);
        assert_consistent(&backlog);
    }

    #[test]
    fn takes_highest_priority_first() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(
            &mut backlog,
            1,
            vec![
                at_priority(1, 1, 1),
                at_priority(2, 2, 1),
                at_priority(3, 3, 0),
            ],
            1,
        );

        let taken = backlog.take_batch(10, 5);

        // The highest-priority item arrived last and still leaves first.
        assert_eq!(keys_taken(&taken), vec![(3, 3), (1, 1), (2, 2)]);
    }

    #[test]
    fn a_busy_high_priority_still_hands_turns_to_the_low_one() {
        // Two levels, so priority zero runs two turns and priority one runs one.
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(
            &mut backlog,
            1,
            vec![
                item(1, 1),
                item(2, 2),
                item(3, 3),
                at_priority(4, 4, 1),
                at_priority(5, 5, 1),
                at_priority(6, 6, 1),
            ],
            1,
        );

        assert_eq!(
            keys_taken(&backlog.take_batch(10, 7)),
            vec![(1, 1), (2, 2), (4, 4), (3, 3), (5, 5), (6, 6)]
        );
        assert_consistent(&backlog);
    }

    #[test]
    fn the_priority_rotation_resumes_after_a_restore() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(
            &mut backlog,
            1,
            vec![item(1, 1), item(2, 2), item(3, 3), at_priority(4, 4, 1)],
            1,
        );
        // Spends both of priority zero's turns.
        assert_eq!(keys_taken(&backlog.take_batch(2, 5)), vec![(1, 1), (2, 2)]);

        let mut restored = TestQueue::restore(config(), backlog.snapshot(), 5);

        // A queue starting fresh would run priority zero again here.
        assert_eq!(keys_taken(&restored.take_batch(1, 6)), vec![(4, 4)]);
        assert_consistent(&restored);
    }

    #[test]
    fn an_out_of_range_priority_is_clamped_rather_than_panicking() {
        let mut backlog = TestQueue::new(config(), 0);

        backlog.admit(1, vec![at_priority(1, 1, 99)], 1);

        assert_eq!(keys_taken(&backlog.take_batch(10, 2)), vec![(1, 1)]);
    }

    // Folding.

    #[test]
    fn folds_a_key_it_already_holds() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        let admissions = results(backlog.admit(1, vec![item(1, 1)], 2));

        assert_eq!(admissions, vec![Admission::Folded]);
        assert_eq!(backlog.stored_total, 1);
        assert_eq!(backlog.stats(2).folded_duplicates, 1);
    }

    #[test]
    fn the_same_key_under_another_sender_is_a_different_item() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        let admissions = results(backlog.admit(2, vec![item(1, 1)], 1));

        assert_eq!(admissions, vec![Admission::Accepted]);
        assert_eq!(backlog.stored_total, 2);
    }

    #[test]
    fn a_fold_leaves_the_original_where_it_was() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(2, 2)], 1);

        // Resubmitting must not change the original entry's arrival time.
        assert_eq!(
            results(backlog.admit(1, vec![item(1, 1)], 9)),
            vec![Admission::Folded]
        );

        assert_eq!(
            keys_taken(&backlog.take_batch(10, 10)),
            vec![(1, 1), (2, 2)]
        );
    }

    #[test]
    fn drops_once_a_group_is_at_its_cap() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_pending_per_group: 2,
                ..config()
            },
            0,
        );

        let admissions = results(backlog.admit(1, vec![item(7, 1), item(7, 2), item(7, 3)], 1));

        assert_eq!(
            admissions,
            vec![Admission::Accepted, Admission::Accepted, Admission::Dropped]
        );
        assert_eq!(backlog.stats(1).dropped_group_capped, 1);
        assert_consistent(&backlog);
    }

    #[test]
    fn a_group_frees_its_cap_as_its_items_leave() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_pending_per_group: 2,
                ..config()
            },
            0,
        );
        admit_each(&mut backlog, 1, vec![item(7, 1), item(7, 2)], 1);

        backlog.take_batch(1, 3);

        assert_eq!(
            results(backlog.admit(1, vec![item(7, 3)], 4)),
            vec![Admission::Accepted]
        );
        assert_consistent(&backlog);
    }

    #[test]
    fn an_expired_duplicate_does_not_cover_a_new_submission() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        assert_eq!(
            results(backlog.admit(1, vec![item(1, 1)], 101)),
            vec![Admission::Accepted]
        );
        assert_eq!(keys_taken(&backlog.take_batch(1, 101)), vec![(1, 1)]);
        assert_eq!(backlog.stats(101).discarded_expired, 1);
        assert_consistent(&backlog);
    }

    #[test]
    fn expired_entries_do_not_count_towards_the_group_cap() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_pending_per_group: 1,
                ..config()
            },
            0,
        );
        backlog.admit(1, vec![item(1, 1)], 1);

        assert_eq!(
            results(backlog.admit(1, vec![item(1, 2)], 101)),
            vec![Admission::Accepted]
        );
        assert_eq!(keys_taken(&backlog.take_batch(1, 101)), vec![(1, 2)]);
        assert_consistent(&backlog);
    }

    // Fairness.

    #[test]
    fn takes_one_sender_at_a_time_in_turn() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(1, 2), item(1, 3)], 1);
        backlog.admit(2, vec![item(2, 1)], 1);
        admit_each(&mut backlog, 3, vec![item(3, 1), item(3, 2)], 1);

        let taken = backlog.take_batch(5, 5);

        assert_eq!(senders_taken(&taken), vec![1, 2, 3, 1, 3]);
        assert_eq!(backlog.stored_total, 1);
    }

    #[test]
    fn the_cursor_resumes_where_the_last_batch_stopped() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(1, 2)], 1);
        admit_each(&mut backlog, 2, vec![item(2, 1), item(2, 2)], 1);

        let first = backlog.take_batch(1, 5);
        let second = backlog.take_batch(1, 6);
        let third = backlog.take_batch(1, 7);

        assert_eq!(senders_taken(&first), vec![1]);
        assert_eq!(senders_taken(&second), vec![2]);
        assert_eq!(senders_taken(&third), vec![1]);
    }

    #[test]
    fn one_sender_alone_stops_at_its_own_ceiling() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_entries_per_sender: 3,
                ..config()
            },
            0,
        );

        let admissions =
            results(backlog.admit(1, vec![item(1, 1), item(2, 2), item(3, 3), item(4, 4)], 1));

        assert_eq!(
            admissions,
            vec![
                Admission::Accepted,
                Admission::Accepted,
                Admission::Accepted,
                Admission::Full {
                    retry_after_ms: 1_000
                },
            ]
        );
        // Other senders can use the five remaining slots.
        assert_eq!(backlog.stored_total, 3);
    }

    #[test]
    fn an_incumbent_over_its_share_is_refused_while_a_newcomer_is_admitted() {
        let mut backlog = TestQueue::new(config(), 0);
        // With only one sender, all five items fit.
        admit_each(
            &mut backlog,
            1,
            vec![item(1, 1), item(2, 2), item(3, 3), item(4, 4), item(5, 5)],
            1,
        );
        backlog.admit(2, vec![item(1, 1)], 6);

        // Each sender's share is now four. Sender 1 already holds five.
        let incumbent = results(backlog.admit(1, vec![item(6, 6)], 7));
        let newcomer = results(backlog.admit(2, vec![item(2, 2)], 7));

        assert_eq!(
            incumbent,
            vec![Admission::Full {
                retry_after_ms: 1_000
            }]
        );
        assert_eq!(newcomer, vec![Admission::Accepted]);
    }

    #[test]
    fn a_sender_is_dropped_once_it_empties() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.admit(2, vec![item(2, 1)], 1);

        backlog.take_batch(1, 2);

        assert_eq!(backlog.stats(2).active_senders, 1);
        assert_consistent(&backlog);
    }

    // Retry delays.

    #[test]
    fn a_full_buffer_answers_with_the_base_hint() {
        let mut backlog = full_queue(1);

        let admissions = results(backlog.admit(1, vec![item(9, 9)], 1));

        assert_eq!(
            admissions,
            vec![Admission::Full {
                retry_after_ms: 1_000
            }]
        );
    }

    #[test]
    fn the_hint_doubles_the_longer_the_buffer_stays_full() {
        let mut backlog = full_queue(1);
        backlog.admit(1, vec![item(9, 9)], 1);

        // Full since 1, and the hint doubles every 10ns.
        assert_eq!(hint_at(&mut backlog, 11), 2_000);
        assert_eq!(hint_at(&mut backlog, 26), 4_000);
    }

    #[test]
    fn pressure_starts_when_the_last_free_slot_is_filled() {
        let mut backlog = full_queue(1);

        assert_eq!(backlog.stats(21).at_capacity_for_ns, Some(20));
        assert_eq!(hint_at(&mut backlog, 21), 4_000);
    }

    #[test]
    fn a_sender_at_its_cap_gets_the_stalled_hint() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_entries_per_sender: 1,
                ..config()
            },
            0,
        );
        backlog.admit(1, vec![item(1, 1)], 1);

        assert_eq!(
            results(backlog.admit(1, vec![item(2, 2)], 60)),
            vec![Admission::Full {
                retry_after_ms: 60_000
            }]
        );
    }

    #[test]
    fn the_hint_stops_at_the_ceiling() {
        let mut backlog = full_queue(1);
        backlog.admit(1, vec![item(9, 9)], 1);

        // Three doublings reach the maximum delay of 8_000ms.
        assert_eq!(hint_at(&mut backlog, 41), 8_000);
        assert_eq!(hint_at(&mut backlog, 45), 8_000);
    }

    #[test]
    fn the_hint_jumps_once_nothing_is_draining() {
        let mut backlog = full_queue(1);
        backlog.admit(1, vec![item(9, 9)], 1);

        // The dispatcher has no room to accept entries.
        for tick in 2..60 {
            assert!(backlog.take_batch(0, tick).is_empty());
        }

        assert_eq!(hint_at(&mut backlog, 60), 60_000);
    }

    #[test]
    fn an_idle_buffer_filled_in_one_call_is_not_stalled() {
        // Time spent empty must not trigger the stalled retry delay.
        let mut backlog = TestQueue::new(config(), 0);
        let items = (1..=9).map(|id| item(id, id)).collect();

        let admissions = results(backlog.admit(1, items, 10_000));

        assert_eq!(
            admissions.last(),
            Some(&Admission::Full {
                retry_after_ms: 1_000
            })
        );
    }

    #[test]
    fn pressure_clears_once_occupancy_falls_below_the_low_mark() {
        let mut backlog = full_queue(1);
        backlog.admit(1, vec![item(9, 9)], 1);
        assert!(backlog.stats(1).at_capacity_for_ns.is_some());

        // Removing three entries brings occupancy below the reset threshold of six.
        backlog.take_batch(3, 2);

        assert!(backlog.stats(2).at_capacity_for_ns.is_none());
        assert_consistent(&backlog);
    }

    #[test]
    fn taking_nothing_leaves_the_silence_clock_alone() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        assert!(backlog.take_batch(0, 30).is_empty());

        assert_eq!(backlog.stats(30).silence_ns, 30);
        assert_eq!(backlog.stored_total, 1);
    }

    // Expiry.

    #[test]
    fn an_expired_entry_is_discarded_rather_than_taken() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        let taken = backlog.take_batch(10, 101);

        assert!(taken.is_empty());
        assert_eq!(backlog.stats(101).discarded_expired, 1);
        assert_consistent(&backlog);
    }

    #[test]
    fn expired_entries_are_swept_to_make_room() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_entries: 2,
                max_entries_per_sender: 2,
                pressure_cleared_below: 2,
                ..config()
            },
            0,
        );
        admit_each(&mut backlog, 1, vec![item(1, 1), item(2, 2)], 1);

        // Both stored entries have expired, so the new item should fit.
        let admissions = results(backlog.admit(1, vec![item(3, 3)], 200));

        assert_eq!(admissions, vec![Admission::Accepted]);
        assert_eq!(backlog.stats(200).discarded_expired, 2);
        assert_consistent(&backlog);
    }

    #[test]
    fn a_live_entry_behind_an_expired_one_still_leaves() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.admit(1, vec![item(2, 2)], 150);

        let taken = backlog.take_batch(10, 151);

        assert_eq!(keys_taken(&taken), vec![(2, 2)]);
        assert_eq!(backlog.stats(151).discarded_expired, 1);
    }

    #[test]
    fn expired_senders_do_not_stop_a_batch_before_live_senders() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.admit(2, vec![item(2, 2)], 1);
        backlog.admit(3, vec![item(3, 3)], 50);

        let taken = backlog.take_batch(1, 101);

        assert_eq!(senders_taken(&taken), vec![3]);
        assert_eq!(backlog.stats(101).discarded_expired, 2);
        assert_eq!(backlog.stored_total, 0);
        assert_consistent(&backlog);
    }

    // Snapshot and restore.

    #[test]
    fn snapshot_and_restore_keep_the_entries_and_their_arrival_times() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![at_priority(1, 1, 1), item(2, 2)], 1);
        backlog.admit(2, vec![item(3, 3)], 3);

        let mut restored = TestQueue::restore(config(), backlog.snapshot(), 4);

        assert_eq!(restored.stored_total, backlog.stored_total);
        assert_eq!(restored.snapshot(), backlog.snapshot());
        // Restore must preserve priority and arrival order.
        assert_eq!(
            keys_taken(&restored.take_batch(10, 50)),
            keys_taken(&backlog.take_batch(10, 50))
        );
        assert_consistent(&restored);
    }

    #[test]
    fn a_restored_buffer_is_not_reported_as_stalled() {
        let backlog = full_queue(1);
        let snapshot = backlog.snapshot();

        // The entries are old enough to trigger a stall, but have not expired.
        let mut restored = TestQueue::restore(config(), snapshot, 60);
        assert_eq!(
            restored.stored_total, 8,
            "the entries aged out, so this proves nothing"
        );

        assert_eq!(
            results(restored.admit(1, vec![item(9, 9)], 60)),
            vec![Admission::Full {
                retry_after_ms: 1_000
            }]
        );
    }

    #[test]
    fn restoring_nothing_gives_an_empty_buffer() {
        let empty = TestQueue::new(config(), 0);
        let restored = TestQueue::restore(config(), empty.snapshot(), 10);

        assert_eq!(restored.stored_total, 0);
        assert_eq!(restored.stats(10).active_senders, 0);
        assert_consistent(&restored);
    }

    #[test]
    fn restore_preserves_the_next_sender_to_serve() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1), item(1, 2)], 1);
        backlog.admit(2, vec![item(2, 1)], 1);
        assert_eq!(senders_taken(&backlog.take_batch(1, 2)), vec![1]);

        let mut restored = TestQueue::restore(config(), backlog.snapshot(), 3);

        assert_eq!(senders_taken(&restored.take_batch(1, 4)), vec![2]);
        assert_consistent(&restored);
    }

    #[test]
    fn restore_restarts_the_pressure_timer() {
        let backlog = full_queue(1);
        let mut restored = TestQueue::restore(config(), backlog.snapshot(), 60);

        assert_eq!(restored.stats(60).at_capacity_for_ns, Some(0));
        assert_eq!(hint_at(&mut restored, 70), 2_000);
    }

    /// The result is positional, and the caller zips it against its own list. Zip
    /// truncates silently, so a branch that forgot to answer would lose the tail of
    /// a batch without failing anything.
    #[test]
    fn every_item_gets_exactly_one_answer() {
        let mut queue = TestQueue::new(
            QueueConfig {
                max_pending_per_group: 2,
                ..config()
            },
            0,
        );
        // Group 7 at its limit, and the queue at its ceiling.
        admit_each(&mut queue, 1, vec![item(7, 1), item(7, 2)], 1);
        admit_each(
            &mut queue,
            1,
            vec![
                item(1, 1),
                item(2, 2),
                item(3, 3),
                item(4, 4),
                item(5, 5),
                item(6, 6),
            ],
            3,
        );
        assert_eq!(queue.stored_total, config().max_entries);

        // One item down each of the four paths, in the order admit checks them.
        let batch = vec![item(7, 1), item(7, 3), item(8, 8), item(9, 9)];
        let answers = queue.admit(1, batch, 20);

        // Each answer names the item it belongs to, so the caller never reads by position.
        assert_eq!(
            answers,
            vec![
                Admitted {
                    key: (7, 1),
                    admission: Admission::Folded
                },
                Admitted {
                    key: (7, 3),
                    admission: Admission::Dropped
                },
                Admitted {
                    key: (8, 8),
                    admission: Admission::Full {
                        retry_after_ms: 2_000
                    }
                },
                Admitted {
                    key: (9, 9),
                    admission: Admission::Full {
                        retry_after_ms: 2_000
                    }
                },
            ]
        );
        assert!(queue.admit(1, vec![], 20).is_empty());
    }

    #[test]
    fn a_configuration_whose_pressure_cannot_clear_is_incoherent() {
        assert!(config().is_coherent());
        assert!(!QueueConfig {
            pressure_cleared_below: 0,
            ..config()
        }
        .is_coherent());
        assert!(!QueueConfig {
            max_entries_per_sender: config().max_entries + 1,
            ..config()
        }
        .is_coherent());
        assert!(!QueueConfig {
            discard_entries_after_ns: 0,
            ..config()
        }
        .is_coherent());
    }

    /// A wrong `PRIORITY_LEVELS` is a programming error, but the queue is built
    /// during `post_upgrade`, where a panic fails the upgrade, so it degrades.
    #[test]
    fn an_item_without_priority_levels_still_works() {
        #[derive(Clone)]
        struct NoPriorityLevels(u8);

        impl QueueItem for NoPriorityLevels {
            type Key = u8;
            type Group = u8;
            const PRIORITY_LEVELS: usize = 0;

            fn key(&self) -> u8 {
                self.0
            }
            fn group(&self) -> u8 {
                self.0
            }
            fn priority(&self) -> usize {
                7
            }
        }

        let mut queue: AdmissionQueue<u8, NoPriorityLevels> = AdmissionQueue::new(config(), 0);

        assert_eq!(
            results(queue.admit(1, vec![NoPriorityLevels(1)], 1)),
            vec![Admission::Accepted]
        );
        assert_eq!(queue.take_batch(10, 2).len(), 1);
    }

    // Test helpers.

    /// Fills the queue at nanosecond 1.
    fn full_queue(sender: u8) -> TestQueue {
        let mut backlog = TestQueue::new(config(), 0);
        let items = (1..=8).map(|id| item(id, id)).collect();
        assert_eq!(
            results(backlog.admit(sender, items, 1)),
            vec![Admission::Accepted; 8],
            "test setup failed to fill the queue"
        );
        backlog
    }

    /// The outcomes without their keys, for assertions that only check what happened.
    fn results<Key>(admitted: Vec<Admitted<Key>>) -> Vec<Admission> {
        admitted
            .into_iter()
            .map(|answer| answer.admission)
            .collect()
    }

    /// Reads the retry delay by submitting to a full queue.
    fn hint_at(backlog: &mut TestQueue, now_ns: Timestamp) -> u32 {
        match backlog
            .admit(99, vec![item(99, 99)], now_ns)
            .remove(0)
            .admission
        {
            Admission::Full { retry_after_ms } => retry_after_ms,
            other => panic!("expected a full queue, got {other:?}"),
        }
    }
}
