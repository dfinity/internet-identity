//! Bounded queue with per-sender and per-group limits.
//!
//! Senders take turns; within each sender, priority `p` of `n` gets `n - p` turns.
//! Entries at the same priority are ordered by deadline, then key.
//!
//! Deduplication covers queued entries only. Resends can extend a deadline up to
//! the entry's maximum lifetime. Callers supply trusted canister time and handle
//! authorization and delivery.
// Used by the submission endpoint and dispatcher in follow-up PRs.
#![allow(dead_code)]

use internet_identity_interface::internet_identity::types::Timestamp;
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;
use std::ops::Bound::{Excluded, Unbounded};

pub(crate) trait QueueItem: Clone {
    /// Deduplication key within one sender.
    type Key: Clone + Eq + Hash + Ord;

    /// Items in the same group share a per-sender pending limit.
    type Group: Copy + Eq + Hash;

    /// Zero is treated as one priority level.
    const PRIORITY_LEVELS: usize;

    fn key(&self) -> Self::Key;
    fn group(&self) -> Self::Group;

    /// Lower values get more turns. Out-of-range values use the lowest priority.
    fn priority(&self) -> usize;

    /// Optional submitter deadline, capped by the queue expiry limit.
    fn expires_at_ns(&self) -> Option<Timestamp> {
        None
    }
}

const fn turns_at_priority(priority: usize, levels: usize) -> usize {
    if levels > priority {
        levels - priority
    } else {
        1
    }
}

#[derive(Clone, Debug)]
pub(crate) struct QueueConfig {
    /// Bounds entry count; callers must bound item size separately.
    pub(crate) max_entries: usize,
    /// Upper bound; active senders also share total capacity equally.
    pub(crate) max_entries_per_sender: usize,
    pub(crate) max_pending_per_group: usize,
    /// Reset the queue pressure timer below this occupancy.
    pub(crate) pressure_cleared_below: usize,
    /// Expiry limit from the latest submission, capped by the item deadline.
    pub(crate) discard_entries_after_ns: u64,
    /// Maximum lifetime from first admission, including resends.
    pub(crate) max_lifetime_ns: u64,
    pub(crate) retry: RetryPolicy,
}

impl QueueConfig {
    pub(crate) const fn is_coherent(&self) -> bool {
        self.max_entries > 0
            && self.max_entries_per_sender > 0
            && self.max_pending_per_group > 0
            && self.max_entries_per_sender <= self.max_entries
            && self.pressure_cleared_below > 0
            && self.pressure_cleared_below <= self.max_entries
            && self.discard_entries_after_ns > 0
            && self.max_lifetime_ns >= self.discard_entries_after_ns
            && self.retry.is_coherent()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct RetryPolicy {
    pub(crate) base_ms: u32,
    /// Retry ceiling while the queue is making progress.
    pub(crate) ceiling_ms: u32,
    /// Retry delay after `stalled_after_silence_ns` without progress.
    pub(crate) when_stalled_ms: u32,
    /// Interval of sustained pressure between delay doublings.
    pub(crate) doubles_every_ns: u64,
    pub(crate) max_doublings: u32,
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Admission {
    Accepted,
    /// Duplicate key; the resend replaces the queued entry, keeping its arrival time.
    Folded,
    /// Already expired on arrival.
    Dropped,
    /// Queue, sender, or group capacity reached.
    Full {
        retry_after_ms: u32,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Admitted<Key> {
    pub(crate) key: Key,
    pub(crate) admission: Admission,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Entry<Item> {
    pub(crate) received_at_ns: Timestamp,
    pub(crate) expires_at_ns: Timestamp,
    pub(crate) item: Item,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Taken<Sender, Item> {
    pub(crate) sender: Sender,
    pub(crate) entry: Entry<Item>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct SenderSnapshot<Sender, Item> {
    sender: Sender,
    entries: Vec<Entry<Item>>,
    serving_priority: usize,
    turns_left: usize,
}

/// Private fields prevent callers from constructing inconsistent snapshots.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct QueueSnapshot<Sender, Item> {
    senders: Vec<SenderSnapshot<Sender, Item>>,
    next_sender: Option<Sender>,
}

impl<Sender, Item: QueueItem> QueueSnapshot<Sender, Item> {
    /// Rebuild from entries, restarting sender and priority rotations.
    pub(crate) fn from_entries(senders: Vec<(Sender, Vec<Entry<Item>>)>) -> Self {
        let levels = Item::PRIORITY_LEVELS.max(1);
        Self {
            senders: senders
                .into_iter()
                .map(|(sender, entries)| SenderSnapshot {
                    sender,
                    entries,
                    serving_priority: 0,
                    turns_left: turns_at_priority(0, levels),
                })
                .collect(),
            next_sender: None,
        }
    }
}

/// Expired entries count until removed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct QueueStats {
    pub(crate) stored_total: usize,
    pub(crate) active_senders: usize,
    pub(crate) next_expiry_in_ns: Option<u64>,
    /// Queue pressure duration; resets below `pressure_cleared_below`.
    pub(crate) at_capacity_for_ns: Option<u64>,
    /// Time since the last successful take or construction.
    pub(crate) silence_ns: u64,
    pub(crate) discarded_expired: u64,
    pub(crate) folded_duplicates: u64,
    pub(crate) rejected_group_full: u64,
    pub(crate) dropped_already_expired: u64,
}

/// Locates an entry by key without scanning the priority buckets.
#[derive(Clone, Copy, Debug)]
struct Placed {
    priority: usize,
    expires_at_ns: Timestamp,
}

#[derive(Clone, Copy, Debug, Default)]
struct GroupState {
    pending: usize,
    full_since_ns: Option<Timestamp>,
}

/// Ordered by deadline, then key.
type PriorityBucket<Item> = BTreeMap<(Timestamp, <Item as QueueItem>::Key), Entry<Item>>;

struct SenderQueue<Item: QueueItem> {
    entries_by_priority: Vec<PriorityBucket<Item>>,
    placement: HashMap<Item::Key, Placed>,
    pending_per_group: HashMap<Item::Group, GroupState>,
    serving_priority: usize,
    turns_left: usize,
    /// Starts on the first refusal at the sender limit.
    at_cap_since_ns: Option<Timestamp>,
}

impl<Item: QueueItem> SenderQueue<Item> {
    fn new() -> Self {
        let levels = Item::PRIORITY_LEVELS.max(1);
        Self {
            entries_by_priority: (0..levels).map(|_| BTreeMap::new()).collect(),
            placement: HashMap::new(),
            pending_per_group: HashMap::new(),
            serving_priority: 0,
            turns_left: turns_at_priority(0, levels),
            at_cap_since_ns: None,
        }
    }

    fn len(&self) -> usize {
        self.entries_by_priority.iter().map(BTreeMap::len).sum()
    }

    fn is_empty(&self) -> bool {
        self.entries_by_priority.iter().all(BTreeMap::is_empty)
    }

    fn pending(&self, group: &Item::Group) -> usize {
        self.pending_per_group
            .get(group)
            .map_or(0, |state| state.pending)
    }

    /// Start group pressure on refusal, independent of entry age.
    fn group_pressure_since(&mut self, group: &Item::Group, now_ns: Timestamp) -> Timestamp {
        *self
            .pending_per_group
            .entry(*group)
            .or_default()
            .full_since_ns
            .get_or_insert(now_ns)
    }

    /// Start sender pressure on refusal, since its capacity share can change.
    fn sender_pressure_since(&mut self, now_ns: Timestamp) -> Timestamp {
        *self.at_cap_since_ns.get_or_insert(now_ns)
    }

    fn insert(&mut self, entry: Entry<Item>) {
        let priority = entry
            .item
            .priority()
            .min(self.entries_by_priority.len().saturating_sub(1));
        let key = entry.item.key();
        let group = self
            .pending_per_group
            .entry(entry.item.group())
            .or_default();
        group.pending += 1;
        self.at_cap_since_ns = None;
        self.placement.insert(
            key.clone(),
            Placed {
                priority,
                expires_at_ns: entry.expires_at_ns,
            },
        );
        self.entries_by_priority[priority].insert((entry.expires_at_ns, key), entry);
    }

    fn remove(
        &mut self,
        priority: usize,
        expires_at_ns: Timestamp,
        key: &Item::Key,
        max_pending_per_group: usize,
    ) -> Option<Entry<Item>> {
        let entry = self
            .entries_by_priority
            .get_mut(priority)?
            .remove(&(expires_at_ns, key.clone()))?;
        self.placement.remove(key);
        let group = entry.item.group();
        if let Some(state) = self.pending_per_group.get_mut(&group) {
            state.pending = state.pending.saturating_sub(1);
            if state.pending == 0 {
                self.pending_per_group.remove(&group);
            } else if state.pending < max_pending_per_group {
                state.full_since_ns = None;
            }
        }
        Some(entry)
    }

    /// Replace the queued item and deadline, keeping its original arrival time.
    fn refresh(
        &mut self,
        key: &Item::Key,
        item: Item,
        candidate_ns: Timestamp,
        max_lifetime_ns: u64,
    ) {
        let Some(placed) = self.placement.get(key).copied() else {
            return;
        };
        let Some(mut entry) = self
            .entries_by_priority
            .get_mut(placed.priority)
            .and_then(|bucket| bucket.remove(&(placed.expires_at_ns, key.clone())))
        else {
            return;
        };
        entry.expires_at_ns =
            candidate_ns.min(entry.received_at_ns.saturating_add(max_lifetime_ns));
        entry.item = item;
        // The key is unchanged, so the group count stands and only the lane can move.
        let priority = entry
            .item
            .priority()
            .min(self.entries_by_priority.len().saturating_sub(1));
        self.placement.insert(
            key.clone(),
            Placed {
                priority,
                expires_at_ns: entry.expires_at_ns,
            },
        );
        self.entries_by_priority[priority].insert((entry.expires_at_ns, key.clone()), entry);
    }

    /// Advance priorities after their allotted turns, skipping empty buckets.
    fn next_to_take(&mut self) -> Option<(usize, Timestamp, Item::Key)> {
        let levels = self.entries_by_priority.len().max(1);
        // Include one extra pass when the current priority has no turns left.
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
    /// Preserve the sender rotation across batches.
    next_sender: Option<Sender>,
    stored_total: usize,

    /// Exclude time spent empty from stall detection.
    nonempty_since_ns: Option<Timestamp>,
    /// Queue pressure starts when full and resets below `pressure_cleared_below`.
    at_capacity_since_ns: Option<Timestamp>,
    last_taken_ns: Timestamp,

    discarded_expired: u64,
    folded_duplicates: u64,
    rejected_group_full: u64,
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
            rejected_group_full: 0,
            dropped_already_expired: 0,
        }
    }

    /// Return one outcome per input item, in input order.
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
            let answer = |admission| Admitted {
                key: key.clone(),
                admission,
            };

            let expires_at_ns = self.expiry_for(now_ns, &item);
            if expires_at_ns <= now_ns {
                self.dropped_already_expired += 1;
                admissions.push(answer(Admission::Dropped));
                continue;
            }

            if self.holds(&sender, &key) {
                self.folded_duplicates += 1;
                self.refresh(&sender, &key, item, expires_at_ns);
                admissions.push(answer(Admission::Folded));
                continue;
            }

            if self.pending_for(&sender, &item.group()) >= self.config.max_pending_per_group {
                self.rejected_group_full += 1;
                let since_ns = self.group_pressure_since(&sender, &item.group(), now_ns);
                let retry_after_ms = self.retry_after_ms(Some(since_ns), now_ns);
                admissions.push(answer(Admission::Full { retry_after_ms }));
                continue;
            }

            // Reclaim expired entries before refusing capacity, once per admission call.
            if !swept && (self.is_full() || self.sender_at_cap(&sender)) {
                self.discard_expired(now_ns);
                swept = true;
            }

            if self.is_full() || self.sender_at_cap(&sender) {
                // Sender pressure can persist while the queue still has room.
                let since_ns = match self.at_capacity_since_ns {
                    Some(since_ns) => since_ns,
                    None => self.sender_pressure_since(&sender, now_ns),
                };
                let retry_after_ms = self.retry_after_ms(Some(since_ns), now_ns);
                admissions.push(answer(Admission::Full { retry_after_ms }));
                continue;
            }

            self.insert(
                &sender,
                Entry {
                    received_at_ns: now_ns,
                    expires_at_ns,
                    item,
                },
            );
            admissions.push(answer(Admission::Accepted));
        }

        admissions
    }

    fn remove_batch(&mut self, limit: usize, now_ns: Timestamp) -> Vec<Taken<Sender, Item>> {
        let mut taken = Vec::new();

        while taken.len() < limit {
            let Some(sender) = self.advance_cursor() else {
                break;
            };
            // Each iteration takes an entry or removes a sender with no live entries.
            if let Some(entry) = self.pop_live(&sender, now_ns) {
                taken.push(Taken { sender, entry });
            }
        }

        if !taken.is_empty() {
            self.last_taken_ns = now_ns;
        }
        taken
    }

    /// Remove up to `limit` live entries if synchronous `store` succeeds.
    /// On error, restore entries with their original timestamps and restore queue
    /// pressure and progress timers. Scheduling cursors are not rolled back.
    /// Expired entries do not count toward the limit; zero skips queue processing.
    pub(crate) fn take_batch<T, E>(
        &mut self,
        limit: usize,
        now_ns: Timestamp,
        store: impl FnOnce(&[Taken<Sender, Item>]) -> Result<T, E>,
    ) -> Result<T, E> {
        let nonempty_since_ns = self.nonempty_since_ns;
        let at_capacity_since_ns = self.at_capacity_since_ns;
        let last_taken_ns = self.last_taken_ns;

        let batch = self.remove_batch(limit, now_ns);
        match store(&batch) {
            Ok(stored) => Ok(stored),
            Err(error) => {
                for taken in batch {
                    self.insert(&taken.sender, taken.entry);
                }
                self.nonempty_since_ns = nonempty_since_ns;
                self.at_capacity_since_ns = at_capacity_since_ns;
                self.last_taken_ns = last_taken_ns;
                Err(error)
            }
        }
    }

    /// Iterate over stored entries with their senders; order is unspecified.
    pub(crate) fn entries(&self) -> impl Iterator<Item = (&Sender, &Entry<Item>)> {
        self.senders.iter().flat_map(|(sender, queue)| {
            queue
                .entries_by_priority
                .iter()
                .flat_map(|bucket| bucket.values())
                .map(move |entry| (sender, entry))
        })
    }

    /// Restore entries and scheduling cursors, resetting retry timers and metrics.
    /// Keep entries even if the new capacity is lower; reject new items until space frees up.
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
            rejected_group_full: self.rejected_group_full,
            dropped_already_expired: self.dropped_already_expired,
        }
    }

    fn expiry_for(&self, received_at_ns: Timestamp, item: &Item) -> Timestamp {
        let limit = received_at_ns.saturating_add(self.config.discard_entries_after_ns);
        item.expires_at_ns()
            .map_or(limit, |chosen| chosen.min(limit))
    }

    fn holds(&self, sender: &Sender, key: &Item::Key) -> bool {
        self.senders
            .get(sender)
            .is_some_and(|queue| queue.placement.contains_key(key))
    }

    fn refresh(&mut self, sender: &Sender, key: &Item::Key, item: Item, candidate_ns: Timestamp) {
        let max_lifetime_ns = self.config.max_lifetime_ns;
        if let Some(queue) = self.senders.get_mut(sender) {
            queue.refresh(key, item, candidate_ns, max_lifetime_ns);
        }
    }

    fn pending_for(&self, sender: &Sender, group: &Item::Group) -> usize {
        self.senders
            .get(sender)
            .map_or(0, |queue| queue.pending(group))
    }

    fn group_pressure_since(
        &mut self,
        sender: &Sender,
        group: &Item::Group,
        now_ns: Timestamp,
    ) -> Timestamp {
        self.senders
            .entry(sender.clone())
            .or_insert_with(SenderQueue::new)
            .group_pressure_since(group, now_ns)
    }

    fn sender_pressure_since(&mut self, sender: &Sender, now_ns: Timestamp) -> Timestamp {
        self.senders
            .entry(sender.clone())
            .or_insert_with(SenderQueue::new)
            .sender_pressure_since(now_ns)
    }

    fn is_full(&self) -> bool {
        self.stored_total >= self.config.max_entries
    }

    fn sender_at_cap(&self, sender: &Sender) -> bool {
        let held = self.senders.get(sender).map_or(0, SenderQueue::len);
        held >= self.sender_cap()
    }

    /// Share capacity equally without evicting entries when the share shrinks.
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

    /// Discard expired entries before they consume priority turns.
    fn pop_live(&mut self, sender: &Sender, now_ns: Timestamp) -> Option<Entry<Item>> {
        self.discard_expired_for(sender, now_ns);
        self.pop_front(sender)
    }

    fn pop_front(&mut self, sender: &Sender) -> Option<Entry<Item>> {
        let max_pending_per_group = self.config.max_pending_per_group;
        let queue = self.senders.get_mut(sender)?;
        let (priority, expires_at_ns, key) = queue.next_to_take()?;
        let entry = queue.remove(priority, expires_at_ns, &key, max_pending_per_group)?;
        let emptied = queue.is_empty();
        self.after_removal(sender, emptied);
        Some(entry)
    }

    fn discard_expired(&mut self, now_ns: Timestamp) {
        for sender in self.senders.keys().cloned().collect::<Vec<_>>() {
            self.discard_expired_for(&sender, now_ns);
        }
    }

    fn discard_expired_for(&mut self, sender: &Sender, now_ns: Timestamp) {
        let max_pending_per_group = self.config.max_pending_per_group;
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
                queue.remove(priority, expires_at_ns, &key, max_pending_per_group);
                let emptied = queue.is_empty();
                self.after_removal(sender, emptied);
                self.discarded_expired += 1;
            }
        }
    }

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

    /// Resume at the next remaining sender if the saved cursor was removed.
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

    fn retry_after_ms(&self, pressure_since_ns: Option<Timestamp>, now_ns: Timestamp) -> u32 {
        let retry = &self.config.retry;

        // Exclude time spent empty from stall detection.
        let waiting_since_ns = self
            .last_taken_ns
            .max(self.nonempty_since_ns.unwrap_or(now_ns));
        if now_ns.saturating_sub(waiting_since_ns) > retry.stalled_after_silence_ns {
            return retry.when_stalled_ms;
        }

        let under_pressure_for_ns = now_ns.saturating_sub(pressure_since_ns.unwrap_or(now_ns));
        let doublings = (under_pressure_for_ns / retry.doubles_every_ns.max(1))
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
    use std::collections::HashSet;

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
            max_lifetime_ns: 250,
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

    /// Admit items one nanosecond apart to give them distinct deadlines.
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
            let placed: HashSet<(u8, u8)> = queue.placement.keys().copied().collect();
            assert_eq!(placed, keys, "placement drifted for sender {sender}");

            for (key, placed) in &queue.placement {
                let found = queue
                    .entries_by_priority
                    .get(placed.priority)
                    .and_then(|bucket| bucket.get(&(placed.expires_at_ns, *key)));
                assert!(
                    found.is_some(),
                    "placement points nowhere for {key:?} of sender {sender}"
                );
            }

            let mut groups: HashMap<u8, usize> = HashMap::new();
            for (group, _) in &keys {
                *groups.entry(*group).or_insert(0) += 1;
            }
            let pending_counts: HashMap<u8, usize> = queue
                .pending_per_group
                .iter()
                .map(|(group, state)| (*group, state.pending))
                .collect();
            assert_eq!(
                pending_counts, groups,
                "pending_per_group drifted for sender {sender}"
            );

            for (group, state) in &queue.pending_per_group {
                assert!(
                    state.full_since_ns.is_none()
                        || state.pending >= backlog.config.max_pending_per_group,
                    "group {group} kept a pressure clock below its cap"
                );
            }
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

    #[test]
    fn a_resend_moves_the_queued_entry_past_its_first_deadline() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        assert_eq!(
            results(backlog.admit(1, vec![item(1, 1)], 90)),
            vec![Admission::Folded]
        );

        let taken = backlog.remove_batch(1, 150);
        assert_eq!(keys_taken(&taken), vec![(1, 1)]);
        assert_eq!(taken[0].entry.expires_at_ns, 190);
        assert_eq!(taken[0].entry.received_at_ns, 1);
    }

    #[test]
    fn resending_cannot_keep_an_entry_alive_for_ever() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        for now_ns in (50..=250).step_by(50) {
            backlog.admit(1, vec![item(1, 1)], now_ns);
        }

        // Lifetime is capped at 251ns; the last resend would otherwise expire at 350ns.
        assert!(backlog.remove_batch(1, 252).is_empty());
        assert_eq!(backlog.stats(252).discarded_expired, 1);
        assert_consistent(&backlog);
    }

    #[test]
    fn a_resend_may_shorten_a_queued_entrys_life() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        backlog.admit(1, vec![expiring(1, 1, 50)], 2);

        assert_eq!(keys_taken(&backlog.remove_batch(1, 40)), vec![(1, 1)]);
    }

    #[test]
    fn a_shortened_resend_expires_on_its_new_deadline() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.admit(1, vec![expiring(1, 1, 50)], 2);

        // Nanosecond 101 was the deadline it would have kept without the resend.
        assert!(backlog.remove_batch(1, 60).is_empty());
        assert_eq!(backlog.stats(60).discarded_expired, 1);
        assert_consistent(&backlog);
    }

    #[test]
    fn a_resend_moves_the_entry_into_the_lane_it_now_asks_for() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![at_priority(1, 1, 1)], 1);
        backlog.admit(1, vec![at_priority(2, 2, 1)], 2);

        // Both sat in the same lane, (2, 2) ahead on the nearer deadline.
        backlog.admit(1, vec![at_priority(1, 1, 0)], 3);

        assert_eq!(
            keys_taken(&backlog.remove_batch(2, 4)),
            vec![(1, 1), (2, 2)]
        );
        assert_consistent(&backlog);
    }

    #[test]
    fn a_refreshed_entry_moves_behind_a_nearer_deadline() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.admit(1, vec![item(2, 2)], 2);

        backlog.admit(1, vec![item(1, 1)], 10);

        assert_eq!(
            keys_taken(&backlog.remove_batch(2, 20)),
            vec![(2, 2), (1, 1)]
        );
        assert_consistent(&backlog);
    }

    #[test]
    fn a_sender_at_its_own_ceiling_is_sent_away_for_longer_each_time() {
        // Two senders share eight slots, giving each a limit of four.
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(2, vec![item(9, 9)], 1);
        admit_each(
            &mut backlog,
            1,
            vec![item(1, 1), item(2, 2), item(3, 3), item(4, 4)],
            1,
        );

        assert_eq!(sender_hint_at(&mut backlog, 5), 1_000);
        assert_eq!(sender_hint_at(&mut backlog, 25), 4_000);

        assert!(!backlog.is_full());
        assert_eq!(backlog.stats(25).at_capacity_for_ns, None);
    }

    #[test]
    fn an_item_may_ask_to_die_before_the_queues_own_limit() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![expiring(1, 1, 11)], 1);

        // The queue deadline would be 101ns without the item deadline.
        assert!(backlog.remove_batch(10, 12).is_empty());
        assert_eq!(backlog.stats(12).discarded_expired, 1);
        assert_consistent(&backlog);
    }

    #[test]
    fn an_item_may_not_ask_to_outlive_the_queues_own_limit() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![expiring(1, 1, 10_000)], 1);

        assert_eq!(keys_taken(&backlog.remove_batch(10, 100)), vec![(1, 1)]);

        backlog.admit(1, vec![expiring(2, 2, 10_000)], 1);
        assert!(backlog.remove_batch(10, 101).is_empty());
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

        assert_eq!(
            keys_taken(&backlog.remove_batch(10, 3)),
            vec![(2, 2), (1, 1)]
        );
        assert_consistent(&backlog);
    }

    #[test]
    fn stats_report_the_time_left_on_the_nearest_deadline() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![expiring(1, 1, 90)], 1);
        backlog.admit(2, vec![expiring(2, 2, 20)], 1);

        assert_eq!(backlog.stats(5).next_expiry_in_ns, Some(15));
    }

    #[test]
    fn takes_in_arrival_order_at_the_same_priority() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(2, 2), item(3, 3)], 1);

        let taken = backlog.remove_batch(10, 5);

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

        let taken = backlog.remove_batch(10, 5);

        assert_eq!(keys_taken(&taken), vec![(3, 3), (1, 1), (2, 2)]);
    }

    #[test]
    fn a_busy_high_priority_still_hands_turns_to_the_low_one() {
        // Priority 0 gets two turns; priority 1 gets one.
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
            keys_taken(&backlog.remove_batch(10, 7)),
            vec![(1, 1), (2, 2), (4, 4), (3, 3), (5, 5), (6, 6)]
        );
        assert_consistent(&backlog);
    }

    #[test]
    fn an_out_of_range_priority_is_clamped_rather_than_panicking() {
        let mut backlog = TestQueue::new(config(), 0);

        backlog.admit(1, vec![at_priority(1, 1, 99)], 1);

        assert_eq!(keys_taken(&backlog.remove_batch(10, 2)), vec![(1, 1)]);
    }

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
    fn refuses_once_a_group_is_at_its_cap() {
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
            vec![
                Admission::Accepted,
                Admission::Accepted,
                Admission::Full {
                    retry_after_ms: 1_000
                }
            ]
        );
        assert_eq!(backlog.stats(1).rejected_group_full, 1);
        assert_consistent(&backlog);
    }

    #[test]
    fn a_group_that_stays_full_sends_the_sender_away_for_longer() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_pending_per_group: 2,
                ..config()
            },
            0,
        );
        backlog.admit(1, vec![item(7, 1), item(7, 2)], 1);

        assert_eq!(group_hint_at(&mut backlog, 1), 1_000);
        assert_eq!(group_hint_at(&mut backlog, 21), 4_000);

        assert_eq!(backlog.stats(21).at_capacity_for_ns, None);
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

        backlog.remove_batch(1, 3);

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
        assert_eq!(keys_taken(&backlog.remove_batch(1, 101)), vec![(1, 1)]);
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
        assert_eq!(keys_taken(&backlog.remove_batch(1, 101)), vec![(1, 2)]);
        assert_consistent(&backlog);
    }

    #[test]
    fn takes_one_sender_at_a_time_in_turn() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(1, 2), item(1, 3)], 1);
        backlog.admit(2, vec![item(2, 1)], 1);
        admit_each(&mut backlog, 3, vec![item(3, 1), item(3, 2)], 1);

        let taken = backlog.remove_batch(5, 5);

        assert_eq!(senders_taken(&taken), vec![1, 2, 3, 1, 3]);
        assert_eq!(backlog.stored_total, 1);
    }

    #[test]
    fn the_cursor_resumes_where_the_last_batch_stopped() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(1, 2)], 1);
        admit_each(&mut backlog, 2, vec![item(2, 1), item(2, 2)], 1);

        let first = backlog.remove_batch(1, 5);
        let second = backlog.remove_batch(1, 6);
        let third = backlog.remove_batch(1, 7);

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
        assert_eq!(backlog.stored_total, 3);
    }

    #[test]
    fn an_incumbent_over_its_share_is_refused_while_a_newcomer_is_admitted() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(
            &mut backlog,
            1,
            vec![item(1, 1), item(2, 2), item(3, 3), item(4, 4), item(5, 5)],
            1,
        );
        backlog.admit(2, vec![item(1, 1)], 6);

        // Sender 1 keeps its five entries even though its share is now four.
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

        backlog.remove_batch(1, 2);

        assert_eq!(backlog.stats(2).active_senders, 1);
        assert_consistent(&backlog);
    }

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

        // Full since 1ns, with a doubling every 10ns.
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

        // Three doublings reach the 8,000ms ceiling.
        assert_eq!(hint_at(&mut backlog, 41), 8_000);
        assert_eq!(hint_at(&mut backlog, 45), 8_000);
    }

    #[test]
    fn the_hint_jumps_once_nothing_is_draining() {
        let mut backlog = full_queue(1);
        backlog.admit(1, vec![item(9, 9)], 1);

        for tick in 2..60 {
            assert!(backlog.remove_batch(0, tick).is_empty());
        }

        assert_eq!(hint_at(&mut backlog, 60), 60_000);
    }

    #[test]
    fn an_idle_buffer_filled_in_one_call_is_not_stalled() {
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

        // Three removals bring occupancy below the reset threshold of six.
        backlog.remove_batch(3, 2);

        assert!(backlog.stats(2).at_capacity_for_ns.is_none());
        assert_consistent(&backlog);
    }

    #[test]
    fn taking_nothing_leaves_the_silence_clock_alone() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        assert!(backlog.remove_batch(0, 30).is_empty());

        assert_eq!(backlog.stats(30).silence_ns, 30);
        assert_eq!(backlog.stored_total, 1);
    }

    #[test]
    fn an_expired_entry_is_discarded_rather_than_taken() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        let taken = backlog.remove_batch(10, 101);

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

        let admissions = results(backlog.admit(1, vec![item(3, 3)], 200));

        assert_eq!(admissions, vec![Admission::Accepted]);
        assert_eq!(backlog.stats(200).discarded_expired, 2);
        assert_consistent(&backlog);
    }

    #[test]
    fn a_stored_batch_is_taken_for_good() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1), item(2, 2)], 1);

        let keys = backlog
            .take_batch(2, 30, |batch| {
                Ok::<_, ()>(batch.iter().map(|t| t.entry.item.key()).collect::<Vec<_>>())
            })
            .unwrap();

        assert_eq!(keys, vec![(1, 1), (2, 2)]);
        assert_eq!(backlog.stored_total, 0);
        assert_eq!(backlog.stats(30).silence_ns, 0, "a take is progress");
        assert_consistent(&backlog);
    }

    #[test]
    fn a_batch_the_caller_cannot_store_stays_in_the_queue() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1), item(2, 2)], 1);

        let outcome = backlog.take_batch(2, 30, |batch| {
            assert_eq!(batch.len(), 2, "the batch reaches the caller either way");
            Err::<(), _>("no room downstream")
        });

        assert_eq!(outcome, Err("no room downstream"));
        assert_eq!(backlog.stored_total, 2);
        assert_eq!(backlog.stats(30).silence_ns, 30);
        assert_consistent(&backlog);

        let retried = backlog.remove_batch(2, 40);
        assert_eq!(keys_taken(&retried), vec![(1, 1), (2, 2)]);
        assert_eq!(retried[0].entry.received_at_ns, 1);
        assert_eq!(retried[0].entry.expires_at_ns, 101);
    }

    #[test]
    fn a_batch_that_fills_the_queue_sweeps_and_carries_on() {
        let mut backlog = TestQueue::new(config(), 0);
        // Only the capacity sweep reaches these expired entries from another sender.
        admit_each(
            &mut backlog,
            2,
            vec![item(1, 1), item(2, 2), item(3, 3), item(4, 4)],
            1,
        );

        let batch = (1..=6).map(|id| item(5, id)).collect();
        let admissions = results(backlog.admit(1, batch, 150));

        // Capacity is reached mid-batch, so sweeping only before the loop would miss it.
        assert_eq!(admissions, vec![Admission::Accepted; 6]);
        assert_eq!(backlog.stats(150).discarded_expired, 4);
        assert_consistent(&backlog);
    }

    #[test]
    fn a_live_entry_behind_an_expired_one_still_leaves() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.admit(1, vec![item(2, 2)], 150);

        let taken = backlog.remove_batch(10, 151);

        assert_eq!(keys_taken(&taken), vec![(2, 2)]);
        assert_eq!(backlog.stats(151).discarded_expired, 1);
    }

    #[test]
    fn expired_senders_do_not_stop_a_batch_before_live_senders() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.admit(2, vec![item(2, 2)], 1);
        backlog.admit(3, vec![item(3, 3)], 50);

        let taken = backlog.remove_batch(1, 101);

        assert_eq!(senders_taken(&taken), vec![3]);
        assert_eq!(backlog.stats(101).discarded_expired, 2);
        assert_eq!(backlog.stored_total, 0);
        assert_consistent(&backlog);
    }

    #[test]
    fn every_item_gets_exactly_one_answer() {
        let mut queue = TestQueue::new(
            QueueConfig {
                max_pending_per_group: 2,
                ..config()
            },
            0,
        );
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

        let batch = vec![item(7, 1), item(7, 3), item(8, 8), item(9, 9)];
        let answers = queue.admit(1, batch, 20);

        assert_eq!(
            answers,
            vec![
                Admitted {
                    key: (7, 1),
                    admission: Admission::Folded
                },
                // Group pressure starts here; queue pressure has been running since 8ns.
                Admitted {
                    key: (7, 3),
                    admission: Admission::Full {
                        retry_after_ms: 1_000
                    }
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
        assert_eq!(queue.remove_batch(10, 2).len(), 1);
    }

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

    fn results<Key>(admitted: Vec<Admitted<Key>>) -> Vec<Admission> {
        admitted
            .into_iter()
            .map(|answer| answer.admission)
            .collect()
    }

    fn sender_hint_at(backlog: &mut TestQueue, now_ns: Timestamp) -> u32 {
        match backlog
            .admit(1, vec![item(8, 8)], now_ns)
            .remove(0)
            .admission
        {
            Admission::Full { retry_after_ms } => retry_after_ms,
            other => panic!("expected a full sender, got {other:?}"),
        }
    }

    fn group_hint_at(backlog: &mut TestQueue, now_ns: Timestamp) -> u32 {
        match backlog
            .admit(1, vec![item(7, 9)], now_ns)
            .remove(0)
            .admission
        {
            Admission::Full { retry_after_ms } => retry_after_ms,
            other => panic!("expected a full group, got {other:?}"),
        }
    }

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
