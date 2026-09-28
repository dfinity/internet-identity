//! Bounded queue with per-sender, per-lane and per-group limits.
//!
//! Senders take turns; within each sender, lanes take the turns the item type sets.
//! A lane holds at most its turns' share of the sender cap, counting only the lanes
//! busy within the expiry window, which is what it can drain before its entries expire.
//! Entries in a lane are ordered by deadline, then key, so an entry due sooner is
//! admitted only if what is due before it drains in time.
//!
//! Deduplication covers queued entries only. Resends can extend a deadline up to
//! the entry's maximum lifetime. Callers supply trusted canister time and handle
//! authorization and delivery.

use internet_identity_interface::internet_identity::types::Timestamp;
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;
use std::ops::Bound::{Excluded, Unbounded};

pub(crate) trait QueueItem: Clone {
    /// Deduplication key within one sender.
    type Key: Clone + Eq + Hash + Ord;

    /// Items in the same group share a per-sender pending limit. Equal keys must name
    /// the same group.
    type Group: Copy + Eq + Hash;

    /// Turns each lane gets per round, most urgent first; empty is one lane, zero is one turn.
    const TURNS: &'static [usize];

    fn key(&self) -> Self::Key;
    fn group(&self) -> Self::Group;

    /// Lane index into `TURNS`. Out of range uses the last lane.
    fn lane(&self) -> usize;

    /// Optional submitter deadline, capped by the queue expiry limit.
    fn expires_at_ns(&self) -> Option<Timestamp> {
        None
    }
}

fn lane_count<Item: QueueItem>() -> usize {
    Item::TURNS.len().max(1)
}

fn turns_for_lane<Item: QueueItem>(lane: usize) -> usize {
    Item::TURNS.get(lane).copied().unwrap_or(1).max(1)
}

fn lane_of<Item: QueueItem>(item: &Item) -> usize {
    item.lane().min(lane_count::<Item>() - 1)
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
    pub(crate) base_ns: u64,
    /// Retry ceiling while the queue is making progress. Also what bounds the
    /// doubling, so there is no separate cap on how often the delay may double.
    pub(crate) ceiling_ns: u64,
    /// Retry delay after `stalled_after_silence_ns` without progress.
    pub(crate) when_stalled_ns: u64,
    /// Interval of sustained pressure between delay doublings.
    pub(crate) doubles_every_ns: u64,
    pub(crate) stalled_after_silence_ns: u64,
}

impl RetryPolicy {
    const fn is_coherent(&self) -> bool {
        self.base_ns > 0
            && self.base_ns <= self.ceiling_ns
            && self.ceiling_ns <= self.when_stalled_ns
            && self.doubles_every_ns > 0
            && self.stalled_after_silence_ns > 0
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Admission {
    Accepted,
    /// Duplicate key; the resend replaces the queued entry, keeping its arrival time.
    Folded,
    /// Already expired on arrival.
    Dropped,
    /// Queue, sender, lane, or group capacity reached.
    Full {
        retry_after_ns: u64,
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
    pub(crate) rejected_lane_full: u64,
    pub(crate) dropped_already_expired: u64,
}

/// Locates an entry by key without scanning the lanes.
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
    /// Per lane, starts on the first refusal at the lane cap.
    lane_full_since_ns: Vec<Option<Timestamp>>,
}

impl<Item: QueueItem> SenderQueue<Item> {
    fn new() -> Self {
        Self {
            entries_by_priority: (0..lane_count::<Item>()).map(|_| BTreeMap::new()).collect(),
            placement: HashMap::new(),
            pending_per_group: HashMap::new(),
            serving_priority: 0,
            turns_left: turns_for_lane::<Item>(0),
            at_cap_since_ns: None,
            lane_full_since_ns: vec![None; lane_count::<Item>()],
        }
    }

    fn len(&self) -> usize {
        self.entries_by_priority.iter().map(BTreeMap::len).sum()
    }

    fn is_empty(&self) -> bool {
        self.entries_by_priority.iter().all(BTreeMap::is_empty)
    }

    fn lane_len(&self, lane: usize) -> usize {
        self.entries_by_priority.get(lane).map_or(0, BTreeMap::len)
    }

    /// Entries ordered before the given position, counted up to `limit`.
    fn count_due_before(
        &self,
        lane: usize,
        expires_at_ns: Timestamp,
        key: &Item::Key,
        limit: usize,
    ) -> usize {
        self.entries_by_priority.get(lane).map_or(0, |bucket| {
            bucket
                .range(..(expires_at_ns, key.clone()))
                .take(limit)
                .count()
        })
    }

    fn pending_in_group(&self, group: &Item::Group) -> usize {
        self.pending_per_group
            .get(group)
            .map_or(0, |state| state.pending)
    }

    /// Start group pressure on refusal, independent of entry age.
    fn record_group_refusal(&mut self, group: &Item::Group, now_ns: Timestamp) -> Timestamp {
        *self
            .pending_per_group
            .entry(*group)
            .or_default()
            .full_since_ns
            .get_or_insert(now_ns)
    }

    /// Start sender pressure on refusal, since its capacity share can change.
    fn record_sender_refusal(&mut self, now_ns: Timestamp) -> Timestamp {
        *self.at_cap_since_ns.get_or_insert(now_ns)
    }

    fn record_lane_refusal(&mut self, lane: usize, now_ns: Timestamp) -> Timestamp {
        self.lane_full_since_ns
            .get_mut(lane)
            .map_or(now_ns, |since| *since.get_or_insert(now_ns))
    }

    fn add_admitted_entry(&mut self, entry: Entry<Item>) {
        let lane = self.add_entry(entry);
        self.at_cap_since_ns = None;
        self.lane_full_since_ns[lane] = None;
    }

    /// Store an entry without touching the admission clocks, returning its lane.
    fn add_entry(&mut self, entry: Entry<Item>) -> usize {
        let priority = lane_of(&entry.item);
        let key = entry.item.key();
        let group = self
            .pending_per_group
            .entry(entry.item.group())
            .or_default();
        group.pending += 1;
        self.placement.insert(
            key.clone(),
            Placed {
                priority,
                expires_at_ns: entry.expires_at_ns,
            },
        );
        self.entries_by_priority[priority].insert((entry.expires_at_ns, key), entry);
        priority
    }

    fn remove_entry(
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
    fn replace_entry(
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
        let priority = lane_of(&entry.item);
        self.placement.insert(
            key.clone(),
            Placed {
                priority,
                expires_at_ns: entry.expires_at_ns,
            },
        );
        self.entries_by_priority[priority].insert((entry.expires_at_ns, key.clone()), entry);
    }

    /// Advance lanes after their allotted turns, skipping empty ones.
    fn take_lane_turn(&mut self) -> Option<(usize, Timestamp, Item::Key)> {
        let levels = self.entries_by_priority.len().max(1);
        // Include one extra pass when the current lane has no turns left.
        for _ in 0..=levels {
            if self.turns_left == 0 {
                self.serving_priority = (self.serving_priority + 1) % levels;
                self.turns_left = turns_for_lane::<Item>(self.serving_priority);
            }
            match self.first_in_lane(self.serving_priority) {
                Some((expires_at_ns, key)) => {
                    self.turns_left = self.turns_left.saturating_sub(1);
                    return Some((self.serving_priority, expires_at_ns, key));
                }
                None => self.turns_left = 0,
            }
        }
        None
    }

    fn first_in_lane(&self, priority: usize) -> Option<(Timestamp, Item::Key)> {
        self.entries_by_priority
            .get(priority)?
            .first_key_value()
            .map(|((expires_at_ns, key), _)| (*expires_at_ns, key.clone()))
    }

    fn earliest_expiry_ns(&self) -> Option<Timestamp> {
        self.entries_by_priority
            .iter()
            .filter_map(|entries| entries.first_key_value().map(|((ts, _), _)| *ts))
            .min()
    }
}

pub(crate) struct AdmissionQueue<Sender: Clone + Ord, Item: QueueItem> {
    config: QueueConfig,
    senders: BTreeMap<Sender, SenderQueue<Item>>,
    /// When each sender's lanes last held an entry, kept for one expiry window after
    /// the sender empties.
    lane_held_ns: BTreeMap<Sender, Vec<Option<Timestamp>>>,
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
    rejected_lane_full: u64,
    dropped_already_expired: u64,
}

impl<Sender: Clone + Ord, Item: QueueItem> AdmissionQueue<Sender, Item> {
    pub(crate) fn new(config: QueueConfig, now_ns: Timestamp) -> Self {
        Self {
            config,
            senders: BTreeMap::new(),
            lane_held_ns: BTreeMap::new(),
            next_sender: None,
            stored_total: 0,
            nonempty_since_ns: None,
            at_capacity_since_ns: None,
            last_taken_ns: now_ns,
            discarded_expired: 0,
            folded_duplicates: 0,
            rejected_group_full: 0,
            rejected_lane_full: 0,
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
        if items.is_empty() {
            return Vec::new();
        }
        // Discard expired entries for this sender before admitting new ones, so they can take the turns.
        self.discard_sender_expired(&sender, now_ns);

        let mut admissions = Vec::with_capacity(items.len());
        let mut swept = false; // Whether expired entries were discarded once for this admission call.

        for item in items {
            let key = item.key();
            let answer = |admission| Admitted {
                key: key.clone(),
                admission,
            };

            // Reject items that are already expired on arrival, even if the queue has room.
            let expires_at_ns = self.capped_expiry_ns(now_ns, &item);
            if expires_at_ns <= now_ns {
                self.dropped_already_expired += 1;
                admissions.push(answer(Admission::Dropped));
                continue;
            }

            // Fold duplicates: the resend replaces the queued item and deadline. Doesn't affect queue capacity, so it doesn't reset the pressure timer.
            if self.sender_holds_key(&sender, &key) {
                self.folded_duplicates += 1;
                self.replace_queued_entry(&sender, &key, item, expires_at_ns);
                admissions.push(answer(Admission::Folded));
                continue;
            }

            // Reject items that would exceed the per-group pending limit, even if the queue has room.
            if self.pending_in_group(&sender, &item.group()) >= self.config.max_pending_per_group {
                self.rejected_group_full += 1;
                let since_ns = self.record_group_refusal(&sender, &item.group(), now_ns);
                let retry_after_ns = self.retry_after_ns(Some(since_ns), now_ns);
                admissions.push(answer(Admission::Full { retry_after_ns }));
                continue;
            }

            // Reclaim expired entries before refusing capacity, once per admission call.
            if !swept && (self.queue_is_full() || self.sender_is_at_cap(&sender)) {
                self.discard_all_expired(now_ns);
                swept = true;
            }

            // Reject items if the queue or sender is at capacity.
            if self.queue_is_full() || self.sender_is_at_cap(&sender) {
                // Sender pressure can persist while the queue still has room.
                let since_ns = match self.at_capacity_since_ns {
                    Some(since_ns) => since_ns,
                    None => self.record_sender_refusal(&sender, now_ns),
                };
                let retry_after_ns = self.retry_after_ns(Some(since_ns), now_ns);
                admissions.push(answer(Admission::Full { retry_after_ns }));
                continue;
            }

            // Reject items if the lane is at capacity or cannot drain in time before the entry expires.
            let lane = lane_of(&item);
            if !self.lane_has_room_for(&sender, lane, expires_at_ns, &key, now_ns) {
                self.rejected_lane_full += 1;
                let since_ns = self.record_lane_refusal(&sender, lane, now_ns);
                let retry_after_ns = self.retry_after_ns(Some(since_ns), now_ns);
                admissions.push(answer(Admission::Full { retry_after_ns }));
                continue;
            }

            self.add_admitted_entry(
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

    /// Remove up to `limit` live entries if synchronous `store` succeeds.
    /// On error, entries go back with their timestamps and the queue's clocks are restored;
    /// scheduling cursors, group clocks and the state of a sender the take emptied are not.
    pub(crate) fn take_batch<T, E>(
        &mut self,
        limit: usize,
        now_ns: Timestamp,
        store: impl FnOnce(&[Taken<Sender, Item>]) -> Result<T, E>,
    ) -> Result<T, E> {
        // Discard expired entries before taking, so they don't consume turns.
        if limit > 0 {
            self.discard_all_expired(now_ns);
        }
        let nonempty_since_ns = self.nonempty_since_ns;
        let at_capacity_since_ns = self.at_capacity_since_ns;
        let last_taken_ns = self.last_taken_ns;

        // Remove the batch before storing, so the store sees a consistent view of the queue.
        let batch = self.remove_batch(limit, now_ns);
        match store(&batch) {
            Ok(stored) => Ok(stored),
            Err(error) => {
                for taken in batch {
                    self.put_back_entry(&taken.sender, taken.entry);
                }
                self.nonempty_since_ns = nonempty_since_ns;
                self.at_capacity_since_ns = at_capacity_since_ns;
                self.last_taken_ns = last_taken_ns;
                Err(error)
            }
        }
    }
    // Remove up to `limit` live entries, advancing the sender cursor and recording the last take time.
    fn remove_batch(&mut self, limit: usize, now_ns: Timestamp) -> Vec<Taken<Sender, Item>> {
        let mut taken = Vec::new();

        while taken.len() < limit {
            let Some(sender) = self.next_sender_in_rotation() else {
                break;
            };
            // Each iteration takes an entry or removes a sender with no live entries.
            if let Some(entry) = self.take_live_entry(&sender, now_ns) {
                taken.push(Taken { sender, entry });
            }
        }

        if !taken.is_empty() {
            self.last_taken_ns = now_ns;
        }
        self.forget_quiet_lanes(now_ns);
        taken
    }

    pub(crate) fn stats(&self, now_ns: Timestamp) -> QueueStats {
        QueueStats {
            stored_total: self.stored_total,
            active_senders: self.senders.len(),
            next_expiry_in_ns: self
                .senders
                .values()
                .filter_map(SenderQueue::earliest_expiry_ns)
                .min()
                .map(|nearest| nearest.saturating_sub(now_ns)),
            at_capacity_for_ns: self
                .at_capacity_since_ns
                .map(|since| now_ns.saturating_sub(since)),
            silence_ns: now_ns.saturating_sub(self.last_taken_ns),
            discarded_expired: self.discarded_expired,
            folded_duplicates: self.folded_duplicates,
            rejected_group_full: self.rejected_group_full,
            rejected_lane_full: self.rejected_lane_full,
            dropped_already_expired: self.dropped_already_expired,
        }
    }

    fn capped_expiry_ns(&self, received_at_ns: Timestamp, item: &Item) -> Timestamp {
        let limit = received_at_ns.saturating_add(self.config.discard_entries_after_ns);
        item.expires_at_ns()
            .map_or(limit, |chosen| chosen.min(limit))
    }

    fn sender_holds_key(&self, sender: &Sender, key: &Item::Key) -> bool {
        self.senders
            .get(sender)
            .is_some_and(|queue| queue.placement.contains_key(key))
    }

    fn replace_queued_entry(
        &mut self,
        sender: &Sender,
        key: &Item::Key,
        item: Item,
        candidate_ns: Timestamp,
    ) {
        let max_lifetime_ns = self.config.max_lifetime_ns;
        if let Some(queue) = self.senders.get_mut(sender) {
            queue.replace_entry(key, item, candidate_ns, max_lifetime_ns);
        }
    }

    fn pending_in_group(&self, sender: &Sender, group: &Item::Group) -> usize {
        self.senders
            .get(sender)
            .map_or(0, |queue| queue.pending_in_group(group))
    }

    fn record_group_refusal(
        &mut self,
        sender: &Sender,
        group: &Item::Group,
        now_ns: Timestamp,
    ) -> Timestamp {
        self.senders
            .entry(sender.clone())
            .or_insert_with(SenderQueue::new)
            .record_group_refusal(group, now_ns)
    }

    fn record_sender_refusal(&mut self, sender: &Sender, now_ns: Timestamp) -> Timestamp {
        self.senders
            .entry(sender.clone())
            .or_insert_with(SenderQueue::new)
            .record_sender_refusal(now_ns)
    }

    fn queue_is_full(&self) -> bool {
        self.stored_total >= self.config.max_entries
    }

    fn sender_is_at_cap(&self, sender: &Sender) -> bool {
        let held = self.senders.get(sender).map_or(0, SenderQueue::len);
        held >= self.sender_cap()
    }

    /// Share capacity equally without evicting entries when the share shrinks.
    fn sender_cap(&self) -> usize {
        self.config
            .max_entries_per_sender
            .min(self.config.max_entries / self.senders.len().max(1))
    }

    fn lane_len(&self, sender: &Sender, lane: usize) -> usize {
        self.senders
            .get(sender)
            .map_or(0, |queue| queue.lane_len(lane))
    }

    /// Split the sender cap by turns across this lane and those that hold entries or held
    /// one within the expiry window.
    fn lane_cap(&self, sender: &Sender, lane: usize, now_ns: Timestamp) -> usize {
        let window_ns = self.config.discard_entries_after_ns;
        let held_ns = self.lane_held_ns.get(sender);
        let busy_turns: usize = (0..lane_count::<Item>())
            .filter(|&other| {
                other == lane
                    || self.lane_len(sender, other) > 0
                    || held_ns
                        .and_then(|held| held[other])
                        .is_some_and(|at| now_ns.saturating_sub(at) < window_ns)
            })
            .map(turns_for_lane::<Item>)
            .sum();
        (self
            .sender_cap()
            .saturating_mul(turns_for_lane::<Item>(lane))
            / busy_turns.max(1))
        .max(1)
    }

    /// Whether the lane has room, and drains what is due before the entry by its deadline.
    fn lane_has_room_for(
        &self,
        sender: &Sender,
        lane: usize,
        expires_at_ns: Timestamp,
        key: &Item::Key,
        now_ns: Timestamp,
    ) -> bool {
        let cap = self.lane_cap(sender, lane, now_ns);
        let depth = self.lane_len(sender, lane);
        if depth >= cap {
            return false;
        }
        let window_ns = self.config.discard_entries_after_ns.max(1);
        let due_in_ns = expires_at_ns.saturating_sub(now_ns);
        let in_time =
            ((cap as u64).saturating_mul(due_in_ns) / window_ns).clamp(1, cap as u64) as usize;
        depth < in_time
            || self.senders.get(sender).map_or(0, |queue| {
                queue.count_due_before(lane, expires_at_ns, key, in_time)
            }) < in_time
    }

    fn record_lane_refusal(
        &mut self,
        sender: &Sender,
        lane: usize,
        now_ns: Timestamp,
    ) -> Timestamp {
        self.senders
            .get_mut(sender)
            .map_or(now_ns, |queue| queue.record_lane_refusal(lane, now_ns))
    }

    fn record_lane_held(&mut self, sender: &Sender, lane: usize, at_ns: Timestamp) {
        let held = self
            .lane_held_ns
            .entry(sender.clone())
            .or_insert_with(|| vec![None; lane_count::<Item>()]);
        held[lane] = held[lane].max(Some(at_ns));
    }

    fn forget_quiet_lanes(&mut self, now_ns: Timestamp) {
        let window_ns = self.config.discard_entries_after_ns;
        let senders = &self.senders;
        self.lane_held_ns.retain(|sender, held| {
            senders.contains_key(sender)
                || held
                    .iter()
                    .flatten()
                    .any(|at| now_ns.saturating_sub(*at) < window_ns)
        });
    }

    fn add_admitted_entry(&mut self, sender: &Sender, entry: Entry<Item>) {
        let received_at_ns = entry.received_at_ns;
        if self.stored_total == 0 {
            self.nonempty_since_ns = Some(received_at_ns);
        }
        self.senders
            .entry(sender.clone())
            .or_insert_with(SenderQueue::new)
            .add_admitted_entry(entry);
        self.stored_total += 1;
        if self.queue_is_full() {
            self.at_capacity_since_ns.get_or_insert(received_at_ns);
        }
    }

    fn put_back_entry(&mut self, sender: &Sender, entry: Entry<Item>) {
        self.senders
            .entry(sender.clone())
            .or_insert_with(SenderQueue::new)
            .add_entry(entry);
        self.stored_total += 1;
    }

    /// Discard expired entries before they consume lane turns.
    fn take_live_entry(&mut self, sender: &Sender, now_ns: Timestamp) -> Option<Entry<Item>> {
        self.discard_sender_expired(sender, now_ns);
        self.take_next_entry(sender, now_ns)
    }

    fn take_next_entry(&mut self, sender: &Sender, now_ns: Timestamp) -> Option<Entry<Item>> {
        let max_pending_per_group = self.config.max_pending_per_group;
        let queue = self.senders.get_mut(sender)?;
        let (priority, expires_at_ns, key) = queue.take_lane_turn()?;
        let entry = queue.remove_entry(priority, expires_at_ns, &key, max_pending_per_group)?;
        let emptied = queue.is_empty();
        self.account_for_removal(sender, emptied);
        self.record_lane_held(sender, priority, now_ns);
        Some(entry)
    }

    fn discard_all_expired(&mut self, now_ns: Timestamp) {
        for sender in self.senders.keys().cloned().collect::<Vec<_>>() {
            self.discard_sender_expired(&sender, now_ns);
        }
    }

    fn discard_sender_expired(&mut self, sender: &Sender, now_ns: Timestamp) {
        let max_pending_per_group = self.config.max_pending_per_group;
        for priority in 0..lane_count::<Item>() {
            loop {
                let Some(queue) = self.senders.get_mut(sender) else {
                    return;
                };
                let Some((expires_at_ns, key)) = queue.first_in_lane(priority) else {
                    break;
                };
                if expires_at_ns > now_ns {
                    break;
                }
                queue.remove_entry(priority, expires_at_ns, &key, max_pending_per_group);
                let emptied = queue.is_empty();
                self.account_for_removal(sender, emptied);
                self.record_lane_held(sender, priority, expires_at_ns);
                self.discarded_expired += 1;
            }
        }
    }

    fn account_for_removal(&mut self, sender: &Sender, emptied: bool) {
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
    fn next_sender_in_rotation(&mut self) -> Option<Sender> {
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

    fn retry_after_ns(&self, pressure_since_ns: Option<Timestamp>, now_ns: Timestamp) -> u64 {
        let retry = &self.config.retry;

        // Exclude time spent empty from stall detection.
        let waiting_since_ns = self
            .last_taken_ns
            .max(self.nonempty_since_ns.unwrap_or(now_ns));
        if now_ns.saturating_sub(waiting_since_ns) > retry.stalled_after_silence_ns {
            return retry.when_stalled_ns;
        }

        let under_pressure_for_ns = now_ns.saturating_sub(pressure_since_ns.unwrap_or(now_ns));
        let doublings = u32::try_from(under_pressure_for_ns / retry.doubles_every_ns.max(1))
            .unwrap_or(u32::MAX);
        retry
            .base_ns
            .saturating_mul(1u64.checked_shl(doublings).unwrap_or(u64::MAX))
            .min(retry.ceiling_ns)
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
        lane: usize,
        expires_at_ns: Option<Timestamp>,
    }

    impl QueueItem for TestItem {
        type Key = (u8, u8);
        type Group = u8;
        const TURNS: &'static [usize] = &[2, 1];

        fn key(&self) -> Self::Key {
            (self.group, self.id)
        }

        fn group(&self) -> Self::Group {
            self.group
        }

        fn lane(&self) -> usize {
            self.lane
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
            lane: 0,
            expires_at_ns: None,
        }
    }

    fn in_lane(group: u8, id: u8, lane: usize) -> TestItem {
        TestItem {
            lane,
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
                base_ns: 1_000,
                ceiling_ns: 8_000,
                when_stalled_ns: 60_000,
                doubles_every_ns: 10,
                stalled_after_silence_ns: 50,
            },
        }
    }

    /// Nine slots, so busy lanes split them six and three.
    fn roomy_config() -> QueueConfig {
        QueueConfig {
            max_entries: 9,
            max_entries_per_sender: 9,
            ..config()
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
        backlog.admit(1, vec![in_lane(1, 1, 1)], 1);
        backlog.admit(1, vec![in_lane(2, 2, 1)], 2);

        // Both sat in the same lane, (2, 2) ahead on the nearer deadline.
        backlog.admit(1, vec![in_lane(1, 1, 0)], 3);

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

        assert!(!backlog.queue_is_full());
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
            vec![in_lane(1, 1, 1), in_lane(2, 2, 1), in_lane(3, 3, 0)],
            1,
        );

        let taken = backlog.remove_batch(10, 5);

        assert_eq!(keys_taken(&taken), vec![(3, 3), (1, 1), (2, 2)]);
    }

    #[test]
    fn a_busy_high_priority_still_hands_turns_to_the_low_one() {
        // Lane 0 gets two turns; lane 1 gets one.
        let mut backlog = TestQueue::new(roomy_config(), 0);
        admit_each(
            &mut backlog,
            1,
            vec![
                item(1, 1),
                item(2, 2),
                item(3, 3),
                in_lane(4, 4, 1),
                in_lane(5, 5, 1),
                in_lane(6, 6, 1),
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
    fn a_lane_alone_may_hold_the_whole_sender_share() {
        let mut backlog = TestQueue::new(roomy_config(), 0);
        let low: Vec<_> = (1..=9).map(|id| in_lane(id, id, 1)).collect();

        assert_eq!(
            results(backlog.admit(1, low, 1)),
            vec![Admission::Accepted; 9]
        );
    }

    #[test]
    fn busy_lanes_split_the_sender_share_by_their_turns() {
        let mut backlog = TestQueue::new(roomy_config(), 0);
        let low = (2..=5).map(|id| in_lane(id, id, 1));

        // The high item lands first, so the low ones already see its lane busy.
        let admissions =
            results(backlog.admit(1, [item(1, 1)].into_iter().chain(low).collect(), 1));

        assert_eq!(
            admissions,
            vec![
                Admission::Accepted,
                Admission::Accepted,
                Admission::Accepted,
                Admission::Accepted,
                Admission::Full {
                    retry_after_ns: 1_000
                },
            ]
        );
        assert_eq!(backlog.stats(1).rejected_lane_full, 1);
        assert_consistent(&backlog);
    }

    #[test]
    fn a_lane_that_just_emptied_still_holds_its_share_of_turns() {
        let mut backlog = TestQueue::new(roomy_config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.remove_batch(10, 2);
        assert_eq!(backlog.stats(2).active_senders, 0);

        let low: Vec<_> = (1..=4).map(|id| in_lane(id, id, 1)).collect();
        let admissions = results(backlog.admit(1, low, 3));

        assert_eq!(
            admissions,
            vec![
                Admission::Accepted,
                Admission::Accepted,
                Admission::Accepted,
                Admission::Full {
                    retry_after_ns: 1_000
                },
            ]
        );
    }

    #[test]
    fn a_lane_quiet_for_a_whole_window_gives_its_share_back() {
        let mut backlog = TestQueue::new(roomy_config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.remove_batch(10, 2);

        backlog.remove_batch(10, 102);
        assert!(backlog.lane_held_ns.is_empty());

        let low: Vec<_> = (1..=9).map(|id| in_lane(id, id, 1)).collect();
        assert_eq!(
            results(backlog.admit(1, low, 102)),
            vec![Admission::Accepted; 9]
        );
    }

    #[test]
    fn an_expired_lane_counts_as_busy_until_its_deadline_not_its_discard() {
        let mut backlog = TestQueue::new(roomy_config(), 0);
        backlog.admit(1, vec![expiring(1, 1, 10)], 1);

        // Discarded at 105, but it last held an entry at 10, a full window before 110.
        backlog.admit(1, vec![in_lane(2, 2, 1)], 105);
        let low: Vec<_> = (3..=10).map(|id| in_lane(id, id, 1)).collect();

        assert_eq!(
            results(backlog.admit(1, low, 110)),
            vec![Admission::Accepted; 8]
        );
    }

    #[test]
    fn a_short_lived_item_waits_only_for_what_is_due_before_it() {
        let mut backlog = TestQueue::new(roomy_config(), 0);
        let lasting: Vec<_> = (1..=5).map(|id| item(id, id)).collect();
        backlog.admit(1, lasting, 1);

        // Nine drain per window of 100: one by 21, three by 41.
        let admissions = results(backlog.admit(
            1,
            vec![expiring(6, 6, 21), expiring(7, 7, 21), expiring(8, 8, 41)],
            1,
        ));

        assert_eq!(
            admissions,
            vec![
                Admission::Accepted,
                Admission::Full {
                    retry_after_ns: 1_000
                },
                Admission::Accepted,
            ]
        );
        assert_consistent(&backlog);
    }

    #[test]
    fn a_lane_cap_never_falls_below_one() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_entries_per_sender: 2,
                ..config()
            },
            0,
        );

        assert_eq!(
            results(backlog.admit(1, vec![item(1, 1), in_lane(2, 2, 1)], 1)),
            vec![Admission::Accepted; 2]
        );
    }

    #[test]
    fn a_full_lane_is_sent_away_for_longer_while_other_lanes_keep_admitting() {
        let mut backlog = TestQueue::new(roomy_config(), 0);
        let low: Vec<_> = (1..=3).map(|id| in_lane(id, id, 1)).collect();
        backlog.admit(1, [vec![item(9, 9)], low].concat(), 1);

        let hint = |backlog: &mut TestQueue, now_ns| {
            results(backlog.admit(1, vec![in_lane(4, 4, 1)], now_ns))
        };
        assert_eq!(
            hint(&mut backlog, 5),
            vec![Admission::Full {
                retry_after_ns: 1_000
            }]
        );

        backlog.admit(1, vec![item(8, 8)], 15);
        assert_eq!(
            hint(&mut backlog, 15),
            vec![Admission::Full {
                retry_after_ns: 2_000
            }]
        );
    }

    #[test]
    fn an_out_of_range_priority_is_clamped_rather_than_panicking() {
        let mut backlog = TestQueue::new(config(), 0);

        backlog.admit(1, vec![in_lane(1, 1, 99)], 1);

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
                    retry_after_ns: 1_000
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
                    retry_after_ns: 1_000
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
                retry_after_ns: 1_000
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
                retry_after_ns: 1_000
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
                retry_after_ns: 60_000
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
                retry_after_ns: 1_000
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
    fn a_batch_the_caller_cannot_store_keeps_its_lanes_backoff_running() {
        let mut backlog = TestQueue::new(roomy_config(), 0);
        let low: Vec<_> = (1..=3).map(|id| in_lane(id, id, 1)).collect();
        backlog.admit(1, [vec![item(9, 9)], low].concat(), 1);
        let refused = |backlog: &mut TestQueue, now_ns| {
            results(backlog.admit(1, vec![in_lane(4, 4, 1)], now_ns))
        };
        assert_eq!(
            refused(&mut backlog, 5),
            vec![Admission::Full {
                retry_after_ns: 1_000
            }]
        );

        assert!(backlog.take_batch(2, 15, |_| Err::<(), _>(())).is_err());

        assert_eq!(
            refused(&mut backlog, 15),
            vec![Admission::Full {
                retry_after_ns: 2_000
            }]
        );
    }

    #[test]
    fn a_failed_store_after_an_expiry_sweep_leaves_the_clocks_matching_what_is_left() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![expiring(1, 1, 10), expiring(2, 2, 10)], 1);

        assert!(backlog.take_batch(10, 20, |_| Err::<(), _>(())).is_err());

        assert_eq!(backlog.stored_total, 0);
        assert_consistent(&backlog);
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
                        retry_after_ns: 1_000
                    }
                },
                Admitted {
                    key: (8, 8),
                    admission: Admission::Full {
                        retry_after_ns: 2_000
                    }
                },
                Admitted {
                    key: (9, 9),
                    admission: Admission::Full {
                        retry_after_ns: 2_000
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
    fn an_item_without_lanes_still_works() {
        #[derive(Clone)]
        struct NoLanes(u8);

        impl QueueItem for NoLanes {
            type Key = u8;
            type Group = u8;
            const TURNS: &'static [usize] = &[];

            fn key(&self) -> u8 {
                self.0
            }
            fn group(&self) -> u8 {
                self.0
            }
            fn lane(&self) -> usize {
                7
            }
        }

        let mut queue: AdmissionQueue<u8, NoLanes> = AdmissionQueue::new(config(), 0);

        assert_eq!(
            results(queue.admit(1, vec![NoLanes(1)], 1)),
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

    fn sender_hint_at(backlog: &mut TestQueue, now_ns: Timestamp) -> u64 {
        match backlog
            .admit(1, vec![item(8, 8)], now_ns)
            .remove(0)
            .admission
        {
            Admission::Full { retry_after_ns } => retry_after_ns,
            other => panic!("expected a full sender, got {other:?}"),
        }
    }

    fn group_hint_at(backlog: &mut TestQueue, now_ns: Timestamp) -> u64 {
        match backlog
            .admit(1, vec![item(7, 9)], now_ns)
            .remove(0)
            .admission
        {
            Admission::Full { retry_after_ns } => retry_after_ns,
            other => panic!("expected a full group, got {other:?}"),
        }
    }

    fn hint_at(backlog: &mut TestQueue, now_ns: Timestamp) -> u64 {
        match backlog
            .admit(99, vec![item(99, 99)], now_ns)
            .remove(0)
            .admission
        {
            Admission::Full { retry_after_ns } => retry_after_ns,
            other => panic!("expected a full queue, got {other:?}"),
        }
    }
}
