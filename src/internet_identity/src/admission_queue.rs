//! A bounded queue shared by several tenants.
//!
//! [`AdmissionQueue::admit`] removes the submitting tenant's expired entries,
//! checks for duplicates and the group limit, then checks available capacity.
//! Each item is stored, folded into existing work, or rejected with a retry delay.
//!
//! [`AdmissionQueue::take_batch`] takes one item per tenant in turn. Within each
//! tenant, lower-numbered lanes come first, then older entries. The item key breaks
//! ties when arrival times match, including items submitted in the same call.
//!
//! Callers must supply trusted canister time as `now_ns`. The queue assigns that
//! time to new entries and discards entries once they reach the configured age.
//! It forgets entries after taking them, so duplicate detection only covers work
//! still in this queue. A retry delay describes queue pressure, not delivery status.
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
    /// Identifies an item within one tenant. A duplicate key is not stored again.
    type Key: Clone + Eq + Hash + Ord;

    /// Identifies items that share a pending-item limit within one tenant.
    type Group: Copy + Eq + Hash;

    /// Number of priority lanes. Must be positive; lane 0 is taken first.
    const LANES: usize;

    fn key(&self) -> Self::Key;
    fn group(&self) -> Self::Group;

    /// The item's lane. Out-of-range values use the last lane.
    fn lane(&self) -> usize;
}

/// Capacity limits, expiry time, and retry delays.
#[derive(Clone, Debug)]
pub(crate) struct QueueConfig {
    /// Maximum number of entries admitted across all tenants. Callers must also
    /// bound item size if they need a memory limit.
    pub(crate) max_entries: usize,
    /// Maximum entries for one tenant. Its limit may be lower when the queue is
    /// shared by several tenants.
    pub(crate) max_entries_per_tenant: usize,
    /// Maximum pending entries per group within one tenant. Further items fold.
    pub(crate) max_pending_per_group: usize,
    /// Clear the pressure timer when occupancy falls below this value. A value
    /// below `max_entries` avoids resetting the timer whenever one slot opens.
    pub(crate) pressure_cleared_below: usize,
    /// Discard entries when they reach this age, measured from admission.
    pub(crate) discard_after_ns: u64,
    pub(crate) retry: RetryPolicy,
}

impl QueueConfig {
    /// Checks the limits and time intervals. Also usable in a compile-time assertion.
    pub(crate) const fn is_coherent(&self) -> bool {
        self.max_entries > 0
            && self.max_entries_per_tenant > 0
            && self.max_pending_per_group > 0
            && self.max_entries_per_tenant <= self.max_entries
            && self.pressure_cleared_below > 0
            && self.pressure_cleared_below <= self.max_entries
            && self.discard_after_ns > 0
            && self.retry.is_coherent()
    }
}

/// Retry delays returned when the queue or a tenant reaches its limit.
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
    }
}

/// The result of submitting one item.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Admission {
    Stored,
    /// Not stored because its key is already queued or its group is at its limit.
    /// The caller must ensure existing work also covers a folded item.
    Folded,
    /// Not stored because the queue or tenant is full. Retry after this delay.
    Full {
        retry_after_ms: u32,
    },
}

/// An item and the time it was admitted.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Entry<Item> {
    pub(crate) received_at_ns: Timestamp,
    pub(crate) item: Item,
}

/// An entry removed from the queue, with its tenant.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct Taken<Tenant, Item> {
    pub(crate) tenant: Tenant,
    pub(crate) entry: Entry<Item>,
}

/// One tenant's saved entries.
#[derive(Clone, Debug, Eq, PartialEq)]
struct TenantSnapshot<Tenant, Item> {
    tenant: Tenant,
    entries: Vec<Entry<Item>>,
}

/// Saved entries and the next tenant to serve. Private fields keep callers from
/// constructing snapshots with duplicate keys or inconsistent entries.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct QueueSnapshot<Tenant, Item> {
    tenants: Vec<TenantSnapshot<Tenant, Item>>,
    next_tenant: Option<Tenant>,
}

/// Queue counts and timings for metrics. Expired entries count until removed.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct QueueStats {
    pub(crate) stored_total: usize,
    pub(crate) active_tenants: usize,
    /// Age of the oldest stored entry.
    pub(crate) oldest_age_ns: Option<u64>,
    /// Time since the queue filled, until occupancy falls below the reset threshold.
    pub(crate) at_capacity_for_ns: Option<u64>,
    /// Time since the last successful take, or since construction or restore.
    pub(crate) silence_ns: u64,
    pub(crate) discarded_expired: u64,
    pub(crate) folded_duplicates: u64,
    pub(crate) folded_group_capped: u64,
}

/// One tenant's entries, indexed for ordering, duplicate checks, and group limits.
struct TenantQueue<Item: QueueItem> {
    /// Entries ordered by arrival time, then key, in each priority lane.
    lanes: Vec<BTreeMap<(Timestamp, Item::Key), Item>>,
    /// Keys currently queued for this tenant.
    present: HashSet<Item::Key>,
    pending_per_group: HashMap<Item::Group, usize>,
}

impl<Item: QueueItem> TenantQueue<Item> {
    fn new() -> Self {
        Self {
            lanes: (0..Item::LANES).map(|_| BTreeMap::new()).collect(),
            present: HashSet::new(),
            pending_per_group: HashMap::new(),
        }
    }

    fn len(&self) -> usize {
        self.lanes.iter().map(BTreeMap::len).sum()
    }

    fn is_empty(&self) -> bool {
        self.lanes.iter().all(BTreeMap::is_empty)
    }

    fn pending(&self, group: &Item::Group) -> usize {
        self.pending_per_group.get(group).copied().unwrap_or(0)
    }

    fn insert(&mut self, received_at_ns: Timestamp, item: Item) {
        let lane = item.lane().min(Item::LANES - 1);
        let key = item.key();
        *self.pending_per_group.entry(item.group()).or_insert(0) += 1;
        self.present.insert(key.clone());
        self.lanes[lane].insert((received_at_ns, key), item);
    }

    fn remove(&mut self, lane: usize, received_at_ns: Timestamp, key: &Item::Key) -> Option<Item> {
        let item = self.lanes[lane].remove(&(received_at_ns, key.clone()))?;
        self.present.remove(key);
        let group = item.group();
        if let Some(pending) = self.pending_per_group.get_mut(&group) {
            *pending -= 1;
            if *pending == 0 {
                self.pending_per_group.remove(&group);
            }
        }
        Some(item)
    }

    /// The next entry to take: the oldest in the first nonempty lane.
    fn front(&self) -> Option<(usize, Timestamp, Item::Key)> {
        self.lanes.iter().enumerate().find_map(|(lane, entries)| {
            entries
                .first_key_value()
                .map(|((received_at_ns, key), _)| (lane, *received_at_ns, key.clone()))
        })
    }

    fn oldest_in_lane(&self, lane: usize) -> Option<(Timestamp, Item::Key)> {
        self.lanes[lane]
            .first_key_value()
            .map(|((received_at_ns, key), _)| (*received_at_ns, key.clone()))
    }

    fn oldest_arrival_ns(&self) -> Option<Timestamp> {
        self.lanes
            .iter()
            .filter_map(|entries| entries.first_key_value().map(|((ts, _), _)| *ts))
            .min()
    }
}

pub(crate) struct AdmissionQueue<Tenant: Clone + Ord, Item: QueueItem> {
    config: QueueConfig,
    tenants: BTreeMap<Tenant, TenantQueue<Item>>,
    /// Where the next batch resumes taking turns between tenants.
    next_tenant: Option<Tenant>,
    stored_total: usize,

    /// When the queue last changed from empty to nonempty.
    nonempty_since_ns: Option<Timestamp>,
    /// When the queue filled. Reset below `pressure_cleared_below`.
    at_capacity_since_ns: Option<Timestamp>,
    /// Last successful take, or construction or restore time.
    last_taken_ns: Timestamp,

    discarded_expired: u64,
    folded_duplicates: u64,
    folded_group_capped: u64,
}

impl<Tenant: Clone + Ord, Item: QueueItem> AdmissionQueue<Tenant, Item> {
    pub(crate) fn new(config: QueueConfig, now_ns: Timestamp) -> Self {
        assert!(config.is_coherent(), "invalid queue configuration");
        assert!(Item::LANES > 0, "queue items need at least one lane");
        Self {
            config,
            tenants: BTreeMap::new(),
            next_tenant: None,
            stored_total: 0,
            nonempty_since_ns: None,
            at_capacity_since_ns: None,
            last_taken_ns: now_ns,
            discarded_expired: 0,
            folded_duplicates: 0,
            folded_group_capped: 0,
        }
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.stored_total
    }

    #[cfg(test)]
    fn is_empty(&self) -> bool {
        self.stored_total == 0
    }

    /// Submits items for a tenant and returns one result per item, in input order.
    /// Uses trusted `now_ns` for all new entries. Folding leaves existing entries,
    /// including their age and priority, unchanged.
    pub(crate) fn admit(
        &mut self,
        tenant: Tenant,
        items: Vec<Item>,
        now_ns: Timestamp,
    ) -> Vec<Admission> {
        let mut admissions = Vec::with_capacity(items.len());
        let mut swept = false;

        if !items.is_empty() {
            self.discard_expired_for(&tenant, now_ns);
        }

        for item in items {
            if self.holds(&tenant, &item.key()) {
                self.folded_duplicates += 1;
                admissions.push(Admission::Folded);
                continue;
            }

            if self.pending_for(&tenant, &item.group()) >= self.config.max_pending_per_group {
                self.folded_group_capped += 1;
                admissions.push(Admission::Folded);
                continue;
            }

            // Before rejecting for capacity, free expired entries across all tenants.
            // Once per call is enough because now_ns does not change.
            if !swept && (self.is_full() || self.tenant_at_cap(&tenant)) {
                self.discard_expired(now_ns);
                swept = true;
            }

            if self.is_full() || self.tenant_at_cap(&tenant) {
                admissions.push(Admission::Full {
                    retry_after_ms: self.retry_after_ms(now_ns),
                });
                continue;
            }

            self.insert(&tenant, now_ns, item);
            admissions.push(Admission::Stored);
        }

        admissions
    }

    /// Removes up to `limit` live entries, taking one per tenant in turn.
    /// The caller must have room to keep all returned entries. Zero does nothing.
    /// Expired entries are discarded and do not count towards the limit.
    pub(crate) fn take_batch(
        &mut self,
        limit: usize,
        now_ns: Timestamp,
    ) -> Vec<Taken<Tenant, Item>> {
        let mut taken = Vec::new();

        while taken.len() < limit {
            let Some(tenant) = self.advance_cursor() else {
                break;
            };
            // If nothing is live, pop_live removes the tenant. Each iteration
            // therefore returns an entry or reduces the number of tenants.
            if let Some(entry) = self.pop_live(&tenant, now_ns) {
                taken.push(Taken { tenant, entry });
            }
        }

        if !taken.is_empty() {
            self.last_taken_ns = now_ns;
        }
        taken
    }

    /// Copies stored entries and the round-robin cursor. Restore rebuilds the
    /// indexes. Persistence and serialization are the caller's responsibility.
    pub(crate) fn snapshot(&self) -> QueueSnapshot<Tenant, Item> {
        let tenants = self
            .tenants
            .iter()
            .map(|(tenant, queue)| TenantSnapshot {
                tenant: tenant.clone(),
                entries: queue
                    .lanes
                    .iter()
                    .flat_map(|entries| {
                        entries.iter().map(|((received_at_ns, _), item)| Entry {
                            received_at_ns: *received_at_ns,
                            item: item.clone(),
                        })
                    })
                    .collect(),
            })
            .collect();
        QueueSnapshot {
            tenants,
            next_tenant: self.next_tenant.clone(),
        }
    }

    /// Restores saved entries with their original arrival times. Retry timers and
    /// metrics restart at `now_ns`. Existing entries are kept even if the new
    /// configuration lowers capacity; admission waits for space to become available.
    pub(crate) fn restore(
        config: QueueConfig,
        snapshot: QueueSnapshot<Tenant, Item>,
        now_ns: Timestamp,
    ) -> Self {
        let mut backlog = Self::new(config, now_ns);
        for tenant in snapshot.tenants {
            for entry in tenant.entries {
                backlog.insert(&tenant.tenant, entry.received_at_ns, entry.item);
            }
        }
        backlog.next_tenant = snapshot.next_tenant;
        backlog.at_capacity_since_ns = backlog.is_full().then_some(now_ns);
        backlog
    }

    pub(crate) fn stats(&self, now_ns: Timestamp) -> QueueStats {
        QueueStats {
            stored_total: self.stored_total,
            active_tenants: self.tenants.len(),
            oldest_age_ns: self
                .tenants
                .values()
                .filter_map(TenantQueue::oldest_arrival_ns)
                .min()
                .map(|oldest| now_ns.saturating_sub(oldest)),
            at_capacity_for_ns: self
                .at_capacity_since_ns
                .map(|since| now_ns.saturating_sub(since)),
            silence_ns: now_ns.saturating_sub(self.last_taken_ns),
            discarded_expired: self.discarded_expired,
            folded_duplicates: self.folded_duplicates,
            folded_group_capped: self.folded_group_capped,
        }
    }

    fn holds(&self, tenant: &Tenant, key: &Item::Key) -> bool {
        self.tenants
            .get(tenant)
            .is_some_and(|queue| queue.present.contains(key))
    }

    fn pending_for(&self, tenant: &Tenant, group: &Item::Group) -> usize {
        self.tenants
            .get(tenant)
            .map_or(0, |queue| queue.pending(group))
    }

    fn is_full(&self) -> bool {
        self.stored_total >= self.config.max_entries
    }

    fn tenant_at_cap(&self, tenant: &Tenant) -> bool {
        let held = self.tenants.get(tenant).map_or(0, TenantQueue::len);
        held >= self.tenant_cap()
    }

    /// Divides capacity equally among active tenants, up to the per-tenant limit.
    /// Existing entries are not evicted when a new tenant reduces this share.
    fn tenant_cap(&self) -> usize {
        self.config
            .max_entries_per_tenant
            .min(self.config.max_entries / self.tenants.len().max(1))
    }

    fn insert(&mut self, tenant: &Tenant, received_at_ns: Timestamp, item: Item) {
        if self.stored_total == 0 {
            self.nonempty_since_ns = Some(received_at_ns);
        }
        self.tenants
            .entry(tenant.clone())
            .or_insert_with(TenantQueue::new)
            .insert(received_at_ns, item);
        self.stored_total += 1;
        if self.is_full() {
            self.at_capacity_since_ns.get_or_insert(received_at_ns);
        }
    }

    /// Removes the next live entry, discarding expired entries before it.
    fn pop_live(&mut self, tenant: &Tenant, now_ns: Timestamp) -> Option<Entry<Item>> {
        loop {
            let entry = self.pop_front(tenant)?;
            if entry
                .received_at_ns
                .saturating_add(self.config.discard_after_ns)
                <= now_ns
            {
                self.discarded_expired += 1;
                continue;
            }
            return Some(entry);
        }
    }

    fn pop_front(&mut self, tenant: &Tenant) -> Option<Entry<Item>> {
        let queue = self.tenants.get_mut(tenant)?;
        let (lane, received_at_ns, key) = queue.front()?;
        let item = queue.remove(lane, received_at_ns, &key)?;
        let emptied = queue.is_empty();
        self.after_removal(tenant, emptied);
        Some(Entry {
            received_at_ns,
            item,
        })
    }

    /// Removes expired entries from every tenant. Each lane is ordered by age,
    /// so scanning it stops at the first live entry.
    fn discard_expired(&mut self, now_ns: Timestamp) {
        for tenant in self.tenants.keys().cloned().collect::<Vec<_>>() {
            self.discard_expired_for(&tenant, now_ns);
        }
    }

    fn discard_expired_for(&mut self, tenant: &Tenant, now_ns: Timestamp) {
        for lane in 0..Item::LANES {
            loop {
                let Some(queue) = self.tenants.get_mut(tenant) else {
                    return;
                };
                let Some((received_at_ns, key)) = queue.oldest_in_lane(lane) else {
                    break;
                };
                if received_at_ns.saturating_add(self.config.discard_after_ns) > now_ns {
                    break;
                }
                queue.remove(lane, received_at_ns, &key);
                let emptied = queue.is_empty();
                self.after_removal(tenant, emptied);
                self.discarded_expired += 1;
            }
        }
    }

    /// Updates counts and timers after removal, and removes empty tenants.
    fn after_removal(&mut self, tenant: &Tenant, emptied: bool) {
        if emptied {
            self.tenants.remove(tenant);
        }
        self.stored_total -= 1;
        if self.stored_total == 0 {
            self.nonempty_since_ns = None;
        }
        if self.stored_total < self.config.pressure_cleared_below {
            self.at_capacity_since_ns = None;
        }
    }

    /// Selects the next tenant and advances the cursor. If that tenant was removed,
    /// uses the next remaining tenant, wrapping around at the end.
    fn advance_cursor(&mut self) -> Option<Tenant> {
        if self.tenants.is_empty() {
            self.next_tenant = None;
            return None;
        }

        let picked = self
            .next_tenant
            .as_ref()
            .and_then(|from| {
                self.tenants
                    .range(from.clone()..)
                    .next()
                    .map(|(tenant, _)| tenant.clone())
            })
            .or_else(|| self.tenants.keys().next().cloned())?;

        self.next_tenant = self
            .tenants
            .range((Excluded(picked.clone()), Unbounded))
            .next()
            .map(|(tenant, _)| tenant.clone())
            .or_else(|| self.tenants.keys().next().cloned());

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
        let doublings = (at_capacity_for_ns / retry.doubles_every_ns)
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

    /// A small test item with two priority lanes.
    #[derive(Clone, Debug, Eq, PartialEq)]
    struct TestItem {
        group: u8,
        id: u8,
        lane: usize,
    }

    impl QueueItem for TestItem {
        type Key = (u8, u8);
        type Group = u8;
        const LANES: usize = 2;

        fn key(&self) -> Self::Key {
            (self.group, self.id)
        }

        fn group(&self) -> Self::Group {
            self.group
        }

        fn lane(&self) -> usize {
            self.lane
        }
    }

    type TestQueue = AdmissionQueue<u8, TestItem>;

    fn item(group: u8, id: u8) -> TestItem {
        TestItem { group, id, lane: 0 }
    }

    fn in_lane(group: u8, id: u8, lane: usize) -> TestItem {
        TestItem { group, id, lane }
    }

    fn config() -> QueueConfig {
        QueueConfig {
            max_entries: 8,
            max_entries_per_tenant: 8,
            max_pending_per_group: 8,
            pressure_cleared_below: 6,
            discard_after_ns: 100,
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
    fn admit_each(backlog: &mut TestQueue, tenant: u8, items: Vec<TestItem>, from_ns: Timestamp) {
        for (offset, item) in items.into_iter().enumerate() {
            backlog.admit(tenant, vec![item], from_ns + offset as u64);
        }
    }

    fn keys_taken(taken: &[Taken<u8, TestItem>]) -> Vec<(u8, u8)> {
        taken.iter().map(|t| t.entry.item.key()).collect()
    }

    fn tenants_taken(taken: &[Taken<u8, TestItem>]) -> Vec<u8> {
        taken.iter().map(|t| t.tenant).collect()
    }

    /// Checks that counts, indexes, and timers agree with the stored entries.
    fn assert_consistent(backlog: &TestQueue) {
        let counted: usize = backlog.tenants.values().map(TenantQueue::len).sum();
        assert_eq!(backlog.stored_total, counted, "stored_total drifted");

        for (tenant, queue) in &backlog.tenants {
            assert!(!queue.is_empty(), "tenant {tenant} kept after emptying");

            let keys: HashSet<(u8, u8)> = queue
                .lanes
                .iter()
                .flat_map(|lane| lane.keys().map(|(_, key)| *key))
                .collect();
            assert_eq!(queue.present, keys, "present drifted for tenant {tenant}");

            let mut groups: HashMap<u8, usize> = HashMap::new();
            for (group, _) in &keys {
                *groups.entry(*group).or_insert(0) += 1;
            }
            assert_eq!(
                queue.pending_per_group, groups,
                "pending_per_group drifted for tenant {tenant}"
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

    // Ordering.

    #[test]
    fn takes_in_arrival_order_within_a_lane() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(2, 2), item(3, 3)], 1);

        let taken = backlog.take_batch(10, 5);

        assert_eq!(keys_taken(&taken), vec![(1, 1), (2, 2), (3, 3)]);
        assert_consistent(&backlog);
    }

    #[test]
    fn takes_lower_lanes_first() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(
            &mut backlog,
            1,
            vec![in_lane(1, 1, 1), in_lane(2, 2, 1), in_lane(3, 3, 0)],
            1,
        );

        let taken = backlog.take_batch(10, 5);

        // The lane-0 item arrived last and still leaves first.
        assert_eq!(keys_taken(&taken), vec![(3, 3), (1, 1), (2, 2)]);
    }

    #[test]
    fn a_lane_above_the_last_is_clamped_rather_than_panicking() {
        let mut backlog = TestQueue::new(config(), 0);

        backlog.admit(1, vec![in_lane(1, 1, 99)], 1);

        assert_eq!(keys_taken(&backlog.take_batch(10, 2)), vec![(1, 1)]);
    }

    // Folding.

    #[test]
    fn folds_a_key_it_already_holds() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        let admissions = backlog.admit(1, vec![item(1, 1)], 2);

        assert_eq!(admissions, vec![Admission::Folded]);
        assert_eq!(backlog.len(), 1);
        assert_eq!(backlog.stats(2).folded_duplicates, 1);
    }

    #[test]
    fn the_same_key_under_another_tenant_is_a_different_item() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        let admissions = backlog.admit(2, vec![item(1, 1)], 1);

        assert_eq!(admissions, vec![Admission::Stored]);
        assert_eq!(backlog.len(), 2);
    }

    #[test]
    fn a_fold_leaves_the_original_where_it_was() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(2, 2)], 1);

        // Resubmitting must not change the original entry's arrival time.
        assert_eq!(
            backlog.admit(1, vec![item(1, 1)], 9),
            vec![Admission::Folded]
        );

        assert_eq!(
            keys_taken(&backlog.take_batch(10, 10)),
            vec![(1, 1), (2, 2)]
        );
    }

    #[test]
    fn folds_once_a_group_is_at_its_cap() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_pending_per_group: 2,
                ..config()
            },
            0,
        );

        let admissions = backlog.admit(1, vec![item(7, 1), item(7, 2), item(7, 3)], 1);

        assert_eq!(
            admissions,
            vec![Admission::Stored, Admission::Stored, Admission::Folded]
        );
        assert_eq!(backlog.stats(1).folded_group_capped, 1);
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
            backlog.admit(1, vec![item(7, 3)], 4),
            vec![Admission::Stored]
        );
        assert_consistent(&backlog);
    }

    #[test]
    fn an_expired_duplicate_does_not_cover_a_new_submission() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);

        assert_eq!(
            backlog.admit(1, vec![item(1, 1)], 101),
            vec![Admission::Stored]
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
            backlog.admit(1, vec![item(1, 2)], 101),
            vec![Admission::Stored]
        );
        assert_eq!(keys_taken(&backlog.take_batch(1, 101)), vec![(1, 2)]);
        assert_consistent(&backlog);
    }

    // Fairness.

    #[test]
    fn takes_one_tenant_at_a_time_in_turn() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(1, 2), item(1, 3)], 1);
        backlog.admit(2, vec![item(2, 1)], 1);
        admit_each(&mut backlog, 3, vec![item(3, 1), item(3, 2)], 1);

        let taken = backlog.take_batch(5, 5);

        assert_eq!(tenants_taken(&taken), vec![1, 2, 3, 1, 3]);
        assert_eq!(backlog.len(), 1);
    }

    #[test]
    fn the_cursor_resumes_where_the_last_batch_stopped() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![item(1, 1), item(1, 2)], 1);
        admit_each(&mut backlog, 2, vec![item(2, 1), item(2, 2)], 1);

        let first = backlog.take_batch(1, 5);
        let second = backlog.take_batch(1, 6);
        let third = backlog.take_batch(1, 7);

        assert_eq!(tenants_taken(&first), vec![1]);
        assert_eq!(tenants_taken(&second), vec![2]);
        assert_eq!(tenants_taken(&third), vec![1]);
    }

    #[test]
    fn one_tenant_alone_stops_at_its_own_ceiling() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_entries_per_tenant: 3,
                ..config()
            },
            0,
        );

        let admissions = backlog.admit(1, vec![item(1, 1), item(2, 2), item(3, 3), item(4, 4)], 1);

        assert_eq!(
            admissions,
            vec![
                Admission::Stored,
                Admission::Stored,
                Admission::Stored,
                Admission::Full {
                    retry_after_ms: 1_000
                },
            ]
        );
        // Other tenants can use the five remaining slots.
        assert_eq!(backlog.len(), 3);
    }

    #[test]
    fn an_incumbent_over_its_share_is_refused_while_a_newcomer_is_admitted() {
        let mut backlog = TestQueue::new(config(), 0);
        // With only one tenant, all five items fit.
        admit_each(
            &mut backlog,
            1,
            vec![item(1, 1), item(2, 2), item(3, 3), item(4, 4), item(5, 5)],
            1,
        );
        backlog.admit(2, vec![item(1, 1)], 6);

        // Each tenant's share is now four. Tenant 1 already holds five.
        let incumbent = backlog.admit(1, vec![item(6, 6)], 7);
        let newcomer = backlog.admit(2, vec![item(2, 2)], 7);

        assert_eq!(
            incumbent,
            vec![Admission::Full {
                retry_after_ms: 1_000
            }]
        );
        assert_eq!(newcomer, vec![Admission::Stored]);
    }

    #[test]
    fn a_tenant_is_dropped_once_it_empties() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.admit(2, vec![item(2, 1)], 1);

        backlog.take_batch(1, 2);

        assert_eq!(backlog.stats(2).active_tenants, 1);
        assert_consistent(&backlog);
    }

    // Retry delays.

    #[test]
    fn a_full_buffer_answers_with_the_base_hint() {
        let mut backlog = full_queue(1);

        let admissions = backlog.admit(1, vec![item(9, 9)], 1);

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
    fn a_tenant_at_its_cap_gets_the_stalled_hint() {
        let mut backlog = TestQueue::new(
            QueueConfig {
                max_entries_per_tenant: 1,
                ..config()
            },
            0,
        );
        backlog.admit(1, vec![item(1, 1)], 1);

        assert_eq!(
            backlog.admit(1, vec![item(2, 2)], 60),
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

        let admissions = backlog.admit(1, items, 10_000);

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
        assert_eq!(backlog.len(), 1);
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
                max_entries_per_tenant: 2,
                pressure_cleared_below: 2,
                ..config()
            },
            0,
        );
        admit_each(&mut backlog, 1, vec![item(1, 1), item(2, 2)], 1);

        // Both stored entries have expired, so the new item should fit.
        let admissions = backlog.admit(1, vec![item(3, 3)], 200);

        assert_eq!(admissions, vec![Admission::Stored]);
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
    fn expired_tenants_do_not_stop_a_batch_before_live_tenants() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1)], 1);
        backlog.admit(2, vec![item(2, 2)], 1);
        backlog.admit(3, vec![item(3, 3)], 50);

        let taken = backlog.take_batch(1, 101);

        assert_eq!(tenants_taken(&taken), vec![3]);
        assert_eq!(backlog.stats(101).discarded_expired, 2);
        assert!(backlog.is_empty());
        assert_consistent(&backlog);
    }

    // Snapshot and restore.

    #[test]
    fn snapshot_and_restore_keep_the_entries_and_their_arrival_times() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![in_lane(1, 1, 1), item(2, 2)], 1);
        backlog.admit(2, vec![item(3, 3)], 3);

        let mut restored = TestQueue::restore(config(), backlog.snapshot(), 4);

        assert_eq!(restored.len(), backlog.len());
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
            restored.len(),
            8,
            "the entries aged out, so this proves nothing"
        );

        assert_eq!(
            restored.admit(1, vec![item(9, 9)], 60),
            vec![Admission::Full {
                retry_after_ms: 1_000
            }]
        );
    }

    #[test]
    fn restoring_nothing_gives_an_empty_buffer() {
        let empty = TestQueue::new(config(), 0);
        let restored = TestQueue::restore(config(), empty.snapshot(), 10);

        assert!(restored.is_empty());
        assert_eq!(restored.stats(10).active_tenants, 0);
        assert_consistent(&restored);
    }

    #[test]
    fn restore_preserves_the_next_tenant_to_serve() {
        let mut backlog = TestQueue::new(config(), 0);
        backlog.admit(1, vec![item(1, 1), item(1, 2)], 1);
        backlog.admit(2, vec![item(2, 1)], 1);
        assert_eq!(tenants_taken(&backlog.take_batch(1, 2)), vec![1]);

        let mut restored = TestQueue::restore(config(), backlog.snapshot(), 3);

        assert_eq!(tenants_taken(&restored.take_batch(1, 4)), vec![2]);
        assert_consistent(&restored);
    }

    #[test]
    fn restore_restarts_the_pressure_timer() {
        let backlog = full_queue(1);
        let mut restored = TestQueue::restore(config(), backlog.snapshot(), 60);

        assert_eq!(restored.stats(60).at_capacity_for_ns, Some(0));
        assert_eq!(hint_at(&mut restored, 70), 2_000);
    }

    #[test]
    #[should_panic(expected = "invalid queue configuration")]
    fn rejects_a_configuration_whose_pressure_cannot_clear() {
        TestQueue::new(
            QueueConfig {
                pressure_cleared_below: 0,
                ..config()
            },
            0,
        );
    }

    #[test]
    #[should_panic(expected = "queue items need at least one lane")]
    fn rejects_items_without_any_lanes() {
        #[derive(Clone)]
        struct NoLanes;

        impl QueueItem for NoLanes {
            type Key = ();
            type Group = ();
            const LANES: usize = 0;

            fn key(&self) {}
            fn group(&self) {}
            fn lane(&self) -> usize {
                0
            }
        }

        AdmissionQueue::<u8, NoLanes>::new(config(), 0);
    }

    // Test helpers.

    /// Fills the queue at nanosecond 1.
    fn full_queue(tenant: u8) -> TestQueue {
        let mut backlog = TestQueue::new(config(), 0);
        let items = (1..=8).map(|id| item(id, id)).collect();
        assert_eq!(
            backlog.admit(tenant, items, 1),
            vec![Admission::Stored; 8],
            "test setup failed to fill the queue"
        );
        backlog
    }

    /// Reads the retry delay by submitting to a full queue.
    fn hint_at(backlog: &mut TestQueue, now_ns: Timestamp) -> u32 {
        match backlog.admit(99, vec![item(99, 99)], now_ns).remove(0) {
            Admission::Full { retry_after_ms } => retry_after_ms,
            other => panic!("expected a full queue, got {other:?}"),
        }
    }
}
