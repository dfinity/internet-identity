//! A bounded queue of opaque items that decides what to let in and says when to
//! come back.
//!
//! Items enter through [`AdmissionQueue::admit`] and leave through
//! [`AdmissionQueue::take_batch`]. Inside a tenant they are ordered by lane and
//! then by arrival. Across tenants they are taken round-robin, so a tenant holding
//! thousands drains no faster than one holding a handful.
//!
//! The queue stamps arrival times itself and never reads one off an item, so a caller
//! cannot age its own items in or out. Everything else is opaque: [`QueueItem`] asks
//! an item only for an identity, a group and a lane.
//!
//! Admission is synchronous and final. An item is stored, folds into something
//! already held, or is refused with a hint of how long to wait. The hint grows with
//! how long the queue has been full and jumps once nothing is leaving at all, which
//! is a failure further down rather than congestion here.
//!
//! Nothing is kept once an item is taken, so there is no state left to query.
// Nothing calls this yet. The entrypoint and the dispatcher that do arrive in
// later PRs of this stack.
#![allow(dead_code)]

use internet_identity_interface::internet_identity::types::Timestamp;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::hash::Hash;
use std::ops::Bound::{Excluded, Unbounded};

/// What the queue needs from the items it holds.
pub trait QueueItem: Clone {
    /// Tells two items apart inside one tenant. An item whose key is already held
    /// folds instead of being stored again.
    type Key: Clone + Eq + Hash + Ord;

    /// Items sharing a group are capped together, so one group cannot fill a
    /// tenant's share on its own.
    type Group: Copy + Eq + Hash;

    /// How many priority lanes items are sorted into. Lane 0 is taken first. At
    /// least one.
    const LANES: usize;

    fn key(&self) -> Self::Key;
    fn group(&self) -> Self::Group;

    /// Which lane this item belongs in. Values at or above [`QueueItem::LANES`]
    /// are clamped to the last lane.
    fn lane(&self) -> usize;
}

/// Sizes and clocks the queue runs by.
#[derive(Clone, Debug)]
pub struct QueueConfig {
    /// Ceiling on the whole queue, bounding both the heap footprint and the
    /// snapshot an upgrade has to carry.
    pub max_entries: usize,
    /// Ceiling on one tenant even when it is the only one. The gap up to
    /// `max_entries` is what a tenant arriving later finds free.
    pub max_entries_per_tenant: usize,
    /// Items one tenant may hold for one group before further ones fold.
    pub max_pending_per_group: usize,
    /// Occupancy the queue must fall below before it stops reporting pressure.
    /// Under `max_entries`, so the retry hint does not flap at the boundary.
    pub pressure_cleared_below: usize,
    /// Age at which an item is discarded unseen, measured from its arrival.
    pub discard_after_ns: u64,
    pub retry: RetryPolicy,
}

impl QueueConfig {
    /// Whether the sizes relate the way the queue assumes. `const`, so a config can
    /// assert on it and fail the build rather than a test.
    pub const fn is_coherent(&self) -> bool {
        self.max_entries > 0
            && self.max_entries_per_tenant > 0
            && self.max_pending_per_group > 0
            && self.max_entries_per_tenant <= self.max_entries
            && self.pressure_cleared_below <= self.max_entries
            && self.discard_after_ns > 0
            && self.retry.is_coherent()
    }
}

/// How long the queue tells a caller to wait once it stops accepting.
#[derive(Clone, Debug)]
pub struct RetryPolicy {
    /// First hint, given the moment the queue fills.
    pub base_ms: u32,
    /// Ceiling on the backed-off hint while items are still leaving.
    pub ceiling_ms: u32,
    /// Hint given once nothing has left for `stalled_after_silence_ns`. Above the
    /// ceiling on purpose: that is a failure downstream rather than congestion here,
    /// and a short hint would invite a caller to hammer a queue that cannot drain.
    pub when_stalled_ms: u32,
    /// How long the queue stays full for the hint to double.
    pub doubles_every_ns: u64,
    pub max_doublings: u32,
    /// Silence since the last item left that counts as nothing draining.
    pub stalled_after_silence_ns: u64,
}

impl RetryPolicy {
    pub const fn is_coherent(&self) -> bool {
        self.base_ms > 0
            && self.base_ms <= self.ceiling_ms
            && self.ceiling_ms <= self.when_stalled_ms
            && self.doubles_every_ns > 0
            && self.stalled_after_silence_ns > 0
    }
}

/// What the queue did with one submitted item.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Admission {
    Stored,
    /// Not stored, because something already held covers it: either the same key, or
    /// a group already at its cap.
    Folded,
    /// Not stored, because there was no room. Offer it again after the hint.
    Full {
        retry_after_ms: u32,
    },
}

/// An item with the arrival time the queue gave it. Identity stays the item's, the
/// clock is the queue's.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Entry<Item> {
    pub received_at_ns: Timestamp,
    pub item: Item,
}

/// An entry on its way out, with the tenant it was held under.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Taken<Tenant, Item> {
    pub tenant: Tenant,
    pub entry: Entry<Item>,
}

/// One tenant's entries, as [`AdmissionQueue::snapshot`] writes them.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TenantSnapshot<Tenant, Item> {
    pub tenant: Tenant,
    pub entries: Vec<Entry<Item>>,
}

/// A reading of the queue, for metrics.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct QueueStats {
    pub stored_total: usize,
    pub active_tenants: usize,
    /// Age of the entry that has waited longest.
    pub oldest_age_ns: Option<u64>,
    /// How long the queue has been full, if it is.
    pub at_capacity_for_ns: Option<u64>,
    /// How long since anything was taken.
    pub silence_ns: u64,
    pub discarded_expired: u64,
    pub folded_duplicates: u64,
    pub folded_group_capped: u64,
}

/// One tenant's entries, split by lane, with the two indexes admission reads.
struct TenantQueue<Item: QueueItem> {
    /// One map per lane, keyed by arrival then identity so iteration is oldest
    /// first and two arrivals in the same nanosecond stay distinct.
    lanes: Vec<BTreeMap<(Timestamp, Item::Key), Item>>,
    /// Membership only. A duplicate is a no-op, so nothing ever looks an entry up in
    /// order to change it.
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

    /// Oldest entry of the first lane holding anything, which is the next one out.
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

pub struct AdmissionQueue<Tenant: Clone + Ord, Item: QueueItem> {
    config: QueueConfig,
    tenants: BTreeMap<Tenant, TenantQueue<Item>>,
    /// Where the round-robin resumes. Without it every batch restarts at the first
    /// tenant and the ones after it are never reached.
    next_tenant: Option<Tenant>,
    stored_total: usize,

    /// When the queue last went from empty to holding something.
    nonempty_since_ns: Option<Timestamp>,
    /// When the queue last reached `max_entries`, while it stays there.
    at_capacity_since_ns: Option<Timestamp>,
    /// When something last left. What took it is not the queue's concern.
    last_taken_ns: Timestamp,

    discarded_expired: u64,
    folded_duplicates: u64,
    folded_group_capped: u64,
}

impl<Tenant: Clone + Ord, Item: QueueItem> AdmissionQueue<Tenant, Item> {
    pub fn new(config: QueueConfig, now_ns: Timestamp) -> Self {
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

    pub fn len(&self) -> usize {
        self.stored_total
    }

    pub fn is_empty(&self) -> bool {
        self.stored_total == 0
    }

    /// Offers `items` under `tenant` and answers one [`Admission`] each, in the order
    /// they were given.
    ///
    /// The queue's own room is the only thing that can produce [`Admission::Full`].
    /// What becomes of an item after it is taken never reaches a caller here.
    pub fn admit(&mut self, tenant: Tenant, items: Vec<Item>, now_ns: Timestamp) -> Vec<Admission> {
        let mut admissions = Vec::with_capacity(items.len());
        let mut swept = false;

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

            // Expired entries hold room nobody can use, so reclaim before refusing.
            // Once per call, since a second sweep at the same instant finds nothing.
            if !swept && (self.is_full() || self.tenant_at_cap(&tenant)) {
                self.discard_expired(now_ns);
                swept = true;
            }

            if self.is_full() {
                self.at_capacity_since_ns.get_or_insert(now_ns);
                admissions.push(Admission::Full {
                    retry_after_ms: self.retry_after_ms(now_ns),
                });
                continue;
            }

            // Over its own share while the queue still has room. This tenant's
            // entries leave at the round-robin rate whatever the rest is doing, so
            // there is no congestion to back off from.
            if self.tenant_at_cap(&tenant) {
                admissions.push(Admission::Full {
                    retry_after_ms: self.config.retry.base_ms,
                });
                continue;
            }

            self.insert(&tenant, now_ns, item);
            admissions.push(Admission::Stored);
        }

        admissions
    }

    /// Takes up to `limit` entries, one tenant at a time in round-robin order,
    /// discarding expired ones on the way past.
    ///
    /// `limit` is what the caller can accept right now. Zero is a normal answer: it
    /// records that the queue was asked and nothing moved, which is what
    /// [`AdmissionQueue::admit`] reads later as a stall.
    pub fn take_batch(&mut self, limit: usize, now_ns: Timestamp) -> Vec<Taken<Tenant, Item>> {
        let mut taken = Vec::new();
        let mut tenants_without_work = 0;

        while taken.len() < limit {
            let Some(tenant) = self.advance_cursor() else {
                break;
            };
            match self.pop_live(&tenant, now_ns) {
                Some(entry) => {
                    taken.push(Taken { tenant, entry });
                    tenants_without_work = 0;
                }
                None => {
                    tenants_without_work += 1;
                    if tenants_without_work >= self.tenants.len().max(1) {
                        break;
                    }
                }
            }
        }

        if !taken.is_empty() {
            self.last_taken_ns = now_ns;
        }
        taken
    }

    /// Everything held, grouped by tenant. The indexes are left out because
    /// [`AdmissionQueue::restore`] rebuilds them from the entries.
    pub fn snapshot(&self) -> Vec<TenantSnapshot<Tenant, Item>> {
        self.tenants
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
            .collect()
    }

    /// Rebuilds from a snapshot, keeping every arrival time. The silence clock starts
    /// at `now_ns`, so a queue that has just come back is not read as one nothing is
    /// draining.
    pub fn restore(
        config: QueueConfig,
        snapshot: Vec<TenantSnapshot<Tenant, Item>>,
        now_ns: Timestamp,
    ) -> Self {
        let mut backlog = Self::new(config, now_ns);
        for tenant in snapshot {
            for entry in tenant.entries {
                backlog.insert(&tenant.tenant, entry.received_at_ns, entry.item);
            }
        }
        backlog
    }

    pub fn stats(&self, now_ns: Timestamp) -> QueueStats {
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

    /// An equal share of the queue, never more than one tenant may hold alone. The
    /// divisor counts tenants currently holding something, so a tenant that is not
    /// using the queue does not reserve space in it.
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
    }

    /// Next live entry of one tenant. Expired ones are discarded rather than handed
    /// on, so the caller never sees an entry it would only have to throw away.
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

    /// Discards everything past `discard_after_ns`. Items age out after the same
    /// interval whatever they are, so the expired ones sit at the front of each lane
    /// and the walk stops at the first one still live.
    fn discard_expired(&mut self, now_ns: Timestamp) {
        let discard_after_ns = self.config.discard_after_ns;
        for tenant in self.tenants.keys().cloned().collect::<Vec<_>>() {
            for lane in 0..Item::LANES {
                loop {
                    let Some(queue) = self.tenants.get_mut(&tenant) else {
                        break;
                    };
                    let Some((received_at_ns, key)) = queue.oldest_in_lane(lane) else {
                        break;
                    };
                    if received_at_ns.saturating_add(discard_after_ns) > now_ns {
                        break;
                    }
                    queue.remove(lane, received_at_ns, &key);
                    let emptied = queue.is_empty();
                    self.after_removal(&tenant, emptied);
                    self.discarded_expired += 1;
                }
            }
        }
    }

    /// Drops an emptied tenant and moves the counters the pressure clocks read. Every
    /// removal goes through here, so a clock cannot drift from the occupancy.
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

    /// Picks the tenant whose turn it is and points the cursor at the next one. The
    /// cursor may name a tenant that has since emptied and been dropped, so it
    /// resolves to the first tenant at or after it.
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

    /// How long to tell a caller to wait. Being full is always the reason; this only
    /// picks the number, from how long the queue has been full and how long since
    /// anything left it.
    fn retry_after_ms(&self, now_ns: Timestamp) -> u32 {
        let retry = &self.config.retry;

        // Silence counts only from the point there was something to take, so a queue
        // left idle for an hour and then filled in one call does not look stalled.
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

    /// Two lanes and small caps, so a test can reach any boundary in a few lines.
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

    /// Admits items one per nanosecond from `from_ns`, so arrival order is the order
    /// they are listed in.
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

    /// The counters and the indexes are all derivable from the lanes, so anything
    /// that touches them has to leave them agreeing.
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

    // ---- ordering ------------------------------------------------------

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

    // ---- folding -------------------------------------------------------

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

        // Resent last, so a design that refreshed the arrival time would put it
        // behind the item that followed it.
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

    // ---- fairness ------------------------------------------------------

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
        // The queue itself is nowhere near full, so the room is there for others.
        assert_eq!(backlog.len(), 3);
    }

    #[test]
    fn an_incumbent_over_its_share_is_refused_while_a_newcomer_is_admitted() {
        let mut backlog = TestQueue::new(config(), 0);
        // Alone, so its share is the whole queue and all five are taken.
        admit_each(
            &mut backlog,
            1,
            vec![item(1, 1), item(2, 2), item(3, 3), item(4, 4), item(5, 5)],
            1,
        );
        backlog.admit(2, vec![item(1, 1)], 6);

        // Two tenants now, so the share is four and the incumbent is over it.
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

    // ---- back pressure -------------------------------------------------

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
    fn the_hint_stops_at_the_ceiling() {
        let mut backlog = full_queue(1);
        backlog.admit(1, vec![item(9, 9)], 1);

        // Three doublings would be 8_000, and more time cannot push it past that.
        assert_eq!(hint_at(&mut backlog, 41), 8_000);
        assert_eq!(hint_at(&mut backlog, 45), 8_000);
    }

    #[test]
    fn the_hint_jumps_once_nothing_is_draining() {
        let mut backlog = full_queue(1);
        backlog.admit(1, vec![item(9, 9)], 1);

        // The dispatcher keeps asking and keeps having no room of its own.
        for tick in 2..60 {
            assert!(backlog.take_batch(0, tick).is_empty());
        }

        assert_eq!(hint_at(&mut backlog, 60), 60_000);
    }

    #[test]
    fn an_idle_buffer_filled_in_one_call_is_not_stalled() {
        // Nothing has been taken since the queue was built, but nothing was waiting
        // to be taken either, so the silence means idle rather than broken.
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

        // Eight down to five, under the low mark of six.
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

    // ---- expiry --------------------------------------------------------

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

        // Full, but everything in it aged out, so the room is reclaimed rather than
        // the newcomer refused.
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

    // ---- upgrade -------------------------------------------------------

    #[test]
    fn snapshot_and_restore_keep_the_entries_and_their_arrival_times() {
        let mut backlog = TestQueue::new(config(), 0);
        admit_each(&mut backlog, 1, vec![in_lane(1, 1, 1), item(2, 2)], 1);
        backlog.admit(2, vec![item(3, 3)], 3);

        let mut restored = TestQueue::restore(config(), backlog.snapshot(), 500);

        assert_eq!(restored.len(), backlog.len());
        assert_eq!(restored.snapshot(), backlog.snapshot());
        // Order survives, including the lane that arrived first losing to lane 0.
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

        // Past the silence threshold of 50ns but inside the 100ns lifetime, so the
        // entries are still there and only the clock is in question.
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
        let restored = TestQueue::restore(config(), vec![], 10);

        assert!(restored.is_empty());
        assert_eq!(restored.stats(10).active_tenants, 0);
        assert_consistent(&restored);
    }

    // ---- helpers that need the types above -----------------------------

    /// A queue holding exactly `max_entries`, all arrived at nanosecond 1.
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

    /// The hint a full queue gives at `now_ns`, read through `admit` so the test
    /// exercises the path a caller actually takes.
    fn hint_at(backlog: &mut TestQueue, now_ns: Timestamp) -> u32 {
        match backlog.admit(99, vec![item(99, 99)], now_ns).remove(0) {
            Admission::Full { retry_after_ms } => retry_after_ms,
            other => panic!("expected a full queue, got {other:?}"),
        }
    }
}
