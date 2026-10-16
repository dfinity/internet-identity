//! Everything the library holds, in stable memory.

use super::ids;
use super::keys::{self, EntryKey};
use crate::types::{
    Bucket, Content, Counts, Funnel, Misconfigured, MisconfiguredSince, Notification,
    NotificationId,
};
use candid::Principal;
use ic_stable_structures::memory_manager::{MemoryId, MemoryManager, VirtualMemory};
use ic_stable_structures::storable::{Bound, Storable};
use ic_stable_structures::{BTreeMap, DefaultMemoryImpl};
use std::borrow::Cow;

pub type Memory = VirtualMemory<DefaultMemoryImpl>;

/// The stable memories the library keeps its state in. Content has its own
/// because a `StableBTreeMap` pages from its value bound, and content is
/// unbounded where a row is 66 bytes.
#[derive(Clone, Copy, Debug)]
pub struct Memories {
    /// Notifications waiting to be sent, and where each one stands.
    pub entries: MemoryId,
    /// What a channel may still pull.
    pub content: MemoryId,
    /// An hour of the delivery pipeline per row, thirty days deep.
    pub metrics: MemoryId,
}

pub const WEIGHTS: [usize; 4] = [8, 4, 2, 1];
const LANES: u8 = 4;

const HOUR: u64 = 3_600_000_000_000;
const KEEP_HOURS: usize = 720;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Entry {
    pub id: NotificationId,
    pub recipient: Principal,
    pub lane: u8,
    pub expires_at: u64,
    pub due_at: u64,
    /// Its one second attempt has been spent.
    pub retried: bool,
    /// A channel has told the app it showed this one, so a second channel
    /// showing it too is not a second delivery to count.
    pub received: bool,
    /// The hour this notification was queued in, which is the hour every step
    /// it takes afterwards is counted in: a bucket is one cohort, not one
    /// hour's worth of unrelated events.
    pub queued_hour: u32,
    /// The funnel its app named, or none.
    pub funnel: u16,
    /// Somebody acted on the notification a channel showed, which is the last
    /// thing that happens to one and the only stage the app can be sure of.
    pub opened: bool,
}

/// When the second attempt a refusal earns is made: late enough to leave the
/// user most of the window to sign in or turn notifications on, early enough
/// that a five-minute notification still gets the attempt.
fn retry_at(now: u64, expires_at: u64) -> u64 {
    now + expires_at.saturating_sub(now) * 2 / 3
}

/// Where a notification stands, as one small row per notification, so a repeat
/// of a key finds it without walking the map.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Slot {
    kind: u8,
    lane: u8,
    expires_at: u64,
    due_at: u64,
}

impl Slot {
    fn key(&self, id: NotificationId) -> EntryKey {
        match self.kind {
            keys::WAITING => EntryKey::waiting(self.lane, self.expires_at, id),
            keys::PARKED => EntryKey::parked(self.due_at, id),
            _ => EntryKey::accepted(self.expires_at, id),
        }
    }

    fn queued(&self) -> bool {
        self.kind == keys::WAITING || self.kind == keys::PARKED
    }
}

/// A row of the entry map: a notification, or where one stands.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Row {
    Entry(Entry),
    Slot(Slot),
}

impl Storable for Row {
    const BOUND: Bound = Bound::Bounded {
        max_size: 66,
        is_fixed_size: false,
    };

    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut bytes = Vec::with_capacity(66);
        match self {
            Row::Entry(entry) => {
                let who = entry.recipient.as_slice();
                bytes.push(0);
                bytes.extend_from_slice(&entry.id.to_be_bytes());
                bytes.extend_from_slice(&entry.expires_at.to_be_bytes());
                bytes.extend_from_slice(&entry.due_at.to_be_bytes());
                bytes.push(entry.lane);
                bytes.push(u8::from(entry.retried));
                bytes.push(who.len() as u8);
                bytes.extend_from_slice(who);
                // Last, so a row written before they existed still reads: what
                // is not there was not received, in no hour and no funnel.
                bytes.push(u8::from(entry.received));
                bytes.extend_from_slice(&entry.queued_hour.to_be_bytes());
                bytes.extend_from_slice(&entry.funnel.to_be_bytes());
                bytes.push(u8::from(entry.opened));
            }
            Row::Slot(slot) => {
                bytes.push(1);
                bytes.push(slot.kind);
                bytes.push(slot.lane);
                bytes.extend_from_slice(&slot.expires_at.to_be_bytes());
                bytes.extend_from_slice(&slot.due_at.to_be_bytes());
            }
        }
        Cow::Owned(bytes)
    }

    fn into_bytes(self) -> Vec<u8> {
        self.to_bytes().into_owned()
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        let word =
            |from: usize| u64::from_be_bytes(bytes[from..from + 8].try_into().expect("bounded"));
        let four = |from: usize| match bytes.get(from..from + 4) {
            Some(slice) => u32::from_be_bytes(slice.try_into().expect("four bytes")),
            None => 0,
        };
        let two = |from: usize| match bytes.get(from..from + 2) {
            Some(slice) => u16::from_be_bytes(slice.try_into().expect("two bytes")),
            None => 0,
        };
        match bytes[0] {
            0 => {
                let length = usize::from(bytes[27]);
                Row::Entry(Entry {
                    id: word(1),
                    expires_at: word(9),
                    due_at: word(17),
                    lane: bytes[25],
                    retried: bytes[26] == 1,
                    recipient: Principal::from_slice(&bytes[28..28 + length]),
                    received: bytes.get(28 + length).copied().unwrap_or(0) == 1,
                    queued_hour: four(29 + length),
                    funnel: two(33 + length),
                    opened: bytes.get(35 + length).copied().unwrap_or(0) == 1,
                })
            }
            _ => Row::Slot(Slot {
                kind: bytes[1],
                lane: bytes[2],
                expires_at: word(3),
                due_at: word(11),
            }),
        }
    }
}

/// The app's own text, held until a channel pulls it or it expires.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StoredContent(pub Content);

/// Stands in for a url the app did not give. No length can: an empty one is a
/// url an app may send, and reads back as the empty string it wrote.
const NO_URL: u32 = u32::MAX;

/// A length and then that many bytes.
fn push_field(bytes: &mut Vec<u8>, field: &[u8]) {
    bytes.extend_from_slice(&(field.len() as u32).to_be_bytes());
    bytes.extend_from_slice(field);
}

/// Reads a length, moving `rest` past it.
fn take_len(rest: &mut &[u8]) -> u32 {
    let (head, tail) = rest.split_at(4);
    *rest = tail;
    u32::from_be_bytes(head.try_into().expect("four bytes"))
}

/// Reads `len` bytes as text, moving `rest` past them.
fn take_text(rest: &mut &[u8], len: u32) -> String {
    let (head, tail) = rest.split_at(len as usize);
    *rest = tail;
    String::from_utf8(head.to_vec()).expect("stored text is utf-8")
}

/// Unbounded, so the map pages at a kilobyte instead of sizing every node for
/// the longest text an app might ever send, and text past a page spills into
/// pages of its own rather than being refused. Encoded by hand, field by field,
/// so what a notification costs to store is its own length and nothing more.
impl Storable for StoredContent {
    const BOUND: Bound = Bound::Unbounded;

    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let Content { title, body, url } = &self.0;
        let mut bytes =
            Vec::with_capacity(12 + title.len() + body.len() + url.as_ref().map_or(0, String::len));
        push_field(&mut bytes, title.as_bytes());
        push_field(&mut bytes, body.as_bytes());
        match url {
            Some(url) => push_field(&mut bytes, url.as_bytes()),
            None => bytes.extend_from_slice(&NO_URL.to_be_bytes()),
        }
        Cow::Owned(bytes)
    }

    fn into_bytes(self) -> Vec<u8> {
        self.to_bytes().into_owned()
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        let mut rest = bytes.as_ref();
        let title_len = take_len(&mut rest);
        let title = take_text(&mut rest, title_len);
        let body_len = take_len(&mut rest);
        let body = take_text(&mut rest, body_len);
        let url_len = take_len(&mut rest);
        let url = (url_len != NO_URL).then(|| take_text(&mut rest, url_len));
        StoredContent(Content { title, body, url })
    }
}

/// A row of the metrics map: an hour of the pipeline, or the library's
/// counters under `COUNTERS_ROW`. Hour zero is the epoch, so never a bucket.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MetricsRow {
    Hour(Bucket),
    Counters(Meta),
    /// A funnel an app named, and what became of what it labelled.
    Funnel(Funnel),
}

const HOUR_ROW: u8 = 0;
const COUNTER_ROW: u8 = 1;
const FUNNEL_ROW: u8 = 2;

fn push_counts(bytes: &mut Vec<u8>, counts: &Counts) {
    bytes.extend_from_slice(&counts.queued.to_be_bytes());
    bytes.extend_from_slice(&counts.accepted.to_be_bytes());
    bytes.extend_from_slice(&counts.received.to_be_bytes());
    bytes.extend_from_slice(&counts.opened.to_be_bytes());
    bytes.extend_from_slice(&counts.dropped.to_be_bytes());
    bytes.extend_from_slice(&counts.deferred.to_be_bytes());
}

fn take_counts(rest: &mut &[u8]) -> Counts {
    Counts {
        queued: take_u64(rest),
        accepted: take_u64(rest),
        received: take_u64(rest),
        opened: take_u64(rest),
        dropped: take_u64(rest),
        deferred: take_u64(rest),
    }
}

fn take_u8(rest: &mut &[u8]) -> u8 {
    let (head, tail) = rest.split_at(1);
    *rest = tail;
    head[0]
}

fn take_u64(rest: &mut &[u8]) -> u64 {
    let (head, tail) = rest.split_at(8);
    *rest = tail;
    u64::from_be_bytes(head.try_into().expect("eight bytes"))
}

/// Unbounded, because a bound would size every page for the longest funnel name
/// an app might name a campaign, where all but a handful of rows are an hour of
/// counters. Encoded field by field, a kind byte first, the way an entry row is.
impl Storable for MetricsRow {
    const BOUND: Bound = Bound::Unbounded;

    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut bytes = Vec::new();
        match self {
            MetricsRow::Hour(bucket) => {
                bytes.push(HOUR_ROW);
                bytes.extend_from_slice(&bucket.start.to_be_bytes());
                push_counts(&mut bytes, &bucket.counts);
            }
            MetricsRow::Counters(meta) => {
                bytes.push(COUNTER_ROW);
                bytes.extend_from_slice(&meta.counter.to_be_bytes());
                bytes.extend_from_slice(&meta.id_seed.to_be_bytes());
                bytes.extend_from_slice(&meta.bytes.to_be_bytes());
                bytes.extend_from_slice(&meta.batch_limit.to_be_bytes());
                bytes.extend_from_slice(&meta.queued.to_be_bytes());
                match &meta.misconfigured {
                    Some(since) => {
                        bytes.push(1);
                        bytes.push(match since.why {
                            Misconfigured::Origin => 0,
                            Misconfigured::Sender => 1,
                            Misconfigured::NotAuthorizedSender => 2,
                        });
                        bytes.extend_from_slice(&since.since.to_be_bytes());
                    }
                    None => bytes.push(0),
                }
            }
            MetricsRow::Funnel(funnel) => {
                bytes.push(FUNNEL_ROW);
                push_field(&mut bytes, funnel.name.as_bytes());
                push_counts(&mut bytes, &funnel.counts);
            }
        }
        Cow::Owned(bytes)
    }

    fn into_bytes(self) -> Vec<u8> {
        self.to_bytes().into_owned()
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        let mut rest = bytes.as_ref();
        match take_u8(&mut rest) {
            HOUR_ROW => MetricsRow::Hour(Bucket {
                start: take_u64(&mut rest),
                counts: take_counts(&mut rest),
            }),
            COUNTER_ROW => MetricsRow::Counters(Meta {
                counter: take_u64(&mut rest),
                id_seed: take_u64(&mut rest),
                bytes: take_u64(&mut rest),
                batch_limit: take_u64(&mut rest),
                queued: take_u64(&mut rest),
                misconfigured: (take_u8(&mut rest) == 1).then(|| MisconfiguredSince {
                    why: match take_u8(&mut rest) {
                        0 => Misconfigured::Origin,
                        1 => Misconfigured::Sender,
                        _ => Misconfigured::NotAuthorizedSender,
                    },
                    since: take_u64(&mut rest),
                }),
            }),
            _ => MetricsRow::Funnel(Funnel {
                name: {
                    let len = take_len(&mut rest);
                    take_text(&mut rest, len)
                },
                counts: take_counts(&mut rest),
            }),
        }
    }
}

/// An hour's row is keyed by its start in nanoseconds, so the counters and the
/// funnels take the top of the key space, which no hour reaches for another
/// half a trillion years.
const COUNTERS_ROW: u64 = u64::MAX;
const FUNNEL_BASE: u64 = u64::MAX - u16::MAX as u64 - 1;

const fn funnel_row(funnel: u16) -> u64 {
    FUNNEL_BASE + funnel as u64
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Meta {
    pub counter: u64,
    pub id_seed: u64,
    pub bytes: u64,
    pub batch_limit: u64,
    pub queued: u64,
    pub misconfigured: Option<MisconfiguredSince>,
}

pub struct Store {
    entries: BTreeMap<EntryKey, Row, Memory>,
    content: BTreeMap<NotificationId, StoredContent, Memory>,
    rows: BTreeMap<u64, MetricsRow, Memory>,
}

#[derive(Clone, Copy, Debug)]
pub enum Event {
    Queued,
    Accepted,
    Opened,
    Deferred,
    Received,
    Dropped,
}

impl Store {
    pub fn new(manager: &MemoryManager<DefaultMemoryImpl>, memories: Memories) -> Self {
        Self {
            entries: BTreeMap::init(manager.get(memories.entries)),
            content: BTreeMap::init(manager.get(memories.content)),
            rows: BTreeMap::init(manager.get(memories.metrics)),
        }
    }

    fn meta(&self) -> Meta {
        match self.rows.get(&COUNTERS_ROW) {
            Some(MetricsRow::Counters(meta)) => meta,
            _ => Meta::default(),
        }
    }

    fn set_meta(&mut self, meta: Meta) {
        self.rows.insert(COUNTERS_ROW, MetricsRow::Counters(meta));
    }

    /// The funnel a notification names, whether or not it is ever queued, so a
    /// call the ceiling refuses still counts where the app asked it to.
    pub fn funnel_of(&mut self, notification: &Notification) -> u16 {
        match notification.funnel.as_deref() {
            Some(name) if !name.is_empty() => self.funnel_id(name),
            _ => 0,
        }
    }

    /// Adds a notification, or replaces the one its key already names.
    /// `None` when the capacity ceiling refused it.
    pub fn add(
        &mut self,
        recipient: Principal,
        notification: Notification,
        default_expiry: u64,
        capacity_bytes: u64,
        now: u64,
    ) -> Option<NotificationId> {
        self.sweep(now);

        let funnel = self.funnel_of(&notification);

        let mut meta = self.meta();

        let id = match notification.key.as_deref() {
            Some(key) => ids::keyed(recipient, key),
            None => {
                meta.counter += 1;
                ids::keyless(meta.id_seed, meta.counter)
            }
        };

        let expires_at = notification.expires_at.unwrap_or(default_expiry);
        let content = Content {
            title: notification.title,
            body: notification.body,
            url: notification.url,
        };
        let size = size_of_content(&content);

        // Replacing a notification frees what the old one held, so an app that
        // updates one key forever occupies one notification's worth of room.
        if let Some(StoredContent(old)) = self.content.get(&id) {
            meta.bytes = meta.bytes.saturating_sub(size_of_content(&old));
        }
        self.set_meta(meta);

        if self.meta().bytes + size > capacity_bytes {
            self.shed(capacity_bytes, size);
            if self.meta().bytes + size > capacity_bytes {
                return None;
            }
        }

        self.content.insert(id, StoredContent(content));
        let mut meta = self.meta();
        meta.bytes += size;
        self.set_meta(meta);

        match self.slot_of(id) {
            // Still on its way out, so the entry stands: what this send brings
            // is content, a lane and a window of its own.
            Some(slot) if slot.queued() => {
                self.rewindow(id, expires_at);
                return Some(id);
            }
            // Accepted, so Internet Identity holds a notification whose content
            // has just changed. Sending it again replaces that one.
            Some(_) => {
                self.unplace(id);
            }
            None => {}
        }

        let entry = Entry {
            id,
            recipient,
            lane: lane_of(notification.urgency) as u8,
            expires_at,
            due_at: now,
            retried: false,
            received: false,
            queued_hour: (now / HOUR) as u32,
            funnel,
            opened: false,
        };
        self.place(entry, keys::WAITING, entry.expires_at);

        Some(id)
    }

    /// When the outbox next has something to do: now, where anything is
    /// waiting, or the soonest parked entry's turn. Nothing queued, no answer.
    pub fn next_due(&self) -> Option<u64> {
        let waiting = (0..LANES).any(|lane| {
            self.entries
                .range(EntryKey::lane_start(lane)..=EntryKey::lane_end(lane))
                .next()
                .is_some()
        });
        if waiting {
            return Some(0);
        }

        self.entries
            .range(EntryKey::parked_start()..=EntryKey::parked_due_by(u64::MAX))
            .find_map(|row| match row.value() {
                Row::Entry(entry) => Some(entry.due_at),
                Row::Slot(_) => None,
            })
    }

    /// Returns parked entries to their lanes once they are due.
    pub fn promote(&mut self, now: u64) {
        let due: Vec<Entry> = self
            .entries
            .range(EntryKey::parked_start()..=EntryKey::parked_due_by(now))
            .filter_map(|row| match row.value() {
                Row::Entry(entry) => Some(entry),
                Row::Slot(_) => None,
            })
            .collect();

        for entry in due {
            self.place(entry, keys::WAITING, entry.expires_at);
        }
    }

    /// A batch, in weighted cycles: `WEIGHTS[lane]` per lane per cycle, an
    /// empty lane donating its turns, soonest expiry leading its lane.
    pub fn take(&mut self, count: usize) -> Vec<Entry> {
        let mut taken = Vec::new();
        let mut cursors: Vec<Option<EntryKey>> = (0..LANES)
            .map(|lane| Some(EntryKey::lane_start(lane)))
            .collect();

        while taken.len() < count {
            let mut progressed = false;

            for (lane, &weight) in WEIGHTS.iter().enumerate() {
                for _ in 0..weight {
                    if taken.len() >= count {
                        break;
                    }
                    let Some(from) = cursors[lane] else { break };

                    match self
                        .entries
                        .range(from..=EntryKey::lane_end(lane as u8))
                        .next()
                        .map(|row| row.into_pair())
                    {
                        Some((key, Row::Entry(entry))) => {
                            self.entries.remove(&key);
                            taken.push(entry);
                            progressed = true;
                        }
                        Some(_) | None => {
                            cursors[lane] = None;
                            break;
                        }
                    }
                }
            }

            if !progressed {
                break;
            }
        }

        taken
    }

    /// Internet Identity took it: nothing more to send, and its content waits
    /// there for a pull until it expires.
    pub fn settle(&mut self, entry: &Entry) {
        self.place(*entry, keys::ACCEPTED, entry.expires_at);
    }

    /// One more attempt, later in the notification's window. `false` when this
    /// was that attempt and the notification is now dropped.
    pub fn park(&mut self, mut entry: Entry, now: u64) -> bool {
        if entry.retried {
            self.unplace(entry.id);
            self.release(entry.id);
            return false;
        }

        entry.retried = true;
        entry.due_at = retry_at(now, entry.expires_at);
        self.place(entry, keys::PARKED, entry.due_at);
        true
    }

    /// Internet Identity asked for this one later. Not a refusal, so it does
    /// not spend the second attempt.
    pub fn defer(&mut self, mut entry: Entry, retry_after: u64) {
        entry.due_at = retry_after;
        self.place(entry, keys::PARKED, entry.due_at);
    }

    /// Nothing was enqueued; the entry goes back unchanged.
    pub fn requeue(&mut self, mut entry: Entry, now: u64) {
        entry.due_at = now;
        self.place(entry, keys::WAITING, entry.expires_at);
    }

    /// What a batch may no longer be sent: anything past its expiry, which
    /// Internet Identity would refuse, and anything the app has since
    /// dismissed. Only an expiry counts as a drop; a dismissal is the app
    /// getting what it asked for.
    pub fn sendable(&mut self, batch: Vec<Entry>, now: u64) -> (Vec<Entry>, usize) {
        let mut live = Vec::with_capacity(batch.len());
        let mut expired = 0;

        for entry in batch {
            if entry.expires_at <= now {
                // Its window closed with Internet Identity never having taken
                // it, which is the cohort it came in with losing one.
                self.record_for(&entry, Event::Dropped, 1);
                self.unplace(entry.id);
                self.release(entry.id);
                expired += 1;
            } else if self.content.contains_key(&entry.id) {
                live.push(entry);
            } else {
                self.unplace(entry.id);
            }
        }

        (live, expired)
    }

    /// Drops what an expired notification was still holding. Internet Identity
    /// will not pull content it can no longer deliver.
    pub fn sweep(&mut self, now: u64) {
        let expired: Vec<Entry> = self
            .entries
            .range(EntryKey::accepted_start()..=EntryKey::accepted_expired_by(now))
            .filter_map(|row| match row.value() {
                Row::Entry(entry) => Some(entry),
                Row::Slot(_) => None,
            })
            .collect();

        for entry in expired {
            // Internet Identity held it and no channel ever showed it, which is
            // the delivery pipeline's quietest loss.
            if !entry.received {
                self.record_for(&entry, Event::Dropped, 1);
            }
            self.unplace(entry.id);
            self.release(entry.id);
        }
    }

    /// The app says this no longer needs anyone's attention: it leaves the
    /// outbox, and its content goes so a pull finds nothing.
    pub fn dismiss(&mut self, recipient: Principal, key: &str) {
        let id = ids::keyed(recipient, key);
        self.unplace(id);
        self.release(id);
    }

    /// Whether this is the first channel to report showing the notification.
    /// Every channel a recipient has is woken for it, so the report comes once
    /// per channel and the delivery is counted on the first.
    pub fn mark_received(&mut self, id: NotificationId) -> bool {
        let Some(slot) = self.slot_of(id) else {
            return false;
        };
        let Some(Row::Entry(mut entry)) = self.entries.get(&slot.key(id)) else {
            return false;
        };
        if entry.received {
            return false;
        }

        entry.received = true;
        self.entries.insert(slot.key(id), Row::Entry(entry));
        true
    }

    /// Whether this is the first time somebody acted on the notification.
    /// Counted once, like a delivery: several channels may show it, and the
    /// person opens it on one of them.
    pub fn mark_opened(&mut self, id: NotificationId) -> bool {
        let Some(slot) = self.slot_of(id) else {
            return false;
        };
        let Some(Row::Entry(mut entry)) = self.entries.get(&slot.key(id)) else {
            return false;
        };
        if entry.opened {
            return false;
        }

        entry.opened = true;
        self.entries.insert(slot.key(id), Row::Entry(entry));
        self.record_for(&entry, Event::Opened, 1);
        true
    }

    pub fn content_of(&self, id: NotificationId) -> Option<Content> {
        self.content.get(&id).map(|StoredContent(content)| content)
    }

    /// `false` when there is no such notification to have received.
    pub fn holds(&self, id: NotificationId) -> bool {
        self.content.contains_key(&id)
    }

    /// Notifications waiting for Internet Identity to accept them.
    pub fn backlog(&self) -> u64 {
        self.meta().queued
    }

    pub fn batch_limit(&self) -> usize {
        self.meta().batch_limit.max(1) as usize
    }

    pub fn set_batch_limit(&mut self, limit: usize) {
        let mut meta = self.meta();
        meta.batch_limit = limit as u64;
        self.set_meta(meta);
    }

    pub fn misconfigured(&self) -> Option<MisconfiguredSince> {
        self.meta().misconfigured
    }

    pub fn note(&mut self, why: Misconfigured, now: u64) {
        let mut meta = self.meta();
        if meta.misconfigured.map(|since| since.why) == Some(why) {
            return;
        }
        meta.misconfigured = Some(MisconfiguredSince { why, since: now });
        self.set_meta(meta);
    }

    pub fn is_seeded(&self) -> bool {
        self.meta().id_seed != 0
    }

    pub fn seed(&mut self, randomness: &[u8]) {
        let mut meta = self.meta();
        if meta.id_seed == 0 {
            meta.id_seed = ids::seed_from(randomness);
            self.set_meta(meta);
        }
    }

    /// Counts an event in the hour a notification came in, which is the hour it
    /// is counted in whenever its step happens, and in the funnel its app named.
    pub fn record_for(&mut self, entry: &Entry, event: Event, count: u64) {
        self.bump(u64::from(entry.queued_hour) * HOUR, event, count);
        if entry.funnel != 0 {
            self.bump(funnel_row(entry.funnel), event, count);
        }
    }

    /// Counts an event in the hour it happens, which for a notification coming
    /// in is the hour it belongs to.
    pub fn record(&mut self, now: u64, event: Event, count: u64) {
        self.bump(now - (now % HOUR), event, count);
    }

    /// The same count in a funnel row, for a notification whose label is known
    /// before its entry exists.
    pub fn record_in_funnel(&mut self, funnel: u16, event: Event, count: u64) {
        if funnel != 0 {
            self.bump(funnel_row(funnel), event, count);
        }
    }

    fn bump(&mut self, key: u64, event: Event, count: u64) {
        let mut row = match self.rows.get(&key) {
            Some(row) => row,
            // A funnel the app has forgotten is not brought back by a
            // notification still carrying it.
            None if key >= FUNNEL_BASE => return,
            None => MetricsRow::Hour(Bucket {
                start: key,
                ..Bucket::default()
            }),
        };

        let counts = match &mut row {
            MetricsRow::Hour(bucket) => &mut bucket.counts,
            MetricsRow::Funnel(funnel) => &mut funnel.counts,
            MetricsRow::Counters(_) => return,
        };
        match event {
            Event::Queued => counts.queued += count,
            Event::Accepted => counts.accepted += count,
            Event::Deferred => counts.deferred += count,
            Event::Received => counts.received += count,
            Event::Opened => counts.opened += count,
            Event::Dropped => counts.dropped += count,
        }

        self.rows.insert(key, row);

        // One row is the counters rather than an hour.
        while self.hour_rows() > KEEP_HOURS {
            let oldest = self.rows.range(..FUNNEL_BASE).next().map(|row| *row.key());
            match oldest {
                Some(hour) => {
                    self.rows.remove(&hour);
                }
                None => break,
            }
        }
    }

    pub fn hours(&self) -> Vec<Bucket> {
        self.rows
            .range(..FUNNEL_BASE)
            .filter_map(|row| match row.value() {
                MetricsRow::Hour(bucket) => Some(bucket),
                MetricsRow::Counters(_) | MetricsRow::Funnel(_) => None,
            })
            .collect()
    }

    fn hour_rows(&self) -> usize {
        self.rows.range(..FUNNEL_BASE).count()
    }

    pub fn funnels(&self) -> Vec<Funnel> {
        self.rows
            .range(FUNNEL_BASE..COUNTERS_ROW)
            .filter_map(|row| match row.value() {
                MetricsRow::Funnel(funnel) => Some(funnel),
                MetricsRow::Counters(_) | MetricsRow::Hour(_) => None,
            })
            .collect()
    }

    /// The row this label counts in, opening one where the app has not used it
    /// before. `0` is no funnel at all.
    pub fn funnel_id(&mut self, name: &str) -> u16 {
        let mut next = funnel_row(1);
        for row in self.rows.range(FUNNEL_BASE..COUNTERS_ROW) {
            if let MetricsRow::Funnel(funnel) = row.value() {
                if funnel.name == name {
                    return (*row.key() - FUNNEL_BASE) as u16;
                }
            }
            next = *row.key() + 1;
        }
        if next == COUNTERS_ROW {
            return 0;
        }

        self.rows.insert(
            next,
            MetricsRow::Funnel(Funnel {
                name: name.to_string(),
                ..Funnel::default()
            }),
        );
        (next - FUNNEL_BASE) as u16
    }

    /// Drops a funnel's row. What it counted goes with it, and notifications
    /// still carrying it are counted in their hour alone.
    pub fn forget_funnel(&mut self, name: &str) {
        let row = self
            .rows
            .range(FUNNEL_BASE..COUNTERS_ROW)
            .find_map(|row| match row.value() {
                MetricsRow::Funnel(funnel) if funnel.name == name => Some(*row.key()),
                _ => None,
            });
        if let Some(key) = row {
            self.rows.remove(&key);
        }
    }

    /// Carries a fresh window to an entry that is still queued, leaving where it
    /// stands, when it is due and the lane it was queued in alone.
    ///
    /// An entry that has spent its second attempt keeps the window it was
    /// refused in, so a dormant recipient costs an app one round of refusals
    /// however often it repeats the key. Nothing refused a deferred entry, so a
    /// send that finds one waiting on Internet Identity's own timetable is
    /// given the window it asks for.
    fn rewindow(&mut self, id: NotificationId, expires_at: u64) {
        let Some(slot) = self.slot_of(id) else { return };
        let Some(Row::Entry(mut entry)) = self.entries.get(&slot.key(id)) else {
            return;
        };
        if entry.retried {
            return;
        }

        entry.expires_at = expires_at;
        let at = match slot.kind {
            keys::PARKED => entry.due_at,
            _ => entry.expires_at,
        };
        self.place(entry, slot.kind, at);
    }

    fn slot_of(&self, id: NotificationId) -> Option<Slot> {
        match self.entries.get(&EntryKey::slot(id)) {
            Some(Row::Slot(slot)) => Some(slot),
            _ => None,
        }
    }

    /// Puts an entry in one of the three orders and records where it went.
    fn place(&mut self, entry: Entry, kind: u8, at: u64) {
        let slot = Slot {
            kind,
            lane: entry.lane,
            expires_at: entry.expires_at,
            due_at: at,
        };

        let was = self.slot_of(entry.id);
        if let Some(old) = was {
            self.entries.remove(&old.key(entry.id));
        }

        self.entries.insert(slot.key(entry.id), Row::Entry(entry));
        self.entries
            .insert(EntryKey::slot(entry.id), Row::Slot(slot));
        self.count(was.is_some_and(|old| old.queued()), slot.queued());
    }

    /// Takes an entry out of every order it is in.
    fn unplace(&mut self, id: NotificationId) {
        let Some(slot) = self.slot_of(id) else { return };
        self.entries.remove(&slot.key(id));
        self.entries.remove(&EntryKey::slot(id));
        self.count(slot.queued(), false);
    }

    fn count(&mut self, was_queued: bool, is_queued: bool) {
        if was_queued == is_queued {
            return;
        }
        let mut meta = self.meta();
        meta.queued = if is_queued {
            meta.queued + 1
        } else {
            meta.queued.saturating_sub(1)
        };
        self.set_meta(meta);
    }

    fn release(&mut self, id: NotificationId) {
        if let Some(StoredContent(content)) = self.content.remove(&id) {
            let mut meta = self.meta();
            meta.bytes = meta.bytes.saturating_sub(size_of_content(&content));
            self.set_meta(meta);
        }
    }

    /// Sheds the outbox, lowest lane first. Never accepted content: a pull
    /// has to find it.
    fn shed(&mut self, capacity_bytes: u64, needed: u64) {
        for lane in (0..LANES).rev() {
            while self.meta().bytes + needed > capacity_bytes {
                let Some(entry) = self
                    .entries
                    .range(EntryKey::lane_start(lane)..=EntryKey::lane_end(lane))
                    .next()
                    .and_then(|row| match row.value() {
                        Row::Entry(entry) => Some(entry),
                        Row::Slot(_) => None,
                    })
                else {
                    break;
                };

                self.unplace(entry.id);
                self.release(entry.id);
            }
        }
    }
}

pub fn lane_of(urgency: Option<crate::types::Urgency>) -> usize {
    use crate::types::Urgency;
    match urgency.unwrap_or_default() {
        Urgency::High => 0,
        Urgency::Normal => 1,
        Urgency::Low => 2,
        Urgency::VeryLow => 3,
    }
}

fn size_of_content(content: &Content) -> u64 {
    (content.title.len() + content.body.len() + content.url.as_ref().map_or(0, String::len) + 64)
        as u64
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Urgency;

    fn counts() -> Counts {
        // Distinct per field, so a transposed pair cannot round-trip.
        Counts {
            queued: 1,
            accepted: 2,
            received: 3,
            opened: 4,
            dropped: 5,
            deferred: 6,
        }
    }

    fn row_round_trip(row: MetricsRow) {
        assert_eq!(MetricsRow::from_bytes(row.to_bytes()), row);
    }

    #[test]
    fn an_hour_row_round_trips() {
        row_round_trip(MetricsRow::Hour(Bucket {
            start: 7 * HOUR,
            counts: counts(),
        }));
    }

    #[test]
    fn a_counters_row_round_trips() {
        row_round_trip(MetricsRow::Counters(Meta {
            counter: 1,
            id_seed: 2,
            bytes: 3,
            batch_limit: 4,
            queued: 5,
            misconfigured: None,
        }));
    }

    #[test]
    fn a_counters_row_round_trips_every_reason_it_is_misconfigured() {
        for why in [
            Misconfigured::Origin,
            Misconfigured::Sender,
            Misconfigured::NotAuthorizedSender,
        ] {
            row_round_trip(MetricsRow::Counters(Meta {
                counter: 1,
                id_seed: 2,
                bytes: 3,
                batch_limit: 4,
                queued: 5,
                misconfigured: Some(MisconfiguredSince { why, since: 9 }),
            }));
        }
    }

    #[test]
    fn a_funnel_row_round_trips() {
        row_round_trip(MetricsRow::Funnel(Funnel {
            name: "checkout".into(),
            counts: counts(),
        }));
    }

    #[test]
    fn a_funnel_name_is_kept_whole() {
        let named = "🎯".repeat(400);
        let row = MetricsRow::Funnel(Funnel {
            name: named.clone(),
            counts: counts(),
        });

        let MetricsRow::Funnel(read) = MetricsRow::from_bytes(row.to_bytes()) else {
            panic!("a funnel row");
        };
        assert_eq!(read.name, named);
    }

    #[test]
    fn a_funnel_keeps_the_name_the_app_gave_it() {
        let mut store = store();
        let named = "spring campaign — 🌷 all of it, spelled out in full".repeat(20);

        let funnel = store.funnel_id(&named);

        assert_eq!(
            store
                .funnels()
                .into_iter()
                .find(|funnel| funnel.name == named)
                .map(|funnel| funnel.name),
            Some(named.clone()),
            "a name the app can no longer recognise is no label at all"
        );
        assert_eq!(store.funnel_id(&named), funnel, "and it is the same funnel");
    }

    fn round_trip(content: Content) {
        let stored = StoredContent(content);
        let bytes = stored.to_bytes();
        assert_eq!(StoredContent::from_bytes(bytes), stored);
    }

    #[test]
    fn content_round_trips() {
        round_trip(Content {
            title: "New message".into(),
            body: "See you at six".into(),
            url: Some("/chats/7".into()),
        });
    }

    #[test]
    fn content_round_trips_without_a_url() {
        round_trip(Content {
            title: "New message".into(),
            body: "See you at six".into(),
            url: None,
        });
    }

    #[test]
    fn an_empty_url_is_not_a_missing_one() {
        let empty = StoredContent(Content {
            title: String::new(),
            body: String::new(),
            url: Some(String::new()),
        });
        let missing = StoredContent(Content {
            title: String::new(),
            body: String::new(),
            url: None,
        });

        assert_ne!(empty.to_bytes(), missing.to_bytes());
        assert_eq!(
            StoredContent::from_bytes(empty.to_bytes()).0.url,
            Some(String::new())
        );
        assert_eq!(StoredContent::from_bytes(missing.to_bytes()).0.url, None);
    }

    #[test]
    fn content_round_trips_text_that_is_not_ascii() {
        round_trip(Content {
            title: "Diner om zes? 🍝".into(),
            body: "Ja — tot dan".into(),
            url: Some("/chats/zes?q=één".into()),
        });
    }

    #[test]
    fn content_round_trips_past_a_page() {
        round_trip(Content {
            title: "x".repeat(4_096),
            body: "y".repeat(16_384),
            url: Some("z".repeat(1_024)),
        });
    }

    #[test]
    fn content_costs_its_own_length() {
        let stored = StoredContent(Content {
            title: "ab".into(),
            body: "cde".into(),
            url: Some("f".into()),
        });

        // Three lengths and the bytes they count, and nothing besides.
        assert_eq!(stored.to_bytes().len(), 12 + 2 + 3 + 1);
    }

    const HOUR: u64 = 3_600_000_000_000;
    const MINUTE: u64 = 60_000_000_000;
    const ROOMY: u64 = 1_000_000;
    const MEMORY_IDS: Memories = Memories {
        entries: MemoryId::new(0),
        content: MemoryId::new(1),
        metrics: MemoryId::new(2),
    };

    /// Off-canister, `DefaultMemoryImpl` is a plain vector, so the whole store
    /// runs in a test without a replica.
    fn store() -> Store {
        let manager = Box::leak(Box::new(MemoryManager::init(DefaultMemoryImpl::default())));
        Store::new(manager, MEMORY_IDS)
    }

    fn alice() -> Principal {
        Principal::from_text("un4fu-tqaaa-aaaab-qadjq-cai").unwrap()
    }

    fn note(title: &str, key: Option<&str>, urgency: Option<Urgency>) -> Notification {
        Notification {
            title: title.into(),
            body: "body".into(),
            key: key.map(str::to_string),
            urgency,
            ..Default::default()
        }
    }

    #[test]
    fn the_same_key_replaces_rather_than_adds() {
        let mut store = store();
        let first = store.add(alice(), note("one", Some("k"), None), HOUR, ROOMY, 0);
        let second = store.add(alice(), note("two", Some("k"), None), HOUR, ROOMY, 0);

        assert_eq!(first, second);
        assert_eq!(store.content_of(first.unwrap()).unwrap().title, "two");
        assert_eq!(store.take(10).len(), 1, "one entry, not two");
    }

    #[test]
    fn a_batch_comes_out_by_lane_weight_and_then_by_expiry() {
        let mut store = store();
        for i in 0..20 {
            store.add(
                alice(),
                note("campaign", Some(&format!("c{i}")), Some(Urgency::VeryLow)),
                HOUR + i,
                ROOMY,
                0,
            );
            store.add(
                alice(),
                note("urgent", Some(&format!("u{i}")), Some(Urgency::High)),
                HOUR + i,
                ROOMY,
                0,
            );
        }

        let batch = store.take(9);
        let urgent = batch.iter().filter(|entry| entry.lane == 0).count();
        assert_eq!((urgent, batch.len() - urgent), (8, 1), "weights are 8 : 1");

        let urgent_expiries: Vec<u64> = batch
            .iter()
            .filter(|entry| entry.lane == 0)
            .map(|entry| entry.expires_at)
            .collect();
        let mut sorted = urgent_expiries.clone();
        sorted.sort_unstable();
        assert_eq!(urgent_expiries, sorted, "soonest expiry leads its lane");
    }

    #[test]
    fn a_refusal_parks_inside_the_window_and_a_second_one_drops_it() {
        let mut store = store();
        let id = store
            .add(alice(), note("x", Some("k"), None), 30 * HOUR, ROOMY, 0)
            .unwrap();

        let first = store.take(10);
        assert!(store.park(first[0], 0));

        store.promote(HOUR);
        assert!(store.take(10).is_empty(), "not due yet");

        store.promote(20 * HOUR);
        let second = store.take(10);
        assert_eq!(second.len(), 1);
        assert!(
            second[0].due_at < second[0].expires_at,
            "the attempt lands before the expiry it is for"
        );
        assert!(
            !store.park(second[0], 20 * HOUR),
            "a second refusal drops it"
        );
        assert!(store.content_of(id).is_none());
        assert_eq!(store.backlog(), 0);
    }

    #[test]
    fn a_five_minute_notification_still_gets_its_second_attempt() {
        let five_minutes = 300_000_000_000;
        let due = retry_at(0, five_minutes);

        assert!(due > 0, "not immediately, or the attempt is the same call");
        assert!(due < five_minutes, "and not after it has expired");
    }

    #[test]
    fn the_ceiling_sheds_the_campaign_and_keeps_the_urgent() {
        let mut store = store();
        let tiny = 900;

        let campaign: Vec<NotificationId> = (0..20)
            .filter_map(|i| {
                store.add(
                    alice(),
                    note("campaign", Some(&format!("c{i}")), Some(Urgency::VeryLow)),
                    HOUR,
                    tiny,
                    0,
                )
            })
            .collect();
        let urgent = store
            .add(
                alice(),
                note("urgent", Some("u"), Some(Urgency::High)),
                HOUR,
                tiny,
                0,
            )
            .expect("room is made for it");

        let kept = campaign
            .iter()
            .filter(|id| store.content_of(**id).is_some())
            .count();
        assert!(
            kept < campaign.len(),
            "the campaign paid for the room: {kept} of {} still held",
            campaign.len()
        );
        assert!(store.content_of(urgent).is_some());
        assert_eq!(store.backlog() as usize, kept + 1, "what is shed is gone");
    }

    #[test]
    fn a_sweep_frees_the_room_accepted_content_was_holding() {
        let mut store = store();
        // Room for one notification and no more.
        let tiny = 100;

        let filler = store
            .add(alice(), note("filler", Some("f"), None), HOUR, tiny, 0)
            .expect("the first one fits");
        let batch = store.take(10);
        store.settle(&batch[0]);

        // At the ceiling with nothing left to shed, because accepted content
        // is never shed.
        assert!(store
            .add(alice(), note("x", Some("x"), None), HOUR, tiny, 0)
            .is_none());

        // Past the filler's expiry there is room again, and the add sweeps.
        assert!(store
            .add(alice(), note("x", Some("x"), None), 2 * HOUR, tiny, HOUR)
            .is_some());
        assert!(!store.holds(filler), "it expired an hour ago");
    }

    #[test]
    fn a_deferral_does_not_spend_the_second_attempt() {
        let mut store = store();
        store.add(alice(), note("x", Some("k"), None), 30 * HOUR, ROOMY, 0);

        let first = store.take(10);
        store.defer(first[0], 2 * HOUR);
        store.promote(2 * HOUR);

        let deferred = store.take(10);
        assert_eq!(deferred.len(), 1, "a deferral comes back when asked for");
        assert!(
            store.park(deferred[0], 2 * HOUR),
            "the attempt is still there"
        );

        store.promote(30 * HOUR);
        let last = store.take(10);
        assert!(!store.park(last[0], 25 * HOUR), "and only one of them");
    }

    #[test]
    fn accepted_content_goes_when_the_notification_expires() {
        let mut store = store();
        let id = store
            .add(alice(), note("x", Some("k"), None), HOUR, ROOMY, 0)
            .unwrap();

        let batch = store.take(10);
        store.settle(&batch[0]);
        assert_eq!(store.backlog(), 0, "accepted is not backlog");

        store.sweep(HOUR - 1);
        assert!(store.holds(id), "a channel may still pull it");

        store.sweep(HOUR);
        assert!(!store.holds(id), "nothing will pull it now");
    }

    #[test]
    fn a_dismissed_notification_is_never_sent() {
        let mut store = store();
        store.add(alice(), note("x", Some("k"), None), HOUR, ROOMY, 0);
        store.dismiss(alice(), "k");

        assert_eq!(store.backlog(), 0);
        let batch = store.take(10);
        let (live, expired) = store.sendable(batch, 0);
        assert!(live.is_empty());
        assert_eq!(expired, 0, "a dismissal is not a delivery failure");
    }

    #[test]
    fn an_expired_notification_is_dropped_rather_than_sent() {
        let mut store = store();
        let id = store
            .add(alice(), note("x", Some("k"), None), HOUR, ROOMY, 0)
            .unwrap();

        let batch = store.take(10);
        let (live, expired) = store.sendable(batch, HOUR);

        assert!(live.is_empty());
        assert_eq!(expired, 1);
        assert!(!store.holds(id));
        assert_eq!(store.backlog(), 0);
    }

    #[test]
    fn hours_accumulate_and_the_ring_keeps_a_month() {
        let mut store = store();
        store.record(5 * HOUR + 1, Event::Queued, 3);
        store.record(5 * HOUR + 2_000, Event::Accepted, 2);

        let hours = store.hours();
        assert_eq!(hours.len(), 1);
        assert_eq!((hours[0].counts.queued, hours[0].counts.accepted), (3, 2));
        assert_eq!(hours[0].start, 5 * HOUR);

        for hour in 0..800u64 {
            store.record(hour * HOUR, Event::Queued, 1);
        }
        let ring = store.hours();
        assert_eq!(ring.len(), KEEP_HOURS);
        assert_eq!(ring[0].start, 80 * HOUR, "the oldest hours went");
        assert_eq!(ring[KEEP_HOURS - 1].start, 799 * HOUR);
    }

    #[test]
    fn nothing_is_lost_across_a_reopen() {
        let manager = Box::leak(Box::new(MemoryManager::init(DefaultMemoryImpl::default())));
        let id = {
            let mut store = Store::new(manager, MEMORY_IDS);
            store.add(alice(), note("pending", Some("k"), None), HOUR, ROOMY, 0);
            store.record(HOUR, Event::Queued, 1);
            store.set_batch_limit(17);
            ids::keyed(alice(), "k")
        };

        let reopened = Store::new(manager, MEMORY_IDS);
        assert!(reopened.content_of(id).is_some(), "content survives");
        assert_eq!(reopened.backlog(), 1, "the outbox survives");
        assert_eq!(reopened.hours().len(), 1, "metrics survive");
        assert_eq!(reopened.batch_limit(), 17, "the learned limit survives");
    }

    #[test]
    fn a_deferred_entry_takes_the_window_of_the_send_that_finds_it() {
        let mut store = store();
        let id = store
            .add(alice(), note("one", Some("k"), None), MINUTE, ROOMY, 0)
            .unwrap();
        let entry = store.take(1).pop().unwrap();
        store.defer(entry, 2 * MINUTE);

        store.add(
            alice(),
            note("two", Some("k"), None),
            5 * MINUTE,
            ROOMY,
            MINUTE,
        );

        assert_eq!(
            store.next_due(),
            Some(2 * MINUTE),
            "still on II's timetable"
        );
        store.promote(2 * MINUTE);
        let batch = store.take(1);
        let (live, expired) = store.sendable(batch, 2 * MINUTE);
        assert_eq!(
            expired, 0,
            "the second send is not stuck with the first window"
        );
        assert_eq!(live.len(), 1);
        assert_eq!(store.content_of(id).unwrap().title, "two");
    }

    #[test]
    fn a_refused_entry_keeps_the_window_it_was_refused_in() {
        let mut store = store();
        store.add(alice(), note("one", Some("k"), None), MINUTE, ROOMY, 0);
        let entry = store.take(1).pop().unwrap();
        store.park(entry, 0);

        store.add(
            alice(),
            note("two", Some("k"), None),
            5 * MINUTE,
            ROOMY,
            MINUTE,
        );
        store.promote(MINUTE);

        let batch = store.take(1);
        let (live, expired) = store.sendable(batch, MINUTE + 1);
        assert_eq!(
            (live.len(), expired),
            (0, 1),
            "one round of refusals, no more"
        );
    }

    #[test]
    fn nothing_queued_is_nothing_to_wake_for() {
        let mut store = store();
        assert_eq!(store.next_due(), None);

        store.add(alice(), note("one", Some("k"), None), MINUTE, ROOMY, 0);
        assert_eq!(store.next_due(), Some(0), "waiting, so now");

        let entry = store.take(1).pop().unwrap();
        store.defer(entry, 3 * MINUTE);
        assert_eq!(store.next_due(), Some(3 * MINUTE), "when II asked");
    }

    #[test]
    fn a_step_counts_in_the_hour_the_notification_came_in() {
        let mut store = store();
        store.record(0, Event::Queued, 1);
        store.add(alice(), note("one", Some("k"), None), 2 * HOUR, ROOMY, 0);
        let entry = store.take(1).pop().unwrap();

        // Accepted an hour after it was queued, which is a different bucket.
        store.record_for(&entry, Event::Accepted, 1);

        let hours = store.hours();
        assert_eq!(hours.len(), 1, "one cohort, not two");
        assert_eq!((hours[0].counts.queued, hours[0].counts.accepted), (1, 1));
    }

    #[test]
    fn a_delivery_counts_once_however_many_channels_show_it() {
        let mut store = store();
        let id = store
            .add(alice(), note("one", Some("k"), None), HOUR, ROOMY, 0)
            .unwrap();

        assert!(store.mark_received(id), "the first channel to report it");
        assert!(!store.mark_received(id), "the second is the same delivery");
    }

    #[test]
    fn a_notification_no_channel_ever_showed_is_a_drop() {
        let mut store = store();
        store.add(alice(), note("one", Some("k"), None), HOUR, ROOMY, 0);
        let entry = store.take(1).pop().unwrap();
        store.settle(&entry);

        store.sweep(HOUR);

        let hours = store.hours();
        assert_eq!(hours[0].counts.dropped, 1);
    }

    #[test]
    fn a_delivered_notification_expiring_is_no_drop() {
        let mut store = store();
        let id = store
            .add(alice(), note("one", Some("k"), None), HOUR, ROOMY, 0)
            .unwrap();
        let entry = store.take(1).pop().unwrap();
        store.settle(&entry);
        assert!(store.mark_received(id));

        store.sweep(HOUR);

        assert!(store.hours().iter().all(|hour| hour.counts.dropped == 0));
    }

    #[test]
    fn a_funnel_counts_an_ask_that_never_became_a_notification() {
        // What `send` does for an app it cannot send for: the ask is counted, and
        // so is the loss, both in the funnel the app named.
        let mut store = store();
        let funnel = store.funnel_id("promo");
        store.record(0, Event::Queued, 1);
        store.record_in_funnel(funnel, Event::Queued, 1);
        store.record(0, Event::Dropped, 1);
        store.record_in_funnel(funnel, Event::Dropped, 1);

        let funnels = store.funnels();
        assert_eq!(
            (funnels[0].counts.queued, funnels[0].counts.dropped),
            (1, 1)
        );
        assert_eq!(store.hours()[0].counts.queued, 1);
    }

    #[test]
    fn an_open_counts_once_and_only_while_the_window_is_open() {
        let mut store = store();
        let id = store
            .add(alice(), note("one", Some("k"), None), HOUR, ROOMY, 0)
            .unwrap();
        let entry = store.take(1).pop().unwrap();
        store.settle(&entry);

        assert!(store.mark_opened(id), "somebody acted on it");
        assert!(!store.mark_opened(id), "and cannot act on it twice");
        assert_eq!(store.hours()[0].counts.opened, 1);

        store.sweep(HOUR);
        assert!(!store.mark_opened(id), "its window has closed");
        assert_eq!(store.hours()[0].counts.opened, 1);
    }

    #[test]
    fn a_labelled_notification_counts_in_its_funnel_and_its_hour() {
        let mut store = store();
        let mut labelled = note("one", Some("k"), None);
        labelled.funnel = Some("promo".into());
        store.add(alice(), labelled, HOUR, ROOMY, 0);
        let entry = store.take(1).pop().unwrap();
        store.record_for(&entry, Event::Accepted, 1);

        let funnels = store.funnels();
        assert_eq!(funnels.len(), 1);
        assert_eq!(funnels[0].name, "promo");
        assert_eq!(funnels[0].counts.accepted, 1);
        assert_eq!(store.hours()[0].counts.accepted, 1, "and in its hour");
    }

    #[test]
    fn a_forgotten_funnel_takes_its_counts_and_is_not_reopened() {
        let mut store = store();
        let mut labelled = note("one", Some("k"), None);
        labelled.funnel = Some("promo".into());
        store.add(alice(), labelled, HOUR, ROOMY, 0);
        let entry = store.take(1).pop().unwrap();

        store.forget_funnel("promo");
        store.record_for(&entry, Event::Accepted, 1);

        assert!(store.funnels().is_empty());
        assert_eq!(store.hours()[0].counts.accepted, 1, "its hour still counts");
    }
}
