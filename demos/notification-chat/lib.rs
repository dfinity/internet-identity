//! One chatroom, to try Internet Identity notifications end to end.
//!
//! Every message notifies everyone else in the room, under one key, so what a
//! recipient sees is always the latest message. Members can claim a name,
//! unique among the names claimed. The canister serves its own frontend, and
//! lists itself as the one canister allowed to notify for its origin.

use asset_util::{
    collect_assets, Asset, CertifiedAssets, ContentEncoding, ContentType, HeaderField,
};
use candid::{CandidType, Deserialize, Principal};
use ic_cdk::api::{canister_self, certified_data_set, msg_caller, time, trap};
use ic_cdk::{init, post_upgrade, query, update};
use ic_stable_structures::memory_manager::{MemoryId, MemoryManager, VirtualMemory};
use ic_stable_structures::{DefaultMemoryImpl, StableBTreeMap};
use identity_notifications as notifications;
use identity_notifications::{Memories, Metrics, Notification};
use include_dir::{include_dir, Dir};
use serde_bytes::ByteBuf;
use std::cell::RefCell;

mod names;
mod room;
use room::{Refused, Room};

type Memory = VirtualMemory<DefaultMemoryImpl>;

/// The library's memories are 0 to 2; this app keeps out of them.
const NAMES_MEMORY: MemoryId = MemoryId::new(3);

// The endpoints Internet Identity calls: the content of a notification this
// canister sent, and the reports that one was shown and acted on.
notifications::endpoints!();

/// The canisters Internet Identity accepts notifications for this origin from.
/// This canister sends its own, so it lists itself.
const NOTIFICATION_SENDERS_PATH: &str = "/.well-known/ii-notification-senders";

/// What every notification is about. One key, so a new message replaces the
/// notification a recipient has not acted on yet rather than stacking beside
/// it: the one they see carries the last message.
const NOTIFICATION_KEY: &str = "room";

static ASSET_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/dist");

thread_local! {
    static ROOM: RefCell<Room> = RefCell::new(Room::default());
    static ASSETS: RefCell<CertifiedAssets> = RefCell::new(CertifiedAssets::default());
    static MEMORIES: RefCell<MemoryManager<DefaultMemoryImpl>> =
        RefCell::new(MemoryManager::init(DefaultMemoryImpl::default()));
    /// Who is called what. In stable memory, unlike the room: a name is a
    /// fact about an identity, and outlives the room an upgrade empties.
    static NAMES: RefCell<StableBTreeMap<Principal, String, Memory>> = RefCell::new(
        StableBTreeMap::init(MEMORIES.with_borrow(|manager| manager.get(NAMES_MEMORY))),
    );
}

#[init]
fn init() {
    init_assets();
    MEMORIES.with_borrow(|manager| {
        notifications::init(
            manager,
            Memories {
                entries: MemoryId::new(0),
                content: MemoryId::new(1),
                metrics: MemoryId::new(2),
            },
        )
    });
}

/// The room lives on the heap, so an upgrade empties it. The names and the
/// notification library's state are in stable memory and only need their
/// handles back.
#[post_upgrade]
fn post_upgrade() {
    init()
}

// ===== The room =====

/// Joins the room, which is what makes the caller a recipient of what others
/// send. Nothing happens for somebody already in.
#[update]
fn join() {
    let who = signed_in_caller();
    ROOM.with_borrow_mut(|room| room.join(who, time()))
        .unwrap_or_else(refuse)
}

/// Leaves the room, so the caller stops being notified.
#[update]
fn leave() {
    let who = signed_in_caller();
    ROOM.with_borrow_mut(|room| room.leave(who))
}

/// Posts a message and notifies everyone else in the room.
#[update]
fn send(text: String) {
    let from = signed_in_caller();
    let posted = ROOM
        .with_borrow_mut(|room| room.post(from, &text, time()))
        .unwrap_or_else(refuse);

    let author = name_of(from).unwrap_or_else(|| short(from));
    for recipient in posted.recipients {
        notifications::send(
            recipient,
            Notification {
                // Internet Identity puts this app's name in front of the title,
                // so the title itself names who wrote it.
                title: author.clone(),
                body: posted.message.text.clone(),
                url: app_url(),
                key: Some(NOTIFICATION_KEY.to_string()),
                ..Default::default()
            },
        );
    }
}

/// The room, with the names of everyone in it or heard in it.
#[query]
fn room() -> Room {
    let mut room = ROOM.with_borrow(Clone::clone);
    let mut named: Vec<Principal> = room
        .members
        .iter()
        .map(|member| member.who)
        .chain(room.messages.iter().map(|message| message.from))
        .collect();
    named.sort();
    named.dedup();
    room.names = named
        .into_iter()
        .filter_map(|who| name_of(who).map(|name| (who, name)))
        .collect();
    room
}

/// Claims a name for the caller, replacing the one they had: the name as
/// written where nobody else has it, else the first free `name_1`, `name_2`,
/// … Returns the name given. Names are kept across upgrades.
#[update]
fn claim_name(wanted: String) -> String {
    let who = signed_in_caller();
    NAMES.with_borrow_mut(|names| {
        let taken_by_others = |candidate: &str| {
            names
                .iter()
                .any(|entry| *entry.key() != who && names::same(&entry.value(), candidate))
        };
        let Some(name) = names::assign(&wanted, taken_by_others) else {
            trap("a name needs at least one character");
        };
        names.insert(who, name.clone());
        name
    })
}

fn name_of(who: Principal) -> Option<String> {
    NAMES.with_borrow(|names| names.get(&who))
}

/// The notification library's delivery metrics, an hour per bucket.
#[query]
fn notification_metrics() -> Metrics {
    notifications::metrics()
}

/// What the notification library is configured with, read from the canister's
/// environment as the library reads it. The library notes a misconfiguration
/// when a send hits one and never takes the note back, so this is what says
/// whether one still stands.
#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct NotificationConfig {
    /// `notification_origin`, where it is set and not empty.
    pub origin: Option<String>,
    /// `notification_sender`, where it is set and a principal.
    pub sender: Option<Principal>,
}

#[query]
fn notification_config() -> NotificationConfig {
    NotificationConfig {
        origin: configured_origin(),
        sender: environment("notification_sender")
            .and_then(|sender| Principal::from_text(sender).ok()),
    }
}

/// An environment variable the canister was deployed with, where it is set
/// and not empty.
fn environment(name: &str) -> Option<String> {
    #[cfg(target_arch = "wasm32")]
    {
        use ic_cdk::api::{env_var_name_exists, env_var_value};

        if env_var_name_exists(name) {
            let value = env_var_value(name);
            if !value.is_empty() {
                return Some(value);
            }
        }
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = name;
        None
    }
}

fn configured_origin() -> Option<String> {
    environment("notification_origin")
}

fn signed_in_caller() -> Principal {
    let caller = msg_caller();
    if caller == Principal::anonymous() {
        trap("sign in first");
    }
    caller
}

fn refuse<T>(refused: Refused) -> T {
    trap(refused.message())
}

/// A principal as the frontend shows it: its first and last groups.
fn short(principal: Principal) -> String {
    let text = principal.to_text();
    let mut groups = text.split('-');
    match (groups.next(), groups.next_back()) {
        (Some(first), Some(last)) => format!("{first}…{last}"),
        _ => text,
    }
}

/// Where acting on a notification takes the user: this app. Internet Identity
/// refuses a URL off the sending origin, which is the origin the library is
/// configured with, so that is the one named here.
fn app_url() -> Option<String> {
    configured_origin().map(|origin| format!("{origin}/"))
}

// ===== The frontend =====

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpRequest {
    pub method: String,
    pub url: String,
    pub headers: Vec<HeaderField>,
    pub body: ByteBuf,
    pub certificate_version: Option<u16>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpResponse {
    pub status_code: u16,
    pub headers: Vec<HeaderField>,
    pub body: ByteBuf,
}

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
    let path = req.url.split('?').next().unwrap_or("/");
    ASSETS
        .with_borrow(|assets| assets.get_certified_asset(path, req.certificate_version, None))
        .map(|asset| {
            let mut headers = asset.headers;
            headers.extend(static_headers());
            HttpResponse {
                status_code: asset.status_code,
                headers,
                body: ByteBuf::from(asset.content),
            }
        })
        .unwrap_or_else(|| HttpResponse {
            status_code: 404,
            headers: static_headers(),
            body: ByteBuf::from(format!("Asset {path} not found.")),
        })
}

/// Readable from any origin: Internet Identity's service worker reads the
/// well-known files from its own origin before it opens a notification's link.
fn static_headers() -> Vec<HeaderField> {
    vec![("Access-Control-Allow-Origin".to_string(), "*".to_string())]
}

/// Tells the page which canister it is served by, which its host does not
/// always say: a custom domain, or the local gateway's name-based URL.
fn name_canister(html: &str) -> String {
    html.replacen(
        "<head>",
        &format!(
            r#"<head><meta name="canister-id" content="{}">"#,
            canister_self().to_text()
        ),
        1,
    )
}

/// Everything in `dist/`, plus the sender list.
fn init_assets() {
    let mut assets = collect_assets(&ASSET_DIR, Some(name_canister));
    assets.push(Asset {
        url_path: NOTIFICATION_SENDERS_PATH.to_string(),
        content: serde_json::json!({ "senders": [canister_self().to_text()] })
            .to_string()
            .into_bytes(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::JSON,
    });

    ASSETS.with_borrow_mut(|certified_assets| {
        *certified_assets = CertifiedAssets::certify_assets(assets, &static_headers());
        certified_data_set(&certified_assets.root_hash()[..]);
    });
}

ic_cdk::export_candid!();

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_principal_is_shortened_to_its_ends() {
        let principal = Principal::from_text("rdmx6-jaaaa-aaaaa-aaadq-cai").unwrap();
        assert_eq!(short(principal), "rdmx6…cai");
        assert_eq!(short(Principal::anonymous()), "2vxsx…fae");
    }

    /// Writes `chat.did` beside the crate with `BLESS=1`, so the interface in
    /// the repo is the one the code exports rather than one somebody hand-edited.
    #[test]
    fn candid_is_up_to_date() {
        let exported = __export_service();
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("chat.did");

        if std::env::var("BLESS").is_ok() {
            std::fs::write(&path, &exported).expect("writing chat.did");
        }

        let committed = std::fs::read_to_string(&path).expect("chat.did should exist");
        assert_eq!(
            committed.trim(),
            exported.trim(),
            "chat.did is stale — rerun with BLESS=1"
        );
    }
}
