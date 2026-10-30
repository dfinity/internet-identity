//! One chatroom, to try Internet Identity notifications end to end.
//!
//! Every message notifies everyone else in the room, as a notification of its
//! own. Members can claim a name, unique among the names claimed. The canister
//! serves its own frontend, and lists itself as the one canister allowed to
//! notify for its origin.

use candid::{CandidType, Deserialize, Principal};
use ic_asset_certification::{Asset, AssetConfig, AssetEncoding, AssetFallbackConfig, AssetRouter};
use ic_cdk::api::{canister_self, certified_data_set, data_certificate, msg_caller, time, trap};
use ic_cdk::{init, post_upgrade, query, update};
use ic_http_certification::{HttpRequest, HttpResponse, StatusCode};
use ic_stable_structures::memory_manager::{MemoryId, MemoryManager, VirtualMemory};
use ic_stable_structures::{DefaultMemoryImpl, StableBTreeMap};
use identity_notifications as notifications;
use identity_notifications::{Memories, Metrics, Notification};
use include_dir::{include_dir, Dir};
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

/// What this app is made of, for an agent that wants to use it: one canister,
/// this one. Serving it opts the app into ICP MCP under the App Operator
/// Terms; see the README.
const IC_ARCHITECTURE_PATH: &str = "/.well-known/ic-architecture";

/// The guide an agent reads after the Candid interface: what the types do not
/// say. Kept beside the code so a change to one is a change to the other.
const API_DOC: &str = include_str!("api_doc.md");

static ASSET_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/dist");

thread_local! {
    static ROOM: RefCell<Room> = RefCell::new(Room::default());
    static ASSETS: RefCell<AssetRouter<'static>> = RefCell::new(AssetRouter::new());
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
                // No key: a key names what a notification is about, and a
                // second one with the same key replaces the first. Every
                // message is news of its own, so none of them replaces
                // another, and none can be dismissed by name either.
                key: None,
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

/// What the Candid interface does not say: units, who may call what, what
/// cannot be undone. For an agent, after the interface and before a call.
#[query]
fn get_api_doc() -> String {
    API_DOC.to_string()
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

/// Where a request for nothing the canister has ends up: a certified 404,
/// so a gateway passes it on as one. An uncertified answer it refuses as a
/// 503, which reads as the app being down to whoever asked for a file the
/// app does not have, such as `/.well-known/ii-derivation-origin`.
const NOT_FOUND_PATH: &str = "not-found.txt";

#[query]
fn http_request(request: HttpRequest) -> HttpResponse<'static> {
    ASSETS.with_borrow(|assets| {
        assets
            .serve_asset(
                &data_certificate().expect("a query has a data certificate"),
                &request,
            )
            .unwrap_or_else(|error| trap(format!("could not serve the asset: {error}")))
    })
}

/// Readable from any origin: Internet Identity's service worker reads the
/// well-known files from its own origin before it opens a notification's link.
fn static_headers() -> Vec<(String, String)> {
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

/// Every file under `dir`, with its path relative to the embedded root.
fn files_in(dir: &'static Dir<'static>) -> Vec<(String, Vec<u8>)> {
    let mut files: Vec<(String, Vec<u8>)> = dir
        .files()
        .map(|file| {
            (
                file.path().to_string_lossy().into_owned(),
                file.contents().to_vec(),
            )
        })
        .collect();
    for subdir in dir.dirs() {
        files.extend(files_in(subdir));
    }
    files
}

fn content_type_of(path: &str) -> &'static str {
    match path.rsplit('.').next() {
        Some("html") => "text/html",
        Some("js") => "text/javascript",
        Some("css") => "text/css",
        Some("json") => "application/json",
        Some("svg") => "image/svg+xml",
        Some("png") => "image/png",
        Some("ico") => "image/x-icon",
        Some("txt") => "text/plain",
        _ => "application/octet-stream",
    }
}

/// Everything in `dist/`, the two well-known files that name this canister
/// (the sender list for Internet Identity, the architecture manifest for an
/// agent) and the not-found answer. The files naming the canister are written
/// here rather than committed, since the id is only known once it exists.
fn init_assets() {
    let id = canister_self().to_text();
    let mut assets = Vec::new();
    let mut configs = Vec::new();

    for (path, content) in files_in(&ASSET_DIR) {
        let is_html = path.ends_with(".html");
        let content = if is_html {
            name_canister(std::str::from_utf8(&content).expect("HTML is UTF-8")).into_bytes()
        } else {
            content
        };
        // Vite names everything but the page by its hash, so it never changes
        // under a URL; the page does, and is what names the rest.
        let cache = if path.starts_with("assets/") {
            "public, max-age=31536000, immutable"
        } else {
            "public, no-cache, no-store"
        };
        let mut headers = static_headers();
        headers.push(("Cache-Control".to_string(), cache.to_string()));
        configs.push(AssetConfig::File {
            path: path.clone(),
            content_type: Some(content_type_of(&path).to_string()),
            headers,
            fallback_for: vec![],
            aliased_by: if path == "index.html" {
                vec!["/".to_string()]
            } else {
                vec![]
            },
            encodings: vec![AssetEncoding::Identity.default_config()],
        });
        assets.push(Asset::new(path, content));
    }

    for (path, json) in [
        (
            NOTIFICATION_SENDERS_PATH,
            serde_json::json!({ "senders": [id] }),
        ),
        (
            IC_ARCHITECTURE_PATH,
            serde_json::json!({
                "version": "1.0.0",
                "canisters": [{
                    "id": id,
                    "name": "notification_chat",
                    "role": "the app: its backend and its frontend",
                    "description": "one chat room whose messages notify through Internet Identity; call get_api_doc() first",
                }],
            }),
        ),
    ] {
        let path = path.trim_start_matches('/').to_string();
        let mut headers = static_headers();
        headers.push((
            "Cache-Control".to_string(),
            "public, no-cache, no-store".to_string(),
        ));
        configs.push(AssetConfig::File {
            path: path.clone(),
            content_type: Some("application/json".to_string()),
            headers,
            fallback_for: vec![],
            aliased_by: vec![],
            encodings: vec![AssetEncoding::Identity.default_config()],
        });
        assets.push(Asset::new(path, json.to_string().into_bytes()));
    }

    configs.push(AssetConfig::File {
        path: NOT_FOUND_PATH.to_string(),
        content_type: Some("text/plain".to_string()),
        headers: static_headers(),
        fallback_for: vec![AssetFallbackConfig {
            scope: "/".to_string(),
            status_code: Some(StatusCode::NOT_FOUND),
        }],
        aliased_by: vec![],
        encodings: vec![AssetEncoding::Identity.default_config()],
    });
    assets.push(Asset::new(NOT_FOUND_PATH, b"Not found.".to_vec()));

    ASSETS.with_borrow_mut(|router| {
        router.delete_all_assets();
        router
            .certify_assets(assets, configs)
            .unwrap_or_else(|error| trap(format!("could not certify the assets: {error}")));
        certified_data_set(router.root_hash());
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
