use crate::archive::{ArchiveData, ArchiveState};
use crate::openid::OpenIdCredential;
use crate::state::PersistentState;
use crate::stats::activity_stats::activity_counter::active_anchor_counter::ActiveAnchorCounter;
use crate::stats::activity_stats::{ActivityStats, CompletedActivityStats, OngoingActivityStats};
use crate::storage::account::AccountReference;
use crate::storage::account::{Account, AccountKey};
use crate::storage::anchor::{Anchor, Device};
use crate::storage::storable::account::StorableAccount;
use crate::storage::storable::anchor_application_config::AnchorApplicationConfig;
use crate::storage::{AccountReferenceListWrite, AccountReferenceWrite};
use crate::storage::{CreateSessionParams, Header, StorageError, MAX_ENTRIES};
use crate::Storage;
use candid::Principal;
use ic_stable_structures::{Memory, VectorMemory};
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, ApplicationNumber, BrowserBrand, BrowserDescription, FormFactor,
    FrontendHostname, OperatingSystem, PublicKey, Timestamp,
};
use internet_identity_interface::internet_identity::types::{
    ArchiveConfig, DeviceProtection, KeyType, Purpose,
};
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;
use std::collections::BTreeMap;
use std::collections::HashMap;

/// Records that an account was used, the way `prepare_account_delegation` does:
/// read it, stamp it, write it back. `None` where the identity has no such account.
fn record_use(
    storage: &mut Storage<VectorMemory>,
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    now: Timestamp,
) -> Result<Option<Account>, StorageError> {
    let Some(mut account) = storage.read_account(&AccountKey {
        anchor_number,
        origin,
        account_number,
    }) else {
        return Ok(None);
    };
    account.last_used = Some(now);
    storage.write_account(account).map(Some)
}

const HEADER_SIZE: usize = 58;

/// Removing what an identity holds at one origin, in the shape the gate takes it.
pub(crate) fn remove_at(
    origin: &FrontendHostname,
) -> BTreeMap<FrontendHostname, AccountReferenceListWrite> {
    BTreeMap::from([(origin.clone(), None)])
}

/// The account references a list holds, for assertions that go on to index them.
fn held_references(
    storage: &Storage<VectorMemory>,
    anchor_number: AnchorNumber,
    application_number: ApplicationNumber,
) -> Vec<AccountReference> {
    storage
        .stored_account_references(anchor_number, application_number)
        .expect("expected a list holding account references, found none")
}

/// One origin's worth of a write, in the shape the gate takes it.
/// The key a browser proves with at `generation`, and the successor it announces.
///
/// A browser is registered by the key it presents and reached only by the successor it
/// announced, so signing in twice from one browser means presenting `generation` and
/// then `generation + 1` — which is what a real browser does when it rotates.
const SESSION_TEST_ORIGIN: &str = "https://example.com";

pub(crate) fn browser_key(seed: u8, generation: u16) -> PublicKey {
    let mut key = vec![0u8; 32];
    key[0] = seed;
    key[1..3].copy_from_slice(&generation.to_be_bytes());
    ByteBuf::from(key)
}

/// What the browser `seed` names reports about itself. The seed rides in `model`, the
/// description's one free-text field, so entries stay tellable apart.
pub(crate) fn description(seed: u8) -> BrowserDescription {
    BrowserDescription {
        brand: BrowserBrand::Chrome,
        os: OperatingSystem::Macos,
        form_factor: FormFactor::Desktop,
        model: Some(format!("browser {seed}")),
    }
}

/// A first sign-in from the browser `seed` names.
pub(crate) fn params(anchor_number: AnchorNumber, seed: u8, now: u64) -> CreateSessionParams {
    params_at(anchor_number, seed, 0, now)
}

/// A sign-in from the browser `seed` names, presenting the key it holds after
/// `generation` rotations.
pub(crate) fn params_at(
    anchor_number: AnchorNumber,
    seed: u8,
    generation: u16,
    now: u64,
) -> CreateSessionParams {
    CreateSessionParams {
        anchor_number,
        origin: SESSION_TEST_ORIGIN.to_string(),
        account_number: None,
        current_browser_key: browser_key(seed, generation),
        next_browser_key: browser_key(seed, generation + 1),
        browser_description: description(seed),
        valid_till_ns: now + 10_000,
        max_idle_ns: None,
        read_only: false,
        now_ns: now,
    }
}

pub(crate) fn write_at(
    origin: &FrontendHostname,
    account_references: Vec<AccountReference>,
    config: Option<AnchorApplicationConfig>,
) -> BTreeMap<FrontendHostname, AccountReferenceListWrite> {
    BTreeMap::from([(
        origin.clone(),
        Some((
            account_references
                .into_iter()
                .map(AccountReferenceWrite::from)
                .collect(),
            config,
        )),
    )])
}

/// The same, with a record attached to the account reference carrying `account_number` —
/// or, where that is `None`, to the tracked default, which is what names it.
pub(crate) fn write_at_with_record(
    origin: &FrontendHostname,
    account_references: Vec<AccountReference>,
    account_number: Option<AccountNumber>,
    record: StorableAccount,
    config: Option<AnchorApplicationConfig>,
) -> BTreeMap<FrontendHostname, AccountReferenceListWrite> {
    let mut writes = write_at(origin, account_references, config);
    let (account_references, _) = writes
        .get_mut(origin)
        .and_then(Option::as_mut)
        .expect("just built");
    let write = account_references
        .iter_mut()
        .find(|write| write.account_reference.account_number == account_number)
        .expect("the account reference the record belongs to is in the list");
    write.record = Some(record);
    writes
}

/// Creates the application for `origin` the way production does — by writing account
/// state at it — and hands back its number.
///
/// There is no allocator to call on its own any more: an application exists because
/// something holds it, so a test that wants one writes something. The config write is
/// the smallest thing that stores anything at an origin without giving the identity
/// an account there.
pub(crate) fn application_number_for(
    storage: &mut Storage<VectorMemory>,
    origin: &FrontendHostname,
) -> ApplicationNumber {
    // Storing an account reference list derives principals for it, which needs a salt.
    // Tests that never set one still want an application to exist.
    if storage.salt().is_none() {
        storage.update_salt([17u8; 32]);
    }
    // Under some other identity, so the one under test still holds nothing here. That is
    // a real state — an application exists because *someone* holds something at it — and
    // it is the only way to arrange it now that an application is never stored empty.
    let other = storage
        .allocate_anchor(0)
        .expect("an anchor to hold the application with");
    let other_identity = other.anchor_number();
    storage.write(other).expect("writing the other identity");
    let (account_references, _) = storage.account_state_for_origin(other_identity, origin);
    storage
        .write_account_state_for_testing(
            other_identity,
            BTreeMap::from([(
                origin.clone(),
                Some((account_references, Some(AnchorApplicationConfig::default()))),
            )]),
        )
        .expect("writing a config at an origin creates its application");
    storage
        .lookup_application_number_with_origin(origin)
        .expect("the write above created the application")
}

#[test]
fn should_match_actual_header_size() {
    // if this test fails, make sure the change was intentional and upgrade as well as rollback still work!
    assert_eq!(std::mem::size_of::<Header>(), HEADER_SIZE);
}

#[test]
fn should_report_max_number_of_entries_for_256gb() {
    // The maximum number of entries that could be supported by the canister without making any changes
    // is constant. This test now exists to make sure any dev is aware of the limit if making changes
    // to the underlying constants.
    assert_eq!(MAX_ENTRIES, 67_106_816);
}

#[test]
fn should_serialize_header_v9() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((1, 2), memory.clone());
    storage.update_salt([5u8; 32]);
    storage.flush();

    assert_eq!(storage.version(), 9);
    let mut buf = vec![0; HEADER_SIZE];
    memory.read(0, &mut buf);
    assert_eq!(buf, hex::decode("49494309000000000100000000000000020000000000000000100505050505050505050505050505050505050505050505050505050505050505").unwrap());
}

#[test]
fn should_recover_header_from_memory_v9() {
    let memory = VectorMemory::default();
    memory.grow(1);
    memory.write(0, &hex::decode("494943090500000040e2010000000000f1fb090000000000000843434343434343434343434343434343434343434343434343434343434343430002000000000000000000000000000000000000000000000000").unwrap());

    let storage = Storage::from_memory(memory);
    assert_eq!(storage.assigned_anchor_number_range(), (123456, 654321));
    assert_eq!(storage.salt().unwrap(), &[67u8; 32]);
    assert_eq!(storage.anchor_count(), 5);
    assert_eq!(storage.version(), 9);
}

#[test]
fn should_read_previous_write() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((12345, 678910), memory);
    let mut anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();

    anchor.add_device(sample_device()).unwrap();
    storage.write(anchor.clone()).unwrap();

    let read_anchor = storage.read(anchor_number).unwrap();
    assert_eq!(anchor, read_anchor);
}

#[test]
fn should_not_write_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory);
    storage.allocate_anchor(0).unwrap();

    let anchor = Anchor::new(222, 333);

    let result = storage.write(anchor);
    assert!(
        matches!(result, Err(StorageError::BadAnchorNumber(_))),
        "result = {:?}",
        result
    )
}

#[test]
fn should_not_read_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory);
    storage.allocate_anchor(0).unwrap();

    let result = storage.read(222);
    assert!(matches!(result, Err(StorageError::BadAnchorNumber(_))))
}

#[test]
fn should_save_and_restore_persistent_state() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory);
    storage.flush();
    storage.allocate_anchor(0).unwrap();

    let persistent_state = sample_persistent_state();

    storage.write_persistent_state(&persistent_state);
    assert_eq!(storage.read_persistent_state(), persistent_state);
}

#[test]
fn should_read_default_persistent_state_from_new_storage() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);
    storage.flush();

    assert_eq!(storage.read_persistent_state(), PersistentState::default());
}

#[test]
fn should_not_overwrite_persistent_state_with_next_anchor_v9() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    storage.allocate_anchor(0).unwrap();
    storage.write_persistent_state(&sample_persistent_state());
    assert_eq!(storage.read_persistent_state(), sample_persistent_state());

    let anchor = storage.allocate_anchor(0).unwrap();
    storage.write(anchor).unwrap();

    assert_eq!(storage.read_persistent_state(), sample_persistent_state());
}

#[test]
fn should_persist_openid_jwks_across_reload() {
    use identity_jose::jwk::Jwk;

    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    let issuer = "https://accounts.google.com";
    let jwk: Jwk = serde_json::from_str(
        r#"{"kty":"RSA","use":"sig","alg":"RS256","kid":"kid-1","n":"modulus","e":"AQAB"}"#,
    )
    .unwrap();

    assert!(storage.read_openid_jwks(issuer).is_none());
    storage.write_openid_jwks(issuer, vec![jwk]);

    // Re-read from the same backing memory to simulate a canister upgrade.
    let reloaded = Storage::from_memory(memory);
    let restored = reloaded
        .read_openid_jwks(issuer)
        .expect("JWKs should persist across reload");
    assert_eq!(restored.len(), 1);
    assert_eq!(restored[0].kid(), Some("kid-1"));

    // Unknown issuers have no cached keys.
    assert!(reloaded.read_openid_jwks("https://other.example").is_none());
}

#[test]
fn should_write_and_update_openid_credential_lookup() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let mut anchor = storage.allocate_anchor(0).unwrap();
    let openid_credential_0 = openid_credential(0);
    let openid_credential_1 = openid_credential(1);
    let openid_credential_2 = openid_credential(2);
    anchor
        .add_openid_credential(openid_credential_0.clone())
        .unwrap();
    anchor
        .add_openid_credential(openid_credential_1.clone())
        .unwrap();

    // Check if both anchor and OpenID credential lookups are written to storage
    storage.write(anchor.clone()).unwrap();
    assert_eq!(storage.read(anchor.anchor_number()).unwrap(), anchor);
    assert_eq!(
        storage
            .lookup_anchor_with_openid_credential(&openid_credential_0.key(), None)
            .unwrap(),
        anchor.anchor_number()
    );
    assert_eq!(
        storage
            .lookup_anchor_with_openid_credential(&openid_credential_1.key(), None)
            .unwrap(),
        anchor.anchor_number()
    );

    // Check if OpenID credential lookup is cleaned up from storage when anchor is written
    anchor
        .remove_openid_credential(&openid_credential_0.key())
        .unwrap();
    storage.write(anchor.clone()).unwrap();
    assert_eq!(
        storage.lookup_anchor_with_openid_credential(&openid_credential_0.key(), None),
        None
    );
    assert_eq!(
        storage
            .lookup_anchor_with_openid_credential(&openid_credential_1.key(), None)
            .unwrap(),
        anchor.anchor_number()
    );

    // Check if OpenID credential lookup is written to storage when anchor is written
    anchor
        .add_openid_credential(openid_credential_2.clone())
        .unwrap();
    storage.write(anchor.clone()).unwrap();
    assert_eq!(
        storage.lookup_anchor_with_openid_credential(&openid_credential_0.key(), None),
        None
    );
    assert_eq!(
        storage
            .lookup_anchor_with_openid_credential(&openid_credential_1.key(), None)
            .unwrap(),
        anchor.anchor_number()
    );
    assert_eq!(
        storage
            .lookup_anchor_with_openid_credential(&openid_credential_2.key(), None)
            .unwrap(),
        anchor.anchor_number()
    );
}

/// The SSO stable-id index is reconciled from the anchors' stored credentials
/// on every `write()`: a credential carrying a `stable_id` gets an entry, and
/// removing or moving that credential removes or moves the entry — no orphans.
/// Mirrors `should_write_and_update_openid_credential_lookup`.
#[test]
fn should_write_and_update_sso_stable_id_index() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let sso_domain = "acme.example";
    let iss = "https://example.com";
    let primary_client = "example-aud";
    let stable_id = "oid-stable-42";

    // A credential that carries a `stable_id` (a non-`sub` primary credential).
    // An SSO credential always carries its discovery `sso_domain` too; the index
    // key is scoped by it.
    let mut bridged = openid_credential(0);
    bridged.stable_id = Some(stable_id.to_string());
    bridged.sso_domain = Some(sso_domain.to_string());
    // A second credential on the same anchor without a `stable_id` — it must
    // never appear in the SSO stable-id index.
    let plain = openid_credential(1);

    let mut anchor_a = storage.allocate_anchor(0).unwrap();
    anchor_a.add_openid_credential(bridged.clone()).unwrap();
    anchor_a.add_openid_credential(plain.clone()).unwrap();
    storage.write(anchor_a.clone()).unwrap();

    // The bridged credential is indexed; the plain one is not.
    assert_eq!(
        storage.lookup_anchor_by_sso_stable_id(sso_domain, iss, primary_client, stable_id),
        Some(anchor_a.anchor_number())
    );
    assert_eq!(
        storage.lookup_anchor_by_sso_stable_id(sso_domain, iss, primary_client, "no-such-oid"),
        None
    );
    // The same (iss, primary_client, stable_id) discovered through a different
    // domain does not resolve to this entry — the domain is part of the key.
    assert_eq!(
        storage.lookup_anchor_by_sso_stable_id("attacker.example", iss, primary_client, stable_id),
        None
    );

    // Remove the bridged credential -> its index entry self-cleans on write.
    anchor_a.remove_openid_credential(&bridged.key()).unwrap();
    storage.write(anchor_a.clone()).unwrap();
    assert_eq!(
        storage.lookup_anchor_by_sso_stable_id(sso_domain, iss, primary_client, stable_id),
        None
    );

    // Move the bridged credential to a second anchor -> the entry follows it.
    let mut anchor_b = storage.allocate_anchor(0).unwrap();
    anchor_b.add_openid_credential(bridged.clone()).unwrap();
    storage.write(anchor_b.clone()).unwrap();
    assert_eq!(
        storage.lookup_anchor_by_sso_stable_id(sso_domain, iss, primary_client, stable_id),
        Some(anchor_b.anchor_number())
    );
}

#[test]
fn should_write_and_update_device_credential_lookup() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let mut anchor = storage.allocate_anchor(0).unwrap();
    let device_0 = Device {
        pubkey: ByteBuf::from(vec![0]),
        credential_id: Some(ByteBuf::from(vec![0])),
        ..sample_device()
    };
    let device_1 = Device {
        pubkey: ByteBuf::from(vec![1]),
        credential_id: Some(ByteBuf::from(vec![1])),
        ..sample_device()
    };
    let device_2 = Device {
        pubkey: ByteBuf::from(vec![2]),
        credential_id: Some(ByteBuf::from(vec![2])),
        ..sample_device()
    };
    anchor.add_device(device_0.clone()).unwrap();
    anchor.add_device(device_1.clone()).unwrap();

    // Check if both anchor and device credential lookups are written to storage
    storage.write(anchor.clone()).unwrap();
    assert_eq!(storage.read(anchor.anchor_number()).unwrap(), anchor);
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_0.credential_id.clone().unwrap())
            .unwrap(),
        anchor.anchor_number()
    );
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_1.credential_id.clone().unwrap())
            .unwrap(),
        anchor.anchor_number()
    );

    // Check if device credential lookup is cleaned up from storage when anchor is written
    anchor.remove_device(&device_0.pubkey).unwrap();
    storage.write(anchor.clone()).unwrap();
    assert_eq!(
        storage.lookup_anchor_with_device_credential(&device_0.credential_id.clone().unwrap()),
        None
    );
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_1.credential_id.clone().unwrap())
            .unwrap(),
        anchor.anchor_number()
    );

    // Check if device credential lookup is written to storage when anchor is written
    anchor.add_device(device_2.clone()).unwrap();
    storage.write(anchor.clone()).unwrap();
    assert_eq!(
        storage.lookup_anchor_with_device_credential(&device_0.credential_id.clone().unwrap()),
        None
    );
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_1.credential_id.clone().unwrap())
            .unwrap(),
        anchor.anchor_number()
    );
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_2.credential_id.clone().unwrap())
            .unwrap(),
        anchor.anchor_number()
    );
}

#[test]
fn should_not_overwrite_device_credential_lookup() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let mut anchor_0 = storage.allocate_anchor(0).unwrap();
    let mut anchor_1 = storage.allocate_anchor(0).unwrap();
    let device_0 = Device {
        pubkey: ByteBuf::from(vec![0]),
        credential_id: Some(ByteBuf::from(vec![0])),
        ..sample_device()
    };
    let device_1 = Device {
        pubkey: ByteBuf::from(vec![1]),
        credential_id: device_0.credential_id.clone(),
        ..sample_device()
    };
    anchor_0.add_device(device_0.clone()).unwrap();
    anchor_1.add_device(device_1.clone()).unwrap();

    // Make sure that lookup of anchor_0 is not overwritten with anchor_1
    storage.write(anchor_0.clone()).unwrap();
    storage.write(anchor_1.clone()).unwrap();
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_0.credential_id.clone().unwrap())
            .unwrap(),
        anchor_0.anchor_number()
    );
    // Make sure that lookup of anchor_0 is not remove by anchor_1
    anchor_1.remove_device(&device_1.pubkey).unwrap();
    storage.write(anchor_1.clone()).unwrap();
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_0.credential_id.clone().unwrap())
            .unwrap(),
        anchor_0.anchor_number()
    );
}

#[test]
fn should_record_that_a_named_account_was_used() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);
    storage.update_salt([17u8; 32]);
    let origin = "https://example.com".to_string();

    let anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();
    storage.write(anchor).unwrap();

    let account = storage
        .create_account(anchor_number, origin.clone(), "Test Account".to_string())
        .unwrap();
    let key = AccountKey {
        anchor_number,
        origin: origin.clone(),
        account_number: account.account_number,
    };

    assert_eq!(storage.read_account(&key).unwrap().last_used, None);

    for timestamp in [123456789u64, 987654321u64] {
        let mut account = storage.read_account(&key).unwrap();
        account.last_used = Some(timestamp);
        storage.write_account(account).unwrap();

        assert_eq!(
            storage.read_account(&key).unwrap().last_used,
            Some(timestamp)
        );
    }
}

#[test]
fn should_track_the_default_account_on_first_use() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);
    storage.update_salt([17u8; 32]);
    let origin = "https://example.com".to_string();

    let anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();
    storage.write(anchor).unwrap();

    let key = AccountKey {
        anchor_number,
        origin: origin.clone(),
        account_number: None,
    };
    let timestamp = 555_555u64;

    let mut account = storage.read_account(&key).unwrap();
    account.last_used = Some(timestamp);
    storage.write_account(account).unwrap();

    // Nothing was stored at this origin, so recording the use is what gives the
    // default a list: the timestamp has nowhere else to live.
    assert_eq!(
        storage.read_account(&key).unwrap().last_used,
        Some(timestamp)
    );
    let application_number = storage
        .lookup_application_number_with_origin(&origin)
        .unwrap();
    assert_eq!(
        storage.stored_account_references(anchor_number, application_number),
        Some(vec![AccountReference::new(None, Some(timestamp))])
    );
}

#[test]
fn should_record_that_a_tracked_default_was_used() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);
    storage.update_salt([17u8; 32]);
    let origin = "https://example.com".to_string();

    let anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();
    storage.write(anchor).unwrap();

    // A named account gives the origin a list, which the default is tracked in.
    storage
        .create_account(anchor_number, origin.clone(), "Test Account".to_string())
        .unwrap();

    let key = AccountKey {
        anchor_number,
        origin: origin.clone(),
        account_number: None,
    };
    let timestamp = 555_555u64;

    let mut account = storage.read_account(&key).unwrap();
    account.last_used = Some(timestamp);
    storage.write_account(account).unwrap();

    assert_eq!(
        storage.read_account(&key).unwrap().last_used,
        Some(timestamp)
    );
}

#[test]
fn should_refuse_to_write_an_account_no_reference_names() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);
    // Creating an account derives its principal, which needs the salt.
    storage.update_salt([17u8; 32]);
    let origin = "https://example.com".to_string();

    let anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();
    storage.write(anchor).unwrap();

    storage
        .create_account(anchor_number, origin.clone(), "Test Account".to_string())
        .unwrap();

    let unheld_account_number = 99_999u64;
    let key = AccountKey {
        anchor_number,
        origin: origin.clone(),
        account_number: Some(unheld_account_number),
    };

    // Holding a reference is what grants access, so an account this identity does not
    // hold cannot be read, and writing one it names anyway is refused rather than
    // adopted.
    assert!(storage.read_account(&key).is_none());
    assert!(matches!(
        storage.write_account(Account::new_with_last_used(
            anchor_number,
            origin,
            None,
            Some(unheld_account_number),
            Some(123_456u64),
        )),
        Err(StorageError::AccountNotFound { account_number })
            if account_number == unheld_account_number
    ));
}

#[test]
fn should_refuse_to_write_an_account_at_an_unknown_origin() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();
    storage.write(anchor).unwrap();

    let unknown_origin = "https://nonexistent.com".to_string();

    assert!(matches!(
        storage.write_account(Account::new_with_last_used(
            anchor_number,
            unknown_origin,
            None,
            Some(99_999u64),
            Some(123_456u64),
        )),
        Err(StorageError::AccountNotFound { .. })
    ));
}

fn sample_device() -> Device {
    Device {
        pubkey: ByteBuf::from("hello world, I am a public key"),
        alias: "my test device".to_string(),
        credential_id: Some(ByteBuf::from("this is the credential id")),
        aaguid: None,
        purpose: Purpose::Authentication,
        key_type: KeyType::CrossPlatform,
        protection: DeviceProtection::Unprotected,
        origin: Some("https://id.ai".to_string()),
        last_usage_timestamp: Some(1234),
        metadata: None,
    }
}

fn openid_credential(n: u8) -> OpenIdCredential {
    OpenIdCredential {
        iss: "https://example.com".into(),
        sub: n.to_string(),
        aud: "example-aud".into(),
        last_usage_timestamp: Some(n.into()),
        metadata: HashMap::default(),
        sso_domain: None,
        sso_name: None,
        stable_id: None,
    }
}
fn sample_persistent_state() -> PersistentState {
    PersistentState {
        archive_state: ArchiveState::Created {
            data: ArchiveData {
                sequence_number: 39,
                archive_canister: Principal::from_text("2h5ob-7aaaa-aaaad-aacya-cai").unwrap(),
            },
            config: ArchiveConfig {
                module_hash: [99u8; 32],
                entries_buffer_limit: 10_000,
                polling_interval_ns: 60_000_000_000,
                entries_fetch_limit: 1_000,
            },
        },
        canister_creation_cycles_cost: 12_346_000_000,
        active_anchor_stats: ActivityStats {
            completed: CompletedActivityStats {
                daily_events: Some(ActiveAnchorCounter {
                    start_timestamp: 965485,
                    counter: 99,
                }),
                monthly_events: None,
            },
            ongoing: OngoingActivityStats {
                daily_events: ActiveAnchorCounter {
                    start_timestamp: 5648954321,
                    counter: 44,
                },
                monthly_events: vec![ActiveAnchorCounter {
                    start_timestamp: 549843248,
                    counter: 66,
                }],
            },
        },
        ..PersistentState::default()
    }
}

#[cfg(test)]
mod application_lookup_tests {
    use super::application_number_for;

    use super::*;
    use crate::storage::storable::application::StorableOriginSha256;
    use ic_stable_structures::VectorMemory;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn assert_application_lookup<M: Memory + Clone>(
        storage: &Storage<M>,
        origin: &str,
        expected_new: Option<u64>,
    ) {
        let origin = origin.to_string();
        let origin_sha256 = StorableOriginSha256::from_origin(&origin);

        assert_eq!(
            storage.lookup_application_number_with_origin(&origin),
            expected_new,
            "Unexpected result from lookup_application_number_with_origin for origin {}",
            origin
        );
        assert_eq!(
            storage
                .lookup_application_with_origin_memory
                .get(&origin_sha256),
            expected_new,
            "Unexpected lookup_application_with_origin_memory entry for key {}",
            origin_sha256
        );
    }

    #[test]
    fn should_create_new_application_with_sha256_lookup() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());
        let origin = "https://example.com".to_string();

        let app_number = application_number_for(&mut storage, &origin);

        // Should create application number 0 for first application
        assert_eq!(app_number, 0);

        // Should exist in both old and new lookup maps
        assert_application_lookup(&storage, &origin, Some(0));

        // Should exist in applications storage
        let stored_app = storage.stable_application_memory.get(&0);
        assert!(stored_app.is_some());
        let app = stored_app.unwrap();
        assert_eq!(app.origin, origin);
        // No named accounts, and the one account reference is the tracked default the
        // identity that created it holds: an application is never stored holding nothing.
        assert_eq!(app.stored_accounts, 0);
        assert_eq!(app.stored_account_references, 1);
    }

    #[test]
    fn should_return_existing_application_number() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());
        let origin = "https://example.com".to_string();

        // Create application first time
        let app_number1 = application_number_for(&mut storage, &origin);

        // Should return same application number on second call
        let app_number2 = application_number_for(&mut storage, &origin);

        assert_eq!(app_number1, app_number2);
        assert_eq!(app_number1, 0);
    }

    #[test]
    fn should_create_different_numbers_for_different_origins() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());

        let origin1 = "https://example.com".to_string();
        let origin2 = "https://different.com".to_string();
        let origin3 = "https://another.org".to_string();

        let app_num1 = application_number_for(&mut storage, &origin1);
        let app_num2 = application_number_for(&mut storage, &origin2);
        let app_num3 = application_number_for(&mut storage, &origin3);

        assert_eq!(app_num1, 0);
        assert_eq!(app_num2, 1);
        assert_eq!(app_num3, 2);

        // All should be present in both lookup maps
        assert_application_lookup(&storage, &origin1, Some(0));
        assert_application_lookup(&storage, &origin2, Some(1));
        assert_application_lookup(&storage, &origin3, Some(2));
    }

    #[test]
    fn new_storage_should_have_empty_maps() {
        let storage = Storage::new((10, 20), VectorMemory::default());
        assert_eq!(storage.lookup_application_with_origin_memory.len(), 0);
    }

    #[test]
    fn should_handle_very_long_origins_with_sha256() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());

        let long_origin = format!("https://{}.com", "a".repeat(20_000));

        let app_number = application_number_for(&mut storage, &long_origin);
        assert_eq!(app_number, 0);

        // Should be findable in both maps
        assert_application_lookup(&storage, &long_origin, Some(0));

        // Application should be stored with full origin
        let stored_app = storage.stable_application_memory.get(&0).unwrap();
        assert_eq!(stored_app.origin, long_origin);
    }

    #[test]
    fn should_increment_application_counter_correctly() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());

        let origins = [
            "https://app1.com".to_string(),
            "https://app2.com".to_string(),
            "https://app3.com".to_string(),
        ];

        for (i, origin) in origins.iter().enumerate() {
            let app_number = application_number_for(&mut storage, origin);
            assert_eq!(app_number, i as u64);

            // Total application count should increment
            assert_eq!(storage.get_total_application_count(), (i + 1) as u64);
        }
    }

    #[test]
    fn should_preserve_existing_data_on_storage_restart() {
        let memory = VectorMemory::default();
        let origin = "https://persistent-test.com".to_string();

        // Create storage and add application
        {
            let mut storage = Storage::new((10, 20), memory.clone());
            let app_number = application_number_for(&mut storage, &origin);
            assert_eq!(app_number, 0);
        }

        // Recreate storage from same memory
        let storage = Storage::from_memory(memory);

        // Should find existing application in old lookup
        assert_eq!(
            storage.lookup_application_number_with_origin(&origin),
            Some(0)
        );

        // Should find it in both maps
        assert_application_lookup(&storage, &origin, Some(0));

        // Application should still exist in storage
        let stored_app = storage.stable_application_memory.get(&0);
        assert!(stored_app.is_some());
    }
}

#[cfg(test)]
mod storable_origin_sha256_tests {
    use crate::storage::storable::application::StorableOriginSha256;
    use ic_stable_structures::Storable;
    use pretty_assertions::assert_eq;
    use std::borrow::Cow;

    #[test]
    fn should_create_different_hashes_for_different_origins() {
        let origin1 = "https://example.com".to_string();
        let origin2 = "https://different.com".to_string();

        let hash1 = StorableOriginSha256::from_origin(&origin1);
        let hash2 = StorableOriginSha256::from_origin(&origin2);

        assert_ne!(hash1, hash2);
    }

    #[test]
    fn should_create_same_hash_for_same_origin() {
        let origin = "https://example.com".to_string();

        let hash1 = StorableOriginSha256::from_origin(&origin);
        let hash2 = StorableOriginSha256::from_origin(&origin);

        assert_eq!(hash1, hash2);
    }

    #[test]
    fn should_be_storable_and_retrievable() {
        let origin = "https://storable-test.com".to_string();
        let original_hash = StorableOriginSha256::from_origin(&origin);

        // Test round-trip serialization
        let bytes = original_hash.to_bytes();
        let recovered_hash = StorableOriginSha256::from_bytes(bytes);

        assert_eq!(original_hash, recovered_hash);
    }

    #[test]
    fn should_handle_short_byte_arrays() {
        let short_bytes = [1, 2, 3, 4, 5];
        let hash = StorableOriginSha256::from_bytes(Cow::Borrowed(&short_bytes));

        // Should be padded with zeros
        let bytes = hash.to_bytes();
        assert_eq!(bytes.len(), 32);
        assert_eq!(&bytes[..5], &short_bytes[..]);
        assert_eq!(&bytes[5..], &[0u8; 27]);
    }

    #[test]
    fn should_handle_oversized_byte_arrays() {
        let oversized_bytes = [42u8; 40];
        let hash = StorableOriginSha256::from_bytes(Cow::Borrowed(&oversized_bytes));

        // Should be truncated to 32 bytes
        let bytes = hash.to_bytes();
        assert_eq!(bytes.len(), 32);
        assert_eq!(&bytes[..], &[42u8; 32]);
    }

    #[test]
    fn should_handle_empty_byte_array() {
        let empty_bytes: Vec<u8> = vec![];
        let hash = StorableOriginSha256::from_bytes(Cow::Borrowed(&empty_bytes));

        // Should be all zeros
        let bytes = hash.to_bytes();
        assert_eq!(bytes.len(), 32);
        assert_eq!(&bytes[..], &[0u8; 32]);
    }

    #[test]
    fn should_respect_storable_bound() {
        assert_eq!(StorableOriginSha256::BOUND.max_size(), 32);
        assert!(StorableOriginSha256::BOUND.is_fixed_size());
    }
}

#[cfg(test)]
mod sync_anchor_with_recovery_phrase_principal_index_tests {
    use super::*;
    use crate::storage::anchor::Device;
    use crate::storage::storable::recovery_key::StorableRecoveryKey;
    use candid::Principal;
    use internet_identity_interface::internet_identity::types::{KeyType, PublicKey};
    use pretty_assertions::assert_eq;

    fn pubkey(n: u8) -> PublicKey {
        vec![n].into()
    }

    fn seed_phrase_device(pubkey: PublicKey) -> Device {
        Device {
            pubkey,
            alias: "seed".to_string(),
            credential_id: None,
            aaguid: None,
            purpose: Purpose::Recovery,
            key_type: KeyType::SeedPhrase,
            protection: DeviceProtection::Unprotected,
            origin: None,
            metadata: None,
            last_usage_timestamp: None,
        }
    }

    fn other_device(pubkey: PublicKey) -> Device {
        Device {
            pubkey,
            alias: "other".to_string(),
            credential_id: Some(ByteBuf::from(vec![1, 2, 3])),
            aaguid: None,
            purpose: Purpose::Authentication,
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: Some("https://id.ai".to_string()),
            last_usage_timestamp: None,
            metadata: None,
        }
    }

    fn device_to_recovery_key(device: &Device) -> StorableRecoveryKey {
        StorableRecoveryKey {
            pubkey: device.pubkey.clone().into_vec(),
            created_at_ns: None,
            last_usage_timestamp_ns: device.last_usage_timestamp,
            is_protected: Some(device.protection == DeviceProtection::Protected),
            special_device_migration: None,
        }
    }

    fn pre_populate_index<M: Memory + Clone>(
        storage: &mut Storage<M>,
        anchor_number: u64,
        recovery_keys: &[StorableRecoveryKey],
    ) {
        for recovery_key in recovery_keys {
            let principal = Principal::self_authenticating(&recovery_key.pubkey);
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .insert(principal, anchor_number);
        }
    }

    #[test]
    fn adds_new_seed_phrase_principals() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number = 1;
        let prev = vec![];
        let curr_devices = [seed_phrase_device(pubkey(42)), other_device(pubkey(99))];
        // Only convert seed phrase devices to recovery keys
        let curr: Vec<StorableRecoveryKey> = curr_devices
            .iter()
            .filter(|d| d.key_type == KeyType::SeedPhrase)
            .map(device_to_recovery_key)
            .collect();

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number, &prev, &curr);

        let principal = Principal::self_authenticating(pubkey(42));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal),
            Some(anchor_number)
        );
        // Should not add non-seed phrase device
        let principal_other = Principal::self_authenticating(pubkey(99));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_other),
            None
        );

        // Bonus: What if the same recovery phrase is used again by another user?
        let another_anchor_number = 2;
        let prev = vec![];
        let curr_devices = [seed_phrase_device(pubkey(42))];
        let curr: Vec<StorableRecoveryKey> = curr_devices
            .iter()
            .filter(|d| d.key_type == KeyType::SeedPhrase)
            .map(device_to_recovery_key)
            .collect();

        storage.sync_anchor_with_recovery_phrase_principal_index(
            another_anchor_number,
            &prev,
            &curr,
        );

        // Index should not change for this principal.
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal),
            Some(anchor_number)
        );
    }

    #[test]
    fn removes_old_seed_phrase_principals() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number = 2;
        let prev_devices = [seed_phrase_device(pubkey(1)), seed_phrase_device(pubkey(2))];
        let curr_devices = [seed_phrase_device(pubkey(2))];
        let prev: Vec<StorableRecoveryKey> =
            prev_devices.iter().map(device_to_recovery_key).collect();
        let curr: Vec<StorableRecoveryKey> =
            curr_devices.iter().map(device_to_recovery_key).collect();

        pre_populate_index(&mut storage, anchor_number, &prev);

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number, &prev, &curr);

        let principal_removed = Principal::self_authenticating(pubkey(1));
        let principal_kept = Principal::self_authenticating(pubkey(2));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_removed),
            None
        );
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_kept),
            Some(anchor_number)
        );
    }

    #[test]
    fn no_change_if_same_devices() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number = 3;
        let prev_devices = [seed_phrase_device(pubkey(7))];
        let curr_devices = [seed_phrase_device(pubkey(7))];
        let prev: Vec<StorableRecoveryKey> =
            prev_devices.iter().map(device_to_recovery_key).collect();
        let curr: Vec<StorableRecoveryKey> =
            curr_devices.iter().map(device_to_recovery_key).collect();

        pre_populate_index(&mut storage, anchor_number, &prev);

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number, &prev, &curr);

        let principal = Principal::self_authenticating(pubkey(7));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal),
            Some(anchor_number)
        );
    }

    #[test]
    fn handles_empty_current_and_previous() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number = 4;
        let prev = vec![];
        let curr = vec![];

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number, &prev, &curr);

        // Should remain empty
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .iter()
                .count(),
            0
        );
    }

    #[test]
    fn adds_and_removes_seed_phrase_principals_in_single_call() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number = 5;
        let prev_devices = [
            seed_phrase_device(pubkey(1)),
            seed_phrase_device(pubkey(2)),
            seed_phrase_device(pubkey(3)),
        ];
        let curr_devices = [
            seed_phrase_device(pubkey(2)),
            seed_phrase_device(pubkey(4)),
            seed_phrase_device(pubkey(5)),
        ];
        let prev: Vec<StorableRecoveryKey> =
            prev_devices.iter().map(device_to_recovery_key).collect();
        let curr: Vec<StorableRecoveryKey> =
            curr_devices.iter().map(device_to_recovery_key).collect();

        pre_populate_index(&mut storage, anchor_number, &prev);

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number, &prev, &curr);

        // Devices 1 and 3 should be removed
        let principal_1 = Principal::self_authenticating(pubkey(1));
        let principal_3 = Principal::self_authenticating(pubkey(3));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_1),
            None
        );
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_3),
            None
        );

        // Device 2 should remain
        let principal_2 = Principal::self_authenticating(pubkey(2));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_2),
            Some(anchor_number)
        );

        // Devices 4 and 5 should be added
        let principal_4 = Principal::self_authenticating(pubkey(4));
        let principal_5 = Principal::self_authenticating(pubkey(5));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_4),
            Some(anchor_number)
        );
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_5),
            Some(anchor_number)
        );
    }

    #[test]
    fn removes_seed_phrase_principals_only_for_specified_anchor() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number_1 = 10;
        let anchor_number_2 = 20;

        // Device present for both anchors
        let device_shared = seed_phrase_device(pubkey(42));
        let device_unique_1 = seed_phrase_device(pubkey(1));
        let device_unique_2 = seed_phrase_device(pubkey(2));

        // Pre-populate index for both anchors
        let recovery_keys_1: Vec<StorableRecoveryKey> =
            [device_shared.clone(), device_unique_1.clone()]
                .iter()
                .map(device_to_recovery_key)
                .collect();
        let recovery_keys_2: Vec<StorableRecoveryKey> =
            [device_shared.clone(), device_unique_2.clone()]
                .iter()
                .map(device_to_recovery_key)
                .collect();

        pre_populate_index(&mut storage, anchor_number_1, &recovery_keys_1);
        pre_populate_index(&mut storage, anchor_number_2, &recovery_keys_2);

        // Remove device_shared and device_unique_1 from anchor_number_1
        let prev = recovery_keys_1;
        let curr = vec![]; // all removed for anchor_number_1

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number_1, &prev, &curr);

        let principal_shared = Principal::self_authenticating(&device_shared.pubkey);
        let principal_unique_1 = Principal::self_authenticating(&device_unique_1.pubkey);
        let principal_unique_2 = Principal::self_authenticating(&device_unique_2.pubkey);

        // Should be removed for anchor_number_1
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_shared),
            Some(anchor_number_2)
        );
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_unique_1),
            None
        );
        // Should remain for anchor_number_2
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_unique_2),
            Some(anchor_number_2)
        );
    }

    #[test]
    fn write_moves_recovery_phrase_principal_between_anchors() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());

        let d1 = other_device(pubkey(1));
        let d2 = seed_phrase_device(pubkey(2)); // recovery device
        let d3 = other_device(pubkey(3));

        let mut anchor_a = storage.allocate_anchor(111).unwrap();
        anchor_a.add_device(d1.clone()).unwrap();
        anchor_a.add_device(d2.clone()).unwrap();

        // Code under test (I)
        storage.write(anchor_a.clone()).unwrap();

        let mut anchor_b = storage.allocate_anchor(222).unwrap();
        anchor_b.add_device(d3.clone()).unwrap();

        // Code under test (II)
        storage.write(anchor_b.clone()).unwrap();

        let principal_d2 = Principal::self_authenticating(&d2.pubkey);

        // d2 should be indexed for anchor_a
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .iter()
                .collect::<Vec<_>>(),
            vec![(principal_d2, anchor_a.anchor_number())]
        );

        // Remove d2 from anchor_a
        anchor_a.remove_device(&d2.pubkey).unwrap();

        // Code under test (III)
        storage.write(anchor_a).unwrap();

        // No recovery devices are left in the index.
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .iter()
                .collect::<Vec<_>>(),
            vec![]
        );

        // Add d2 to anchor_b
        anchor_b.add_device(d2).unwrap();

        // Code under test (IV)
        storage.write(anchor_b.clone()).unwrap();

        // d2 should now be indexed for anchor_b only
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .iter()
                .collect::<Vec<_>>(),
            vec![(principal_d2, anchor_b.anchor_number())]
        );
    }
}

/// Tests that anchors created using `Storage.write` can be read into expected structures.
#[test]
fn test_anchor_storage_migration_round_trip() {
    let mut storage = Storage::new((0, 100), VectorMemory::default());
    let now = 123;

    let test_cases = [
        // Test case 0: Empty anchor
        (
            "empty anchor",
            storage.allocate_anchor(now).unwrap(),
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 0,
                devices: vec![],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 1: Valid recovery phrase (happy case)
        (
            "valid recovery phrase",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("recovery_key_pubkey"),
                        alias: "Recovery Key".to_string(),
                        credential_id: None,
                        aaguid: None,
                        purpose: Purpose::Recovery,
                        key_type: KeyType::SeedPhrase,
                        protection: DeviceProtection::Unprotected,
                        origin: None,
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 1,
                devices: vec![Device {
                    pubkey: ByteBuf::from("recovery_key_pubkey"),
                    alias: "Recovery Key".to_string(),
                    credential_id: None,
                    aaguid: None,
                    purpose: Purpose::Recovery,
                    key_type: KeyType::SeedPhrase,
                    protection: DeviceProtection::Unprotected,
                    origin: None,
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 2: Valid passkey with origin (happy case)
        (
            "valid passkey with origin",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("passkey_pubkey"),
                        alias: "My Passkey".to_string(),
                        credential_id: Some(ByteBuf::from("credential_id_123")),
                        aaguid: Some([1u8; 16]),
                        purpose: Purpose::Authentication,
                        key_type: KeyType::CrossPlatform,
                        protection: DeviceProtection::Unprotected,
                        origin: Some("https://identity.ic0.app".to_string()),
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 2,
                devices: vec![Device {
                    pubkey: ByteBuf::from("passkey_pubkey"),
                    alias: "My Passkey".to_string(),
                    credential_id: Some(ByteBuf::from("credential_id_123")),
                    aaguid: Some([1u8; 16]),
                    purpose: Purpose::Authentication,
                    key_type: KeyType::CrossPlatform,
                    protection: DeviceProtection::Unprotected,
                    origin: Some("https://identity.ic0.app".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 3: Valid passkey without origin (special case)
        (
            "passkey without origin",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("passkey_no_origin"),
                        alias: "Passkey No Origin".to_string(),
                        credential_id: Some(ByteBuf::from("cred_id_no_origin")),
                        aaguid: None,
                        purpose: Purpose::Authentication,
                        key_type: KeyType::CrossPlatform,
                        protection: DeviceProtection::Unprotected,
                        origin: None,
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 3,
                devices: vec![Device {
                    pubkey: ByteBuf::from("passkey_no_origin"),
                    alias: "Passkey No Origin".to_string(),
                    credential_id: Some(ByteBuf::from("cred_id_no_origin")),
                    aaguid: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::CrossPlatform,
                    protection: DeviceProtection::Unprotected,
                    origin: None,
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 4: Recovery passkey with origin (special case)
        (
            "recovery passkey with origin",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("recovery_passkey"),
                        alias: "Recovery Passkey".to_string(),
                        credential_id: Some(ByteBuf::from("recovery_cred_id")),
                        aaguid: None,
                        purpose: Purpose::Recovery,
                        key_type: KeyType::Platform,
                        protection: DeviceProtection::Unprotected,
                        origin: Some("https://identity.ic0.app".to_string()),
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 4,
                devices: vec![Device {
                    pubkey: ByteBuf::from("recovery_passkey"),
                    alias: "Recovery Passkey".to_string(),
                    credential_id: Some(ByteBuf::from("recovery_cred_id")),
                    aaguid: None,
                    purpose: Purpose::Recovery,
                    key_type: KeyType::Platform,
                    protection: DeviceProtection::Unprotected,
                    origin: Some("https://identity.ic0.app".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 5: Recovery passkey without origin (special case)
        (
            "recovery passkey without origin",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("recovery_passkey_no_origin"),
                        alias: "Recovery Passkey No Origin".to_string(),
                        credential_id: Some(ByteBuf::from("recovery_cred_id_no_origin")),
                        aaguid: None,
                        purpose: Purpose::Recovery,
                        key_type: KeyType::Unknown,
                        protection: DeviceProtection::Unprotected,
                        origin: None,
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 5,
                devices: vec![Device {
                    pubkey: ByteBuf::from("recovery_passkey_no_origin"),
                    alias: "Recovery Passkey No Origin".to_string(),
                    credential_id: Some(ByteBuf::from("recovery_cred_id_no_origin")),
                    aaguid: None,
                    purpose: Purpose::Recovery,
                    key_type: KeyType::Unknown,
                    protection: DeviceProtection::Unprotected,
                    origin: None,
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 6: Legacy pin-flow with BrowserStorageKey and Authentication purpose (special case)
        (
            "legacy pin-flow authentication",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("browser_storage_key_auth"),
                        alias: "Browser Storage".to_string(),
                        credential_id: None,
                        aaguid: None,
                        purpose: Purpose::Authentication,
                        key_type: KeyType::BrowserStorageKey,
                        protection: DeviceProtection::Unprotected,
                        origin: Some("https://identity.ic0.app".to_string()),
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 6,
                devices: vec![Device {
                    pubkey: ByteBuf::from("browser_storage_key_auth"),
                    alias: "Browser Storage".to_string(),
                    credential_id: None,
                    aaguid: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::BrowserStorageKey,
                    protection: DeviceProtection::Unprotected,
                    origin: Some("https://identity.ic0.app".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 7: Legacy pin-flow with BrowserStorageKey and Recovery purpose (special case)
        (
            "legacy pin-flow recovery",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("browser_storage_key_recovery"),
                        alias: "Browser Storage Recovery".to_string(),
                        credential_id: None,
                        aaguid: None,
                        purpose: Purpose::Recovery,
                        key_type: KeyType::BrowserStorageKey,
                        protection: DeviceProtection::Unprotected,
                        origin: None,
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 7,
                devices: vec![Device {
                    pubkey: ByteBuf::from("browser_storage_key_recovery"),
                    alias: "Browser Storage Recovery".to_string(),
                    credential_id: None,
                    aaguid: None,
                    purpose: Purpose::Recovery,
                    key_type: KeyType::BrowserStorageKey,
                    protection: DeviceProtection::Unprotected,
                    origin: None,
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 8: Multiple devices of different types
        (
            "mixed device types",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("passkey1"),
                        alias: "Passkey 1".to_string(),
                        credential_id: Some(ByteBuf::from("cred1")),
                        aaguid: None,
                        purpose: Purpose::Authentication,
                        key_type: KeyType::CrossPlatform,
                        protection: DeviceProtection::Unprotected,
                        origin: Some("https://identity.ic0.app".to_string()),
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("recovery_phrase"),
                        alias: "Recovery Key".to_string(),
                        credential_id: None,
                        aaguid: None,
                        purpose: Purpose::Recovery,
                        key_type: KeyType::SeedPhrase,
                        protection: DeviceProtection::Unprotected,
                        origin: None,
                        last_usage_timestamp: Some(now + 10),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 8,
                devices: vec![
                    Device {
                        pubkey: ByteBuf::from("passkey1"),
                        alias: "Passkey 1".to_string(),
                        credential_id: Some(ByteBuf::from("cred1")),
                        aaguid: None,
                        purpose: Purpose::Authentication,
                        key_type: KeyType::CrossPlatform,
                        protection: DeviceProtection::Unprotected,
                        origin: Some("https://identity.ic0.app".to_string()),
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    },
                    Device {
                        pubkey: ByteBuf::from("recovery_phrase"),
                        alias: "Recovery Key".to_string(),
                        credential_id: None,
                        aaguid: None,
                        purpose: Purpose::Recovery,
                        key_type: KeyType::SeedPhrase,
                        protection: DeviceProtection::Unprotected,
                        origin: None,
                        last_usage_timestamp: Some(now + 10),
                        metadata: None,
                    },
                ],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 9: Anchor with OpenID credentials
        (
            "anchor with openid credentials",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor.add_openid_credential(openid_credential(1)).unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 9,
                devices: vec![],
                openid_credentials: vec![openid_credential(1)],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 10: Anchor with name
        (
            "anchor with name",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor.set_name(Some("My Test Anchor".to_string())).unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 10,
                devices: vec![],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: Some("My Test Anchor".to_string()),
                created_at: Some(now),
            },
        ),
        // Test case 11: Passkey with KeyType::Unknown (should be handled correctly)
        (
            "passkey with unknown key type",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("unknown_keytype_passkey"),
                        alias: "Unknown KeyType".to_string(),
                        credential_id: Some(ByteBuf::from("unknown_cred")),
                        aaguid: None,
                        purpose: Purpose::Authentication,
                        key_type: KeyType::Unknown,
                        protection: DeviceProtection::Unprotected,
                        origin: Some("https://identity.ic0.app".to_string()),
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 11,
                devices: vec![Device {
                    pubkey: ByteBuf::from("unknown_keytype_passkey"),
                    alias: "Unknown KeyType".to_string(),
                    credential_id: Some(ByteBuf::from("unknown_cred")),
                    aaguid: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::CrossPlatform,
                    protection: DeviceProtection::Unprotected,
                    origin: Some("https://identity.ic0.app".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 12: Device metadata is not preserved
        (
            "device metadata not preserved",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                let mut device_metadata = HashMap::new();
                device_metadata.insert(
                    "custom_field".to_string(),
                    internet_identity_interface::internet_identity::types::MetadataEntry::String(
                        "custom_value".to_string(),
                    ),
                );
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("device_with_metadata"),
                        alias: "Device With Metadata".to_string(),
                        credential_id: Some(ByteBuf::from("cred_with_metadata")),
                        aaguid: None,
                        purpose: Purpose::Authentication,
                        key_type: KeyType::CrossPlatform,
                        protection: DeviceProtection::Unprotected,
                        origin: Some("https://identity.ic0.app".to_string()),
                        last_usage_timestamp: Some(now),
                        metadata: Some(device_metadata),
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 12,
                devices: vec![Device {
                    pubkey: ByteBuf::from("device_with_metadata"),
                    alias: "Device With Metadata".to_string(),
                    credential_id: Some(ByteBuf::from("cred_with_metadata")),
                    aaguid: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::CrossPlatform,
                    protection: DeviceProtection::Unprotected,
                    origin: Some("https://identity.ic0.app".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None, // Metadata not preserved in stable memory
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 13: Identity metadata is not preserved
        (
            "identity metadata not preserved",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                let mut identity_metadata = HashMap::new();
                identity_metadata.insert(
                    "identity_field".to_string(),
                    internet_identity_interface::internet_identity::types::MetadataEntry::String(
                        "identity_value".to_string(),
                    ),
                );
                anchor.replace_identity_metadata(identity_metadata).unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 13,
                devices: vec![],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None, // Identity metadata not preserved in stable memory
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 14: Device protection is preserved for recovery phrases.
        (
            "device protection preserved for recovery phrase",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("protected_recovery_key"),
                        alias: "Recovery Key".to_string(),
                        credential_id: None,
                        aaguid: None,
                        purpose: Purpose::Recovery,
                        key_type: KeyType::SeedPhrase,
                        protection: DeviceProtection::Protected,
                        origin: None,
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 14,
                devices: vec![Device {
                    pubkey: ByteBuf::from("protected_recovery_key"),
                    alias: "Recovery Key".to_string(),
                    credential_id: None,
                    aaguid: None,
                    purpose: Purpose::Recovery,
                    key_type: KeyType::SeedPhrase,
                    protection: DeviceProtection::Protected, // Protection is preserved
                    origin: None,
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 15: Device protection is preserved for passkeys.
        (
            "device protection defaults to unprotected for passkeys",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                // NOTE: We assign directly to `anchor.devices` here to construct a legacy
                //       anchor state that may not satisfy the invariants enforced by
                //       `add_device()`. This is intentional for testing migration logic
                //       and should not be used as a pattern in regular tests.
                anchor.devices = vec![Device {
                    pubkey: ByteBuf::from("protected_passkey"),
                    alias: "Protected Passkey".to_string(),
                    credential_id: Some(ByteBuf::from("protected_cred")),
                    aaguid: None,
                    // To make this test more realistic, we opt for (legacy) recovery passkeys.
                    purpose: Purpose::Recovery,
                    key_type: KeyType::CrossPlatform,
                    protection: DeviceProtection::Protected,
                    origin: Some("https://id.ai".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }];
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 15,
                devices: vec![Device {
                    pubkey: ByteBuf::from("protected_passkey"),
                    alias: "Protected Passkey".to_string(),
                    credential_id: Some(ByteBuf::from("protected_cred")),
                    aaguid: None,
                    purpose: Purpose::Recovery,
                    key_type: KeyType::CrossPlatform,
                    protection: DeviceProtection::Unprotected, // Protection is NOT preserved
                    origin: Some("https://id.ai".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 16: Fallthrough case - unusual combination that doesn't match other patterns
        (
            "fallthrough case - recovery with browser storage key and origin",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                // NOTE: We assign directly to `anchor.devices` here to construct a legacy
                //       anchor state that may not satisfy the invariants enforced by
                //       `add_device()`. This is intentional for testing migration logic
                //       and should not be used as a pattern in regular tests.
                anchor.devices = vec![Device {
                    pubkey: ByteBuf::from("unusual_device"),
                    alias: "Unusual Device".to_string(),
                    credential_id: Some(ByteBuf::from("unusual_cred")),
                    aaguid: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::SeedPhrase, // Unusual: SeedPhrase with credential_id
                    protection: DeviceProtection::Unprotected,
                    origin: Some("https://identity.ic0.app".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }];
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 16,
                devices: vec![Device {
                    pubkey: ByteBuf::from("unusual_device"),
                    alias: "Unusual Device".to_string(),
                    credential_id: Some(ByteBuf::from("unusual_cred")),
                    aaguid: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::SeedPhrase,
                    protection: DeviceProtection::Unprotected,
                    origin: Some("https://identity.ic0.app".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 17: Recovery phrase alias defaults to "Recovery Key"
        (
            "recovery phrase alias defaults to 'Recovery Key'",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("recovery_phrase_custom_alias"),
                        alias: "My Custom Recovery Alias".to_string(),
                        credential_id: None,
                        aaguid: None,
                        purpose: Purpose::Recovery,
                        key_type: KeyType::SeedPhrase,
                        protection: DeviceProtection::Unprotected,
                        origin: None,
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 17,
                devices: vec![Device {
                    pubkey: ByteBuf::from("recovery_phrase_custom_alias"),
                    alias: "Recovery Key".to_string(), // Alias defaults to "Recovery Key"
                    credential_id: None,
                    aaguid: None,
                    purpose: Purpose::Recovery,
                    key_type: KeyType::SeedPhrase,
                    protection: DeviceProtection::Unprotected,
                    origin: None,
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 18: Passkey key_type defaults to CrossPlatform (Platform -> CrossPlatform)
        (
            "passkey key_type Platform defaults to CrossPlatform",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("platform_passkey"),
                        alias: "Platform Passkey".to_string(),
                        credential_id: Some(ByteBuf::from("platform_cred")),
                        aaguid: None,
                        purpose: Purpose::Authentication,
                        key_type: KeyType::Platform, // Will be normalized to CrossPlatform
                        protection: DeviceProtection::Unprotected,
                        origin: Some("https://identity.ic0.app".to_string()),
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 18,
                devices: vec![Device {
                    pubkey: ByteBuf::from("platform_passkey"),
                    alias: "Platform Passkey".to_string(),
                    credential_id: Some(ByteBuf::from("platform_cred")),
                    aaguid: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::CrossPlatform, // Defaults to CrossPlatform
                    protection: DeviceProtection::Unprotected,
                    origin: Some("https://identity.ic0.app".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
        // Test case 19: Passkey key_type defaults to CrossPlatform (Unknown -> CrossPlatform)
        (
            "passkey key_type Unknown defaults to CrossPlatform",
            {
                let mut anchor = storage.allocate_anchor(now).unwrap();
                anchor
                    .add_device(Device {
                        pubkey: ByteBuf::from("unknown_keytype_passkey_2"),
                        alias: "Unknown Type Passkey".to_string(),
                        credential_id: Some(ByteBuf::from("unknown_cred_2")),
                        aaguid: None,
                        purpose: Purpose::Authentication,
                        key_type: KeyType::Unknown, // Will be normalized to CrossPlatform
                        protection: DeviceProtection::Unprotected,
                        origin: Some("https://identity.ic0.app".to_string()),
                        last_usage_timestamp: Some(now),
                        metadata: None,
                    })
                    .unwrap();
                anchor
            },
            Anchor {
                browsers: vec![],
                next_browser_id: 0,
                session_count: 0,
                anchor_number: 19,
                devices: vec![Device {
                    pubkey: ByteBuf::from("unknown_keytype_passkey_2"),
                    alias: "Unknown Type Passkey".to_string(),
                    credential_id: Some(ByteBuf::from("unknown_cred_2")),
                    aaguid: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::CrossPlatform, // Defaults to CrossPlatform
                    protection: DeviceProtection::Unprotected,
                    origin: Some("https://identity.ic0.app".to_string()),
                    last_usage_timestamp: Some(now),
                    metadata: None,
                }],
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                metadata: None,
                name: None,
                created_at: Some(now),
            },
        ),
    ];

    for (label, anchor, expected_anchor) in test_cases {
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap_or_else(|e| {
            panic!("Test case '{}' failed during write: {:?}", label, e);
        });
        let observed_anchor = storage.read(anchor_number).unwrap_or_else(|e| {
            panic!("Test case '{}' failed during read: {:?}", label, e);
        });

        assert_eq!(
            observed_anchor, expected_anchor,
            "Test case '{}' failed",
            label
        );
    }
}

mod reference_list_write_path_tests {
    use super::application_number_for;
    use super::{write_at, write_at_with_record};
    use crate::storage::account::Account;
    use crate::storage::account::AccountReference;
    use crate::storage::storable::account::StorableAccount;
    use crate::storage::storable::accounts_counter::StorableAccountsCounter;
    use crate::storage::AccountReferenceWrite;
    use crate::storage::{ReferenceCount, ReferenceCounter, StorageError};
    use crate::Storage;
    use ic_stable_structures::VectorMemory;
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use internet_identity_interface::internet_identity::types::FrontendHostname;
    use pretty_assertions::assert_eq;
    use std::collections::BTreeMap;

    fn storage_with_anchor() -> (Storage<VectorMemory>, AnchorNumber) {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        storage.update_salt([17u8; 32]);
        let anchor = storage.allocate_anchor(0).unwrap();
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap();
        (storage, anchor_number)
    }

    /// The write path takes the identity, not its number, so there is nothing to write
    /// against when the identity does not exist. Writing anyway would leave counters,
    /// account reference lists and an application keyed on an owner that never existed,
    /// and nothing would ever prune them.
    #[test]
    fn an_account_write_for_an_identity_that_does_not_exist_is_refused() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let never_allocated = anchor_number + 1;
        let origin = "https://example.com".to_string();

        let result = storage.create_account(never_allocated, origin.clone(), "named".to_string());

        assert!(matches!(result, Err(StorageError::BadAnchorNumber(_))));
        // Refused before anything was written, the application included.
        assert_eq!(storage.lookup_application_number_with_origin(&origin), None);
        assert_eq!(storage.get_total_application_count(), 0);
    }

    /// Everything a write derives, rebuilt from the account reference lists alone.
    ///
    /// A gate whose job is deriving values is only as good as a check that does the
    /// deriving a second way, so this is the shape the counter assertions below take.
    fn derived_counters(
        storage: &Storage<VectorMemory>,
        anchor_number: AnchorNumber,
    ) -> (u64, u64) {
        let mut accounts = 0;
        let mut references = 0;
        for reference in storage.list_identity_account_references(anchor_number) {
            references += 1;
            if reference.account_number.is_some() {
                accounts += 1;
            }
        }
        (accounts, references)
    }

    #[test]
    fn reading_the_state_and_writing_it_back_changes_nothing() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();
        let before = storage
            .stable_anchor_account_counter_memory
            .get(&anchor_number);
        let stored = storage.stored_account_references(anchor_number, 0);

        let state = storage.account_state_for_origin(anchor_number, &origin);
        storage
            .write_account_state_for_testing(
                anchor_number,
                BTreeMap::from([(origin.clone(), Some(state))]),
            )
            .unwrap();

        assert_eq!(
            storage
                .stable_anchor_account_counter_memory
                .get(&anchor_number),
            before
        );
        assert_eq!(storage.stored_account_references(anchor_number, 0), stored);
    }

    #[test]
    fn writing_back_an_untouched_origin_does_not_give_it_an_application() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://nothing-here.com".to_string();

        // What the identity holds at an origin nothing is stored under is the derived
        // default. Writing that back says only what absence already says, so storing it
        // would leave an account reference list — and an application — that nothing asked
        // for and no counter would ever retire.
        let state = storage.account_state_for_origin(anchor_number, &origin);
        storage
            .write_account_state_for_testing(
                anchor_number,
                BTreeMap::from([(origin.clone(), Some(state))]),
            )
            .unwrap();

        assert_eq!(storage.lookup_application_number_with_origin(&origin), None);
        assert_eq!(storage.get_total_application_count(), 0);
        assert_eq!(derived_counters(&storage, anchor_number), (0, 0));
    }

    #[test]
    fn a_first_account_at_an_origin_creates_its_application_with_the_counters_already_right() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();

        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .expect("the write created the application");
        let application = storage
            .stable_application_memory
            .get(&application_number)
            .expect("and stored it");
        // One named account and the tracked default beside it. The application never
        // existed holding zero of either: it is written with what holds it.
        assert_eq!(
            (
                application.stored_accounts,
                application.stored_account_references
            ),
            (1, 2)
        );
        assert_eq!(derived_counters(&storage, anchor_number), (1, 2));
    }

    #[test]
    fn naming_an_account_mints_its_number_and_hands_it_back_in_place() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let default_reference = AccountReference::new(None, None);

        // A record on an account reference with no number is an account being named. The
        // caller never states the number; it comes back where the account reference was.
        let written = storage
            .write_account_state_for_testing(
                anchor_number,
                BTreeMap::from([(
                    origin.clone(),
                    Some((
                        vec![
                            AccountReferenceWrite::from(default_reference),
                            AccountReferenceWrite {
                                account_reference: AccountReference::new(None, None),
                                record: Some(StorableAccount {
                                    name: "named".to_string(),
                                    seed_from_anchor: None,
                                }),
                            },
                        ],
                        None,
                    )),
                )]),
            )
            .unwrap();

        let (account_references, _) = written[&origin]
            .as_ref()
            .expect("a write that holds something is handed back holding it");
        assert_eq!(account_references[0].account_reference.account_number, None);
        assert_eq!(
            account_references[1].account_reference.account_number,
            Some(1)
        );
    }

    #[test]
    fn naming_the_tracked_default_makes_it_the_default() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        // The tracked default gains a number and a record, and nothing numberless is
        // left. What the identity signs in with here is now that account, so the config
        // says so — derived from the write, because the caller could not have named a
        // number that did not exist yet.
        let written = storage
            .write_account_state_for_testing(
                anchor_number,
                write_at_with_record(
                    &origin,
                    vec![AccountReference::new(None, None)],
                    None,
                    StorableAccount {
                        name: "named".to_string(),
                        seed_from_anchor: Some(anchor_number),
                    },
                    None,
                ),
            )
            .unwrap();

        let account_number = written[&origin]
            .as_ref()
            .expect("a write that holds something is handed back holding it")
            .0[0]
            .account_reference
            .account_number;
        assert_eq!(account_number, Some(1));
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        assert_eq!(
            storage
                .stable_anchor_application_config_memory
                .get(&(anchor_number, application_number))
                .map(|config| config.default_account_number),
            Some(account_number)
        );
    }

    #[test]
    fn adding_an_account_beside_a_default_that_stays_does_not_move_the_default() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        // Also a mint, but the tracked default is still there afterwards, so this is a
        // new account rather than the default being named.
        storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();

        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        assert_eq!(
            storage
                .stable_anchor_application_config_memory
                .get(&(anchor_number, application_number)),
            None
        );
    }

    #[test]
    fn an_empty_list_is_refused_at_an_origin_nothing_is_stored_under() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        // A tombstone, which nothing may create yet. An empty list holds no account
        // reference with a number, which is also true of a list holding only the derived
        // default — so this has to be told apart from the write that stores nothing.
        let result =
            storage.write_account_state_for_testing(anchor_number, write_at(&origin, vec![], None));

        assert!(matches!(
            result,
            Err(StorageError::UnstorableAccountReferenceList { .. })
        ));
        assert_eq!(storage.lookup_application_number_with_origin(&origin), None);
    }

    #[test]
    fn a_refusal_at_one_origin_writes_nothing_at_any_of_them() {
        let (mut storage, anchor_number) = storage_with_anchor();
        // Ordered so the write that succeeds is reached first: the batch is keyed by
        // origin, so a gate that stored as it went would have already written this one
        // by the time the next is refused. Named the other way round the test passes
        // whether or not anything is atomic.
        let good = "https://a-good.com".to_string();
        let bad = "https://z-bad.com".to_string();

        let mut writes: BTreeMap<FrontendHostname, _> = BTreeMap::new();
        writes.extend(write_at_with_record(
            &good,
            vec![AccountReference::new(None, None)],
            None,
            StorableAccount {
                name: "named".to_string(),
                seed_from_anchor: None,
            },
            None,
        ));
        // Refused: nothing may write a tombstone.
        writes.extend(write_at(&bad, vec![], None));

        let result = storage.write_account_state_for_testing(anchor_number, writes);

        assert!(result.is_err());
        // Not one of them half-happened: no application, no account number spent, no
        // counter moved.
        assert_eq!(storage.lookup_application_number_with_origin(&good), None);
        assert_eq!(storage.lookup_application_number_with_origin(&bad), None);
        assert_eq!(storage.get_total_application_count(), 0);
        assert_eq!(
            storage.stable_account_counter_memory.get().stored_accounts,
            0
        );
        assert_eq!(derived_counters(&storage, anchor_number), (0, 0));
    }

    #[test]
    fn one_write_spanning_two_origins_moves_the_shared_counters_once() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let first = "https://first.com".to_string();
        let second = "https://second.com".to_string();

        let mut writes: BTreeMap<FrontendHostname, _> = BTreeMap::new();
        for origin in [&first, &second] {
            writes.extend(write_at_with_record(
                origin,
                vec![AccountReference::new(None, None)],
                None,
                StorableAccount {
                    name: "named".to_string(),
                    seed_from_anchor: None,
                },
                None,
            ));
        }

        storage
            .write_account_state_for_testing(anchor_number, writes)
            .unwrap();

        // Two accounts, two account references, and two distinct numbers: the counters
        // shared by both origins are folded across the call rather than each computed
        // against the same stored value.
        assert_eq!(derived_counters(&storage, anchor_number), (2, 2));
        assert_eq!(
            storage
                .stable_anchor_account_counter_memory
                .get(&anchor_number),
            Some(StorableAccountsCounter {
                stored_accounts: 2,
                stored_account_references: 2,
            })
        );
        assert_eq!(
            storage.stable_account_counter_memory.get().stored_accounts,
            2
        );
        assert_eq!(storage.get_total_application_count(), 2);
    }

    #[test]
    fn a_write_refused_by_the_counters_spends_no_application_number() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();
        // Force the divergence: the stored list holds references the counter no longer
        // knows about, so dropping one under-runs it.
        storage.set_counters_for_testing(anchor_number, 0, 0);
        let next_application_number = *storage.next_application_number_memory.get();
        let allocator = storage.stable_account_counter_memory.get().stored_accounts;

        let result = storage.write_account_state_for_testing(
            anchor_number,
            write_at(&origin, vec![AccountReference::new(None, None)], None),
        );

        assert!(matches!(
            result,
            Err(StorageError::AccountCounterOutOfBounds { .. })
        ));
        // The allocators only move in apply, so a refusal leaves both where they were.
        assert_eq!(
            *storage.next_application_number_memory.get(),
            next_application_number
        );
        assert_eq!(
            storage.stable_account_counter_memory.get().stored_accounts,
            allocator
        );
    }

    #[test]
    fn a_new_application_is_never_stored_holding_nothing() {
        let (mut storage, anchor_number) = storage_with_anchor();

        // Every application this identity can reach it through, at every point a write
        // could have left one behind.
        for origin in [
            "https://untouched.com".to_string(),
            "https://named.com".to_string(),
        ] {
            let state = storage.account_state_for_origin(anchor_number, &origin);
            storage
                .write_account_state_for_testing(
                    anchor_number,
                    BTreeMap::from([(origin.clone(), Some(state))]),
                )
                .unwrap();
        }
        storage
            .create_account(
                anchor_number,
                "https://named.com".to_string(),
                "named".to_string(),
            )
            .unwrap();

        for (number, application) in storage.stable_application_memory.iter() {
            let held = storage
                .stored_account_references(anchor_number, number)
                .map(|references| references.len() as u64)
                .unwrap_or_default();
            assert!(
                held > 0,
                "application {number} at {} is stored holding nothing",
                application.origin
            );
            assert_eq!(application.stored_account_references, held);
        }
    }

    #[test]
    fn allocating_past_the_last_account_number_is_refused() {
        let (mut storage, anchor_number) = storage_with_anchor();
        // The allocator hands out the incremented count, so there is no number left
        // after this one. Reachable only here, which is the point: it refuses rather
        // than re-issuing a number, and it does so without trapping.
        storage
            .stable_account_counter_memory
            .set(StorableAccountsCounter {
                stored_accounts: u64::MAX,
                stored_account_references: 0,
            })
            .unwrap();

        let result = storage.create_account(
            anchor_number,
            "https://example.com".to_string(),
            "named".to_string(),
        );

        assert!(matches!(result, Err(StorageError::AccountsCounterOverflow)));
    }

    #[test]
    fn a_refused_default_account_rename_leaves_nothing_behind() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let application_number = application_number_for(&mut storage, &origin);
        // A list with no default reference: the default was removed, so there is nothing
        // for a rename to name.
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(&origin, vec![AccountReference::new(Some(1), None)], None),
            )
            .unwrap();
        let accounts_before = storage.stable_account_memory.len();
        let config_before =
            storage.lookup_anchor_application_config(anchor_number, application_number);

        let result = storage.write_account(Account::new(
            anchor_number,
            origin.clone(),
            Some("named".to_string()),
            None,
        ));

        assert!(matches!(result, Err(StorageError::MissingAccount { .. })));
        // No account number burned, no account stored, no config rewritten.
        assert_eq!(storage.stable_account_memory.len(), accounts_before);
        assert_eq!(
            storage
                .lookup_anchor_application_config(anchor_number, application_number)
                .default_account_number,
            config_before.default_account_number
        );
    }

    #[test]
    fn refuses_a_counter_delta_that_would_underflow_without_writing_anything() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let application_number = application_number_for(&mut storage, &origin);
        let default_reference = AccountReference::new(None, None);
        let named_reference = AccountReference::new(Some(1), None);
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(
                    &origin,
                    vec![default_reference.clone(), named_reference],
                    None,
                ),
            )
            .unwrap();

        // Force the divergence this guards: the stored list holds two references the
        // anchor counter no longer knows about, so dropping one under-runs it.
        storage.set_counters_for_testing(anchor_number, 0, 0);

        let result = storage.write_account_state_for_testing(
            anchor_number,
            write_at(&origin, vec![default_reference], None),
        );

        // The refusal names what diverged: this identity's account count, what it held,
        // and the move that would not fit.
        assert_eq!(
            result.unwrap_err().to_string(),
            StorageError::AccountCounterOutOfBounds {
                counter: ReferenceCounter::Anchor { anchor_number },
                count: ReferenceCount::Accounts,
                stored: 0,
                delta: -1,
            }
            .to_string()
        );
        // Refused before anything was written: the list still holds both references.
        let references = storage
            .stored_account_references(anchor_number, application_number)
            .expect("the list written above is gone");
        assert_eq!(references.len(), 2);
    }

    #[test]
    fn the_two_counts_move_independently_and_the_refusal_says_which_one_failed() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let default_reference = AccountReference::new(None, None);
        let named_reference = AccountReference::new(Some(1), None);
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(
                    &origin,
                    vec![default_reference, named_reference.clone()],
                    None,
                ),
            )
            .unwrap();
        storage.set_counters_for_testing(anchor_number, 0, 0);

        // Dropping the tracked default takes a reference without taking a named
        // account, so the two deltas differ: 0 and -1. Only the reference count can
        // under-run here, and the refusal has to name that one rather than the other.
        let result = storage.write_account_state_for_testing(
            anchor_number,
            write_at(&origin, vec![named_reference], None),
        );

        assert_eq!(
            result.unwrap_err().to_string(),
            StorageError::AccountCounterOutOfBounds {
                counter: ReferenceCounter::Anchor { anchor_number },
                count: ReferenceCount::References,
                stored: 0,
                delta: -1,
            }
            .to_string()
        );
    }

    #[test]
    fn an_empty_list_cannot_be_handed_to_the_write_path() {
        // Refused by `StorableAccountReferenceList`, so the write path cannot store one
        // however the caller assembled it.
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let application_number = application_number_for(&mut storage, &origin);

        let result =
            storage.write_account_state_for_testing(anchor_number, write_at(&origin, vec![], None));

        assert!(matches!(
            result,
            Err(StorageError::UnstorableAccountReferenceList { .. })
        ));
        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            None
        );
    }

    #[test]
    fn refuses_an_origin_whose_application_is_gone_without_writing_anything() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let application_number = application_number_for(&mut storage, &origin);

        // The origin index still resolves, but the application it names is gone. That is
        // a broken invariant rather than an origin nobody has stored anything under, so
        // the write refuses instead of quietly creating a second application for it.
        storage
            .stable_application_memory
            .remove(&application_number);

        let result = storage.write_account_state_for_testing(
            anchor_number,
            write_at(&origin, vec![AccountReference::new(Some(1), None)], None),
        );

        assert!(matches!(
            result,
            Err(StorageError::OriginNotFoundForApplicationNumber { .. })
        ));
        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            None
        );
    }

    #[test]
    fn writing_the_list_the_list_already_holds_touches_nothing() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let application_number = application_number_for(&mut storage, &origin);
        let references = vec![AccountReference::new(Some(1), None)];
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(&origin, references.clone(), None),
            )
            .unwrap();
        // Retiring the application makes a write visible: the write path refuses
        // without one, so a write that still went through it could not succeed here.
        storage
            .stable_application_memory
            .remove(&application_number);

        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(&origin, references.clone(), None),
            )
            .unwrap();

        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            Some(references)
        );
    }

    #[test]
    fn derives_counters_from_added_references() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(
                    &origin,
                    vec![
                        AccountReference::new(None, None),
                        AccountReference::new(Some(7), None),
                    ],
                    None,
                ),
            )
            .unwrap();

        let anchor_counter = storage.get_account_counter(anchor_number);
        assert_eq!(anchor_counter.stored_account_references, 2);
        assert_eq!(anchor_counter.stored_accounts, 1);

        let application = storage.lookup_application_with_origin(&origin).unwrap();
        assert_eq!(application.stored_account_references, 2);
        assert_eq!(application.stored_accounts, 1);

        assert_eq!(
            storage
                .get_total_accounts_counter()
                .stored_account_references,
            2
        );
    }

    #[test]
    fn materializing_a_default_moves_only_the_account_counter() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(&origin, vec![AccountReference::new(None, None)], None),
            )
            .unwrap();
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(&origin, vec![AccountReference::new(Some(3), None)], None),
            )
            .unwrap();

        let anchor_counter = storage.get_account_counter(anchor_number);
        assert_eq!(anchor_counter.stored_account_references, 1);
        assert_eq!(anchor_counter.stored_accounts, 1);

        let application = storage.lookup_application_with_origin(&origin).unwrap();
        assert_eq!(application.stored_account_references, 1);
        assert_eq!(application.stored_accounts, 1);
    }

    #[test]
    fn rewriting_an_unchanged_list_leaves_counters_alone() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let references = vec![AccountReference::new(Some(1), None)];

        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(&origin, references.clone(), None),
            )
            .unwrap();
        let after_first_write = storage.get_account_counter(anchor_number);

        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(
                    &origin,
                    vec![AccountReference::new(Some(1), Some(123))],
                    None,
                ),
            )
            .unwrap();

        assert_eq!(
            storage.get_account_counter(anchor_number),
            after_first_write
        );
    }

    #[test]
    fn written_counters_match_a_rebuild() {
        let (mut storage, anchor_number) = storage_with_anchor();

        for origin in ["https://a.com", "https://b.com", "https://c.com"] {
            let origin = origin.to_string();
            storage
                .create_account(anchor_number, origin.clone(), "account".to_string())
                .unwrap();
            storage
                .create_account(anchor_number, origin, "another account".to_string())
                .unwrap();
        }

        // Counted a second way: what the write path derived, against what the account
        // reference lists actually hold. There is no repair path any more, so this is the
        // check that the derivation is right.
        let written = storage.get_account_counter(anchor_number);
        let mut accounts = 0;
        let mut references = 0;
        for reference in storage.list_identity_account_references(anchor_number) {
            references += 1;
            if reference.account_number.is_some() {
                accounts += 1;
            }
        }

        assert_eq!(written.stored_account_references, references);
        assert_eq!(written.stored_accounts, accounts);
        assert_eq!(written.stored_account_references, 9);
        assert_eq!(written.stored_accounts, 6);
    }
}

/// A `(anchor, application)` list can be absent, empty, or hold references, and those
/// mean three different things. Absence says a default account is still
/// reconstructible; emptiness is a tombstone and says it never can be again.
mod account_reference_state_tests {
    use super::application_number_for;
    use super::write_at;
    use crate::storage::account::{Account, AccountKey, AccountReference};
    use crate::storage::storable::account_reference_list::StorableAccountReferenceList;
    use crate::storage::storable::application::StorableApplication;
    use crate::storage::{AccountReferenceWrite, StorageError};
    use crate::Storage;
    use ic_stable_structures::VectorMemory;
    use internet_identity_interface::internet_identity::types::{AccountNumber, AnchorNumber};
    use pretty_assertions::assert_eq;
    use std::collections::BTreeMap;

    const ORIGIN: &str = "https://example.com";

    fn storage_with_anchor() -> (Storage<VectorMemory>, AnchorNumber) {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        // A write that moves an account number resyncs the principal index, which
        // needs the salt.
        storage.update_salt([17u8; 32]);
        let anchor = storage.allocate_anchor(0).unwrap();
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap();
        (storage, anchor_number)
    }

    /// Plants the list a future account move would leave behind. The write path cannot
    /// store one, which is the whole point, so a test that
    /// needs a tombstone has to write it directly.
    fn plant_tombstone(storage: &mut Storage<VectorMemory>, anchor_number: AnchorNumber) {
        let origin = ORIGIN.to_string();
        let application_number = application_number_for(storage, &origin);
        storage.stable_account_reference_list_memory.insert(
            (anchor_number, application_number),
            StorableAccountReferenceList::tombstone_for_testing(),
        );
        // The counter goes up with the list, as the move that will one day leave a
        // tombstone behind has to do: a stored tombstone the application does not count
        // is a divergence, and the write path refuses those rather than papering over
        // them.
        let application = storage
            .stable_application_memory
            .get(&application_number)
            .expect("the origin was just interned");
        storage.stable_application_memory.insert(
            application_number,
            StorableApplication {
                stored_tombstones: application.stored_tombstones + 1,
                ..application
            },
        );
        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            Some(vec![])
        );
    }

    /// The account number the default reads back as, or `None` where there is no
    /// default to read. The inner `None` is a default that is still derived rather
    /// than stored, so the two levels have to stay apart.
    fn read_default(
        storage: &Storage<VectorMemory>,
        anchor_number: AnchorNumber,
    ) -> Option<Option<AccountNumber>> {
        let origin = ORIGIN.to_string();
        storage
            .read_account(&AccountKey {
                anchor_number,
                origin: origin.clone(),
                account_number: None,
            })
            .map(|account| account.account_number)
    }

    #[test]
    fn an_untouched_list_offers_a_reconstructible_default() {
        let (storage, anchor_number) = storage_with_anchor();
        let origin = ORIGIN.to_string();

        assert_eq!(read_default(&storage, anchor_number), Some(None));
        let accounts = storage.list_accounts(anchor_number, &origin);
        assert_eq!(accounts.len(), 1);
        assert_eq!(accounts[0].account_number, None);
    }

    #[test]
    fn a_tombstoned_list_has_nothing_to_sign_in_as() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = ORIGIN.to_string();
        plant_tombstone(&mut storage, anchor_number);

        // Every reference moved away, so there is nothing to list and no default to
        // reconstruct — answering with a synthetic one would hand its former owner
        // the same principal back.
        assert!(storage.list_accounts(anchor_number, &origin).is_empty());
        assert_eq!(read_default(&storage, anchor_number), None);
    }

    #[test]
    fn a_list_that_names_no_default_has_no_default_to_read() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = ORIGIN.to_string();
        let account = storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();
        let account_number = account.account_number.unwrap();

        // Drop just the default reference, as moving it away would.
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(
                    &origin,
                    vec![AccountReference::new(Some(account_number), None)],
                    None,
                ),
            )
            .unwrap();

        // No synthetic default here: the identity signs in with the named account.
        assert_eq!(read_default(&storage, anchor_number), None);
        let accounts = storage.list_accounts(anchor_number, &origin);
        assert_eq!(accounts.len(), 1);
        assert_eq!(accounts[0].account_number, Some(account_number));
    }

    #[test]
    fn a_named_account_added_to_a_tombstoned_list_does_not_revive_the_default() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = ORIGIN.to_string();
        plant_tombstone(&mut storage, anchor_number);

        let account = storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();

        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        let references = storage
            .stored_account_references(anchor_number, application_number)
            .expect("the named account should have left references behind");
        assert_eq!(
            references
                .iter()
                .map(|r| r.account_number)
                .collect::<Vec<_>>(),
            vec![account.account_number]
        );
    }

    #[test]
    fn a_default_that_can_no_longer_be_named_is_refused_before_anything_is_written() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = ORIGIN.to_string();
        plant_tombstone(&mut storage, anchor_number);
        let counter_before = storage.get_total_accounts_counter().clone();

        let result = storage.write_account(Account::new(
            anchor_number,
            origin.clone(),
            Some("named default".to_string()),
            None,
        ));

        assert!(matches!(result, Err(StorageError::MissingAccount { .. })));
        // Refused before the allocation, so no account number was spent on a record
        // that no reference would have named.
        assert_eq!(
            storage.get_total_accounts_counter().stored_accounts,
            counter_before.stored_accounts
        );
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            Some(vec![])
        );
        assert_eq!(
            storage
                .lookup_anchor_application_config(anchor_number, application_number)
                .default_account_number,
            None
        );
    }

    #[test]
    fn a_default_naming_an_account_the_list_does_not_hold_moves_to_one_it_does() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = ORIGIN.to_string();
        storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();

        // A number this identity does not hold at this origin, which is what a caller
        // that had gone stale would ask for.
        storage
            .set_default_account(anchor_number, origin.clone(), Some(9_999))
            .unwrap();

        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        let references = storage
            .stored_account_references(anchor_number, application_number)
            .unwrap();
        assert_eq!(
            storage
                .lookup_anchor_application_config(anchor_number, application_number)
                .default_account_number,
            references[0].account_number,
            "the default should have moved to the first reference the list still holds"
        );
    }

    #[test]
    fn dropping_the_reference_a_default_names_moves_the_default() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = ORIGIN.to_string();
        let named = storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();
        let account_number = named.account_number.unwrap();
        storage
            .set_default_account(anchor_number, origin.clone(), Some(account_number))
            .unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        assert_eq!(
            storage
                .lookup_anchor_application_config(anchor_number, application_number)
                .default_account_number,
            Some(account_number)
        );

        // The write says what the identity holds afterwards, and this one no longer holds
        // the account the default names. No caller says anything about the default: the
        // point is that it moves without being told to.
        let anchor = storage.read(anchor_number).unwrap();
        storage
            .write_account_state(
                anchor,
                BTreeMap::from([(
                    origin.clone(),
                    Some((
                        vec![AccountReferenceWrite {
                            account_reference: AccountReference::new(None, None),
                            record: None,
                        }],
                        None,
                    )),
                )]),
            )
            .unwrap();

        assert_eq!(
            storage
                .lookup_anchor_application_config(anchor_number, application_number)
                .default_account_number,
            None,
            "the default should have moved to the tracked default the list still holds"
        );
    }

    #[test]
    fn naming_a_default_keeps_its_reference_in_place() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = ORIGIN.to_string();
        let named = storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();
        let stamped_at = 123456u64;
        let default_key = AccountKey {
            anchor_number,
            origin: origin.clone(),
            account_number: None,
        };
        let mut used_default = storage.read_account(&default_key).unwrap();
        used_default.last_used = Some(stamped_at);
        storage.write_account(used_default).unwrap();

        let mut default_to_name = storage.read_account(&default_key).unwrap();
        default_to_name.name = Some("named default".to_string());
        let default = storage.write_account(default_to_name).unwrap();

        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        let references = storage
            .stored_account_references(anchor_number, application_number)
            .expect("naming the default should not have emptied the list");
        // Repointed where it stood, keeping the order accounts are listed in and the
        // timestamp the reference already carried.
        assert_eq!(
            references.iter().collect::<Vec<_>>(),
            vec![
                &AccountReference::new(default.account_number, Some(stamped_at)),
                &AccountReference::new(named.account_number, None),
            ]
        );
    }

    #[test]
    fn renaming_an_account_writes_only_its_record() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = ORIGIN.to_string();
        let account = storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();
        let account_number = account.account_number.unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        let references_before = storage
            .stored_account_references(anchor_number, application_number)
            .unwrap();

        // A skipped write is invisible in the stored bytes, since rewriting the list
        // would store what it already holds. Retiring the application makes it
        // visible: `write_account_state` refuses without one, so a rename that still
        // wrote the list could not succeed here.
        storage
            .stable_application_memory
            .remove(&application_number);

        let mut account_to_rename = storage
            .read_account(&AccountKey {
                anchor_number,
                origin: origin.clone(),
                account_number: Some(account_number),
            })
            .unwrap();
        account_to_rename.name = Some("renamed".to_string());
        let renamed = storage.write_account(account_to_rename).unwrap();

        assert_eq!(renamed.name, Some("renamed".to_string()));
        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            Some(references_before)
        );
    }

    #[test]
    fn an_identity_holding_no_reference_can_neither_rename_nor_stamp_the_account() {
        let (mut storage, owner) = storage_with_anchor();
        let other = {
            let anchor = storage.allocate_anchor(0).unwrap();
            let anchor_number = anchor.anchor_number();
            storage.write(anchor).unwrap();
            anchor_number
        };
        let origin = ORIGIN.to_string();
        let account = storage
            .create_account(owner, origin.clone(), "named".to_string())
            .unwrap();
        let account_number = account.account_number.unwrap();
        // The other identity has a list of its own at this origin, so what refuses the
        // attempts below is the list not naming this account rather than there being no
        // list to look in.
        storage
            .create_account(other, origin.clone(), "mine".to_string())
            .unwrap();

        let rename = storage.write_account(Account::new(
            other,
            origin.clone(),
            Some("stolen".to_string()),
            Some(account_number),
        ));
        assert!(matches!(rename, Err(StorageError::AccountNotFound { .. })));

        let stamp = storage.write_account(Account::new_with_last_used(
            other,
            origin.clone(),
            None,
            Some(account_number),
            Some(123456),
        ));
        assert!(matches!(stamp, Err(StorageError::AccountNotFound { .. })));

        // The owner's record and reference are untouched by either attempt: recording
        // a use finds no reference to stamp under the other identity.
        let owned = storage
            .read_account(&AccountKey {
                anchor_number: owner,
                origin: origin.clone(),
                account_number: Some(account_number),
            })
            .unwrap();
        assert_eq!(owned.name, Some("named".to_string()));
        assert_eq!(owned.last_used, None);
    }
}

mod application_number_allocator_tests {
    use super::application_number_for;
    use crate::storage::storable::application::StorableApplication;
    use crate::Storage;
    use ic_stable_structures::VectorMemory;
    use pretty_assertions::assert_eq;

    fn application(origin: &str) -> StorableApplication {
        StorableApplication {
            origin: origin.to_string(),
            stored_accounts: 0,
            stored_account_references: 0,
            stored_tombstones: 0,
        }
    }

    #[test]
    fn allocates_dense_numbers_from_zero() {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());

        let first = application_number_for(&mut storage, &"https://a.com".into());
        let second = application_number_for(&mut storage, &"https://b.com".into());
        let third = application_number_for(&mut storage, &"https://c.com".into());

        assert_eq!((first, second, third), (0, 1, 2));
    }

    #[test]
    fn returns_the_existing_number_for_a_known_origin() {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        let origin = "https://a.com".to_string();

        let first = application_number_for(&mut storage, &origin);
        let again = application_number_for(&mut storage, &origin);

        assert_eq!(first, again);
        assert_eq!(storage.get_total_application_count(), 1);
    }

    #[test]
    fn takes_past_applications_written_before_the_allocator_existed_into_account() {
        let memory = VectorMemory::default();
        let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
        for (number, origin) in [
            (0, "https://a.com"),
            (1, "https://b.com"),
            (2, "https://c.com"),
        ] {
            storage
                .stable_application_memory
                .insert(number, application(origin));
        }
        storage.next_application_number_memory.set(0).unwrap();
        storage.flush();

        let mut storage = Storage::from_memory(memory);
        let next = application_number_for(&mut storage, &"https://d.com".into());

        assert_eq!(next, 3);
    }

    #[test]
    fn a_gap_below_the_highest_number_is_not_handed_out_again() {
        let memory = VectorMemory::default();
        let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
        for (number, origin) in [
            (0, "https://a.com"),
            (1, "https://b.com"),
            (2, "https://c.com"),
        ] {
            storage
                .stable_application_memory
                .insert(number, application(origin));
        }
        // The lists now have a hole in them while the counter knows nothing, which is
        // the one state a list count gets wrong: it would answer 2, the number
        // `https://c.com` still holds.
        storage.stable_application_memory.remove(&0);
        storage.next_application_number_memory.set(0).unwrap();
        storage.flush();

        let mut storage = Storage::from_memory(memory);
        let next = application_number_for(&mut storage, &"https://d.com".into());

        assert_eq!(next, 3);
        assert_eq!(
            storage.stable_application_memory.get(&2).unwrap().origin,
            "https://c.com"
        );
    }

    #[test]
    fn never_reissues_the_number_of_a_removed_application() {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        for origin in ["https://a.com", "https://b.com", "https://c.com"] {
            application_number_for(&mut storage, &origin.into());
        }

        storage.stable_application_memory.remove(&1);

        let next = application_number_for(&mut storage, &"https://d.com".into());

        assert_eq!(next, 3);
        assert!(storage.stable_application_memory.get(&2).is_some());
    }

    #[test]
    fn a_removal_does_not_lower_the_allocator_across_an_upgrade() {
        let memory = VectorMemory::default();
        let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
        for origin in [
            "https://a.com",
            "https://b.com",
            "https://c.com",
            "https://d.com",
        ] {
            application_number_for(&mut storage, &origin.into());
        }
        storage.flush();
        storage.stable_application_memory.remove(&1);
        storage.stable_application_memory.remove(&2);
        assert_eq!(storage.stable_application_memory.len(), 2);

        let mut storage = Storage::from_memory(memory.clone());
        let next = application_number_for(&mut storage, &"https://e.com".into());

        assert_eq!(next, 4);
        assert_eq!(
            storage.stable_application_memory.get(&3).unwrap().origin,
            "https://d.com"
        );

        let mut storage = Storage::from_memory(memory);
        assert_eq!(
            application_number_for(&mut storage, &"https://f.com".into()),
            5
        );
    }

    #[test]
    fn a_removal_before_the_first_allocation_does_not_collide_with_a_live_number() {
        let memory = VectorMemory::default();
        let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
        for origin in ["https://a.com", "https://b.com", "https://c.com"] {
            application_number_for(&mut storage, &origin.into());
        }
        storage.next_application_number_memory.set(0).unwrap();
        storage.flush();

        let mut storage = Storage::from_memory(memory);
        storage.stable_application_memory.remove(&0);

        let next = application_number_for(&mut storage, &"https://d.com".into());

        assert_eq!(next, 3);
        assert_eq!(
            storage.stable_application_memory.get(&2).unwrap().origin,
            "https://c.com"
        );
    }
}

mod default_account_tracking_tests {
    use super::application_number_for;
    use super::held_references;
    use super::record_use;
    use super::write_at;
    use crate::storage::account::{AccountKey, AccountReference};
    use crate::Storage;
    use ic_stable_structures::VectorMemory;
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use pretty_assertions::assert_eq;

    fn storage_with_anchor() -> (Storage<VectorMemory>, AnchorNumber) {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        storage.update_salt([17u8; 32]);
        let anchor = storage.allocate_anchor(0).unwrap();
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap();
        (storage, anchor_number)
    }

    #[test]
    fn a_default_is_tracked_at_an_origin_another_identity_registered() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        // Another identity reached this origin first, so the application is known
        // while this identity has no reference list there.
        let application_number = application_number_for(&mut storage, &origin);

        record_use(&mut storage, anchor_number, origin, None, 1_000).unwrap();

        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            Some(vec![AccountReference::new(None, Some(1_000))])
        );
    }

    #[test]
    fn recording_a_named_account_never_creates_a_reference() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let application_number = application_number_for(&mut storage, &origin);

        record_use(&mut storage, anchor_number, origin, Some(7), 1_000).unwrap();

        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            None
        );
    }

    #[test]
    fn tracking_registers_the_application_and_the_reference() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();

        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            Some(vec![AccountReference::new(None, Some(1_000))])
        );
    }

    #[test]
    fn tracking_twice_stamps_rather_than_appends() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        record_use(&mut storage, anchor_number, origin.clone(), None, 2_000).unwrap();

        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        let references = held_references(&storage, anchor_number, application_number);
        assert_eq!(references.len(), 1);
        assert_eq!(references[0].last_used, Some(2_000));
        assert_eq!(
            storage
                .get_account_counter(anchor_number)
                .stored_account_references,
            1
        );
    }

    #[test]
    fn tracking_does_not_recreate_a_default_that_was_given_away() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let application_number = application_number_for(&mut storage, &origin);
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(&origin, vec![AccountReference::new(Some(9), None)], None),
            )
            .unwrap();

        record_use(&mut storage, anchor_number, origin, None, 1_000).unwrap();

        let references = held_references(&storage, anchor_number, application_number);
        assert_eq!(references.len(), 1);
        assert_eq!(references[0].account_number, Some(9));
        assert_eq!(references[0].last_used, None);
    }

    #[test]
    fn creating_a_named_account_leaves_the_default_unused() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();

        let default_account = storage
            .read_account(&AccountKey {
                anchor_number,
                origin: origin.clone(),
                account_number: None,
            })
            .unwrap();
        assert_eq!(default_account.last_used, None);
    }

    #[test]
    fn a_config_list_implies_an_account_reference_list() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let application_number = application_number_for(&mut storage, &origin);

        storage
            .set_default_account(anchor_number, origin.clone(), None)
            .unwrap();

        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            Some(vec![AccountReference::new(None, None)])
        );
    }

    #[test]
    fn choosing_a_default_does_not_disturb_an_existing_reference_list() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let application_number = application_number_for(&mut storage, &origin);
        record_use(&mut storage, anchor_number, origin.clone(), None, 7_000).unwrap();

        storage
            .set_default_account(anchor_number, origin.clone(), None)
            .unwrap();

        let references = held_references(&storage, anchor_number, application_number);
        assert_eq!(references.len(), 1);
        assert_eq!(references[0].last_used, Some(7_000));
    }
}

mod tracked_default_eviction_tests {
    use super::application_number_for;
    use super::held_references;
    use super::record_use;
    use super::remove_at;
    use super::write_at;
    use crate::storage::account::{AccountKey, AccountReference};
    use crate::storage::storable::account_reference_list::StorableAccountReferenceList;
    use crate::storage::storable::accounts_counter::StorableAccountsCounter;
    use crate::storage::storable::application::StorableApplication;
    use crate::storage::{
        EVICTABLE_DEFAULT_ACCOUNTS_WATERMARK, MAX_EVICTABLE_DEFAULT_ACCOUNTS,
        MAX_EVICTIONS_PER_CALL,
    };
    use crate::Storage;
    use ic_stable_structures::VectorMemory;
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use pretty_assertions::assert_eq;

    fn storage_with_anchor() -> (Storage<VectorMemory>, AnchorNumber) {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        storage.update_salt([17u8; 32]);
        let anchor = storage.allocate_anchor(0).unwrap();
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap();
        (storage, anchor_number)
    }

    fn origin_of(index: u64) -> String {
        format!("https://dapp-{index}.com")
    }

    fn sign_in_at(storage: &mut Storage<VectorMemory>, anchor_number: AnchorNumber, index: u64) {
        record_use(storage, anchor_number, origin_of(index), None, index + 1).unwrap();
    }

    #[test]
    fn evicting_drops_the_least_recently_used_down_to_the_watermark() {
        let (mut storage, anchor_number) = storage_with_anchor();

        // One sign-in past the cap. The origin a write is touching is never a candidate for
        // its own eviction, so the pass first runs when the *other* lists reach the cap —
        // one sign-in later than the cap itself.
        let signed_in_at = MAX_EVICTABLE_DEFAULT_ACCOUNTS + 1;
        for index in 0..signed_in_at {
            sign_in_at(&mut storage, anchor_number, index);
        }

        // Down to the watermark, and then the origin that triggered the pass on top of it.
        let evicted = MAX_EVICTABLE_DEFAULT_ACCOUNTS - EVICTABLE_DEFAULT_ACCOUNTS_WATERMARK;
        assert_eq!(
            storage.evictable_default_lists(anchor_number).len() as u64,
            EVICTABLE_DEFAULT_ACCOUNTS_WATERMARK + 1
        );

        for index in 0..evicted {
            assert!(storage
                .lookup_application_number_with_origin(&origin_of(index))
                .is_none());
        }
        for index in evicted..signed_in_at {
            let application_number = storage
                .lookup_application_number_with_origin(&origin_of(index))
                .unwrap();
            assert_ne!(
                storage.stored_account_references(anchor_number, application_number),
                None
            );
        }
    }

    #[test]
    fn choosing_a_default_account_is_capped_too() {
        let (mut storage, anchor_number) = storage_with_anchor();

        for index in 0..MAX_EVICTABLE_DEFAULT_ACCOUNTS * 2 {
            storage
                .set_default_account(anchor_number, origin_of(index), None)
                .unwrap();
        }

        assert!(
            storage.evictable_default_lists(anchor_number).len() as u64
                <= MAX_EVICTABLE_DEFAULT_ACCOUNTS
        );
    }

    #[test]
    fn the_list_a_sign_in_just_wrote_is_never_its_own_victim() {
        let (mut storage, anchor_number) = storage_with_anchor();
        for index in 0..MAX_EVICTABLE_DEFAULT_ACCOUNTS - 1 {
            record_use(&mut storage, anchor_number, origin_of(index), None, 1).unwrap();
        }

        let newest_origin = "https://newest.com".to_string();
        record_use(&mut storage, anchor_number, newest_origin.clone(), None, 1).unwrap();

        let newest_application = storage
            .lookup_application_number_with_origin(&newest_origin)
            .unwrap();
        assert_ne!(
            storage.stored_account_references(anchor_number, newest_application),
            None
        );
    }

    /// Puts an identity over the tracked-default cap without going through the write path,
    /// which is the one thing that can no longer produce this state.
    fn plant_over_the_cap(
        storage: &mut Storage<VectorMemory>,
        anchor_number: AnchorNumber,
        lists: u64,
    ) {
        for index in 0..lists {
            let origin = origin_of(index);
            let application_number = index;
            storage.lookup_application_with_origin_memory.insert(
                crate::storage::StorableOriginSha256::from_origin(&origin),
                application_number,
            );
            // Counters that match what is planted, or removing one under-runs them.
            storage.stable_application_memory.insert(
                application_number,
                StorableApplication {
                    origin,
                    stored_accounts: 0,
                    stored_account_references: 1,
                    stored_tombstones: 0,
                },
            );
            storage.stable_account_reference_list_memory.insert(
                (anchor_number, application_number),
                StorableAccountReferenceList::try_from(vec![AccountReference::new(
                    None,
                    Some(index + 1),
                )])
                .unwrap(),
            );
        }
        storage.set_counters_for_testing(anchor_number, 0, lists);
        storage
            .stable_account_counter_memory
            .set(StorableAccountsCounter {
                stored_accounts: 0,
                stored_account_references: lists,
            })
            .unwrap();
    }

    /// Eviction is a consequence of the write, so it follows any write that leaves the
    /// identity over the cap — not only the one that created a list, which is what used to
    /// carry it.
    #[test]
    fn a_write_to_an_origin_that_already_has_a_list_evicts_too() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let lists = MAX_EVICTABLE_DEFAULT_ACCOUNTS + 100;
        plant_over_the_cap(&mut storage, anchor_number, lists);

        // An origin that already has a list, so this write creates nothing — which is
        // exactly the case the old order skipped the sweep for.
        record_use(
            &mut storage,
            anchor_number,
            origin_of(lists - 1),
            None,
            1_000_000,
        )
        .unwrap();

        assert_eq!(
            lists - storage.evictable_default_lists(anchor_number).len() as u64,
            MAX_EVICTIONS_PER_CALL,
            "a write that created nothing still evicted"
        );
    }

    #[test]
    fn one_call_evicts_at_most_a_bounded_batch() {
        let (mut storage, anchor_number) = storage_with_anchor();
        // Planted rather than written. An identity can no longer *reach* far over the cap
        // through the write path — eviction now runs on the write that would take it
        // there — so this is the state an upgrade leaves behind from before the cap
        // existed, which is the only way the batch bound is still the binding one.
        plant_over_the_cap(
            &mut storage,
            anchor_number,
            MAX_EVICTABLE_DEFAULT_ACCOUNTS * 3,
        );
        let before = storage.evictable_default_lists(anchor_number).len() as u64;

        record_use(
            &mut storage,
            anchor_number,
            "https://trigger.com".to_string(),
            None,
            9_999,
        )
        .unwrap();

        assert_eq!(
            before + 1 - storage.evictable_default_lists(anchor_number).len() as u64,
            MAX_EVICTIONS_PER_CALL
        );
    }

    #[test]
    fn signing_in_never_fails_on_the_cap() {
        let (mut storage, anchor_number) = storage_with_anchor();

        for index in 0..MAX_EVICTABLE_DEFAULT_ACCOUNTS * 2 {
            sign_in_at(&mut storage, anchor_number, index);
        }

        let lists = storage.evictable_default_lists(anchor_number).len() as u64;
        assert!(lists <= MAX_EVICTABLE_DEFAULT_ACCOUNTS);
        let newest = storage
            .lookup_application_number_with_origin(&origin_of(
                MAX_EVICTABLE_DEFAULT_ACCOUNTS * 2 - 1,
            ))
            .unwrap();
        assert_ne!(
            storage.stored_account_references(anchor_number, newest),
            None
        );
    }

    #[test]
    fn a_never_used_default_is_evicted_before_a_used_one() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let never_used_origin = "https://never-used.com".to_string();
        let never_used_application = application_number_for(&mut storage, &never_used_origin);
        storage
            .set_default_account(anchor_number, never_used_origin.clone(), None)
            .unwrap();

        for index in 0..MAX_EVICTABLE_DEFAULT_ACCOUNTS {
            sign_in_at(&mut storage, anchor_number, index);
        }

        assert_eq!(
            storage.stored_account_references(anchor_number, never_used_application),
            None
        );
    }

    #[test]
    fn a_default_sharing_a_list_with_a_named_account_is_not_evictable() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let shared_origin = "https://has-a-named-account.com".to_string();
        storage
            .create_account(anchor_number, shared_origin.clone(), "named".to_string())
            .unwrap();
        let shared_application = storage
            .lookup_application_number_with_origin(&shared_origin)
            .unwrap();

        for index in 0..MAX_EVICTABLE_DEFAULT_ACCOUNTS {
            sign_in_at(&mut storage, anchor_number, index);
        }

        let references = held_references(&storage, anchor_number, shared_application);
        assert_eq!(references.len(), 2);
        assert!(references.iter().any(|r| r.account_number.is_none()));
    }

    #[test]
    fn eviction_removes_the_config_list_and_the_counters_follow() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        storage
            .set_default_account(anchor_number, origin.clone(), None)
            .unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .expect("the writes above created the application");

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert!(storage
            .stable_anchor_application_config_memory
            .get(&(anchor_number, application_number))
            .is_none());
        assert_eq!(
            storage
                .get_account_counter(anchor_number)
                .stored_account_references,
            0
        );
        assert!(storage
            .stable_application_memory
            .get(&application_number)
            .is_none());
        assert_eq!(
            storage
                .get_total_accounts_counter()
                .stored_account_references,
            0
        );
    }

    #[test]
    fn eviction_is_an_exact_round_trip() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let _application_number = application_number_for(&mut storage, &origin);

        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        let before = storage
            .read_account(&AccountKey {
                anchor_number,
                origin: origin.clone(),
                account_number: None,
            })
            .unwrap();

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();
        record_use(&mut storage, anchor_number, origin.clone(), None, 2_000).unwrap();

        let after = storage
            .read_account(&AccountKey {
                anchor_number,
                origin: origin.clone(),
                account_number: None,
            })
            .unwrap();

        assert_eq!(before.anchor_number, after.anchor_number);
        assert_eq!(before.origin, after.origin);
        assert_eq!(before.account_number, None);
        assert_eq!(after.account_number, None);
        assert_eq!(after.last_used, Some(2_000));
    }

    #[test]
    fn removing_a_list_that_does_not_exist_is_a_no_op() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let _application_number = application_number_for(&mut storage, &origin);

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert_eq!(
            storage
                .get_account_counter(anchor_number)
                .stored_account_references,
            0
        );
    }

    #[test]
    fn eviction_never_leaves_an_empty_list_behind() {
        let (mut storage, anchor_number) = storage_with_anchor();

        for index in 0..MAX_EVICTABLE_DEFAULT_ACCOUNTS {
            sign_in_at(&mut storage, anchor_number, index);
        }

        let empty_lists = storage
            .stable_account_reference_list_memory
            .range((anchor_number, 0)..=(anchor_number, u64::MAX))
            .filter(|(_, list)| list.clone().into_vec().is_empty())
            .count();
        assert_eq!(empty_lists, 0);
    }

    #[test]
    fn the_upper_bound_ignores_named_accounts() {
        let (mut storage, anchor_number) = storage_with_anchor();
        for index in 0..3 {
            storage
                .create_account(anchor_number, origin_of(index), format!("named-{index}"))
                .unwrap();
        }

        // The bound eviction triggers on is every account reference that is not a named
        // account, taken from counters rather than by looking at the lists.
        let tracked_defaults = |storage: &Storage<VectorMemory>| {
            let counter = storage.get_account_counter(anchor_number);
            counter.stored_account_references - counter.stored_accounts
        };
        assert_eq!(tracked_defaults(&storage), 3);

        sign_in_at(&mut storage, anchor_number, 100);

        assert_eq!(tracked_defaults(&storage), 4);
        assert_eq!(storage.evictable_default_lists(anchor_number).len(), 1);
    }

    #[test]
    fn eviction_only_touches_the_anchor_that_is_at_the_cap() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let other_anchor = storage.allocate_anchor(0).unwrap();
        let other_anchor_number = other_anchor.anchor_number();
        storage.write(other_anchor).unwrap();
        record_use(&mut storage, other_anchor_number, origin_of(0), None, 1).unwrap();

        for index in 0..MAX_EVICTABLE_DEFAULT_ACCOUNTS {
            sign_in_at(&mut storage, anchor_number, index);
        }

        let application_number = storage
            .lookup_application_number_with_origin(&origin_of(0))
            .unwrap();
        assert_ne!(
            storage.stored_account_references(other_anchor_number, application_number),
            None
        );
    }

    #[test]
    fn a_default_reference_survives_when_only_named_accounts_are_evictable_candidates() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        let _application_number = application_number_for(&mut storage, &origin);
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(&origin, vec![AccountReference::new(Some(1), None)], None),
            )
            .unwrap();

        assert_eq!(storage.evictable_default_lists(anchor_number).len(), 0);
    }
}

mod application_removal_tests {
    use super::application_number_for;
    use super::remove_at;
    use super::write_at;
    use crate::storage::storable::account_reference_list::StorableAccountReferenceList;

    use super::record_use;
    use crate::storage::account::AccountReference;
    use crate::storage::storable::application::{StorableApplication, StorableOriginSha256};
    use crate::Storage;
    use ic_stable_structures::VectorMemory;
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use pretty_assertions::assert_eq;

    fn storage_with_anchors() -> (Storage<VectorMemory>, AnchorNumber, AnchorNumber) {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        storage.update_salt([17u8; 32]);
        let first = storage.allocate_anchor(0).unwrap();
        let first_number = first.anchor_number();
        storage.write(first).unwrap();
        let second = storage.allocate_anchor(0).unwrap();
        let second_number = second.anchor_number();
        storage.write(second).unwrap();
        (storage, first_number, second_number)
    }

    #[test]
    fn the_last_reference_leaving_removes_the_application() {
        let (mut storage, anchor_number, _) = storage_with_anchors();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert!(storage
            .lookup_application_number_with_origin(&origin)
            .is_none());
        assert!(storage
            .stable_application_memory
            .get(&application_number)
            .is_none());
        assert_eq!(storage.get_total_application_count(), 0);
    }

    /// A tombstone holds no reference, so the reference count alone says this origin is
    /// unused. Retiring it would drop the origin index entry, and the next visit would
    /// mint a fresh application number the tombstone no longer applies to — handing the
    /// identity back the default it moved away from, which is the one thing a tombstone
    /// exists to prevent.
    #[test]
    fn an_application_another_anchor_has_tombstoned_is_kept() {
        let (mut storage, anchor_number, other_anchor_number) = storage_with_anchors();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        record_use(
            &mut storage,
            other_anchor_number,
            origin.clone(),
            None,
            1_000,
        )
        .unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        plant_tombstone(&mut storage, other_anchor_number, application_number);

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert_eq!(
            storage.lookup_application_number_with_origin(&origin),
            Some(application_number)
        );
        assert_eq!(
            storage
                .stable_application_memory
                .get(&application_number)
                .map(|application| (
                    application.stored_account_references,
                    application.stored_tombstones
                )),
            Some((0, 1))
        );
    }

    /// The other side of the same rule: an account moved back clears the tombstone, and
    /// with nothing left the application is retired like any other.
    #[test]
    fn an_application_whose_last_tombstone_is_cleared_is_retired() {
        let (mut storage, anchor_number, _) = storage_with_anchors();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        plant_tombstone(&mut storage, anchor_number, application_number);

        // The move back: the tombstoned list gains a reference again.
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(
                    &origin,
                    vec![AccountReference::new(None, Some(2_000))],
                    None,
                ),
            )
            .unwrap();
        assert_eq!(
            storage
                .stable_application_memory
                .get(&application_number)
                .map(|application| application.stored_tombstones),
            Some(0)
        );

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert!(storage
            .lookup_application_number_with_origin(&origin)
            .is_none());
    }

    /// Moves every account out of a list, leaving the tombstone a future account move
    /// will. The write path refuses to store an empty list, which is what makes a
    /// tombstone a thing only a move can create, so it is written here directly — with
    /// the application's counters moved as that move will have to move them.
    fn plant_tombstone(
        storage: &mut Storage<VectorMemory>,
        anchor_number: AnchorNumber,
        application_number: u64,
    ) {
        let moved_away = storage
            .stored_account_references(anchor_number, application_number)
            .expect("a list has to exist before it can be emptied");
        let named = moved_away
            .iter()
            .filter(|reference| reference.account_number.is_some())
            .count() as u64;

        storage.stable_account_reference_list_memory.insert(
            (anchor_number, application_number),
            StorableAccountReferenceList::tombstone_for_testing(),
        );
        let application = storage
            .stable_application_memory
            .get(&application_number)
            .expect("the application should still be stored");
        storage.stable_application_memory.insert(
            application_number,
            StorableApplication {
                stored_accounts: application.stored_accounts - named,
                stored_account_references: application.stored_account_references
                    - moved_away.len() as u64,
                stored_tombstones: application.stored_tombstones + 1,
                ..application
            },
        );
    }

    #[test]
    fn an_application_another_anchor_still_references_is_kept() {
        let (mut storage, anchor_number, other_anchor_number) = storage_with_anchors();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        record_use(
            &mut storage,
            other_anchor_number,
            origin.clone(),
            None,
            2_000,
        )
        .unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert_eq!(
            storage.lookup_application_number_with_origin(&origin),
            Some(application_number)
        );
        assert_eq!(
            storage
                .stable_application_memory
                .get(&application_number)
                .unwrap()
                .stored_account_references,
            1
        );
    }

    #[test]
    fn a_removed_number_is_never_reissued() {
        let (mut storage, anchor_number, _) = storage_with_anchors();
        let removed_origin = "https://removed.com".to_string();
        let kept_origin = "https://kept.com".to_string();
        record_use(
            &mut storage,
            anchor_number,
            removed_origin.clone(),
            None,
            1_000,
        )
        .unwrap();
        record_use(
            &mut storage,
            anchor_number,
            kept_origin.clone(),
            None,
            2_000,
        )
        .unwrap();
        let removed_number = storage
            .lookup_application_number_with_origin(&removed_origin)
            .unwrap();
        let kept_number = storage
            .lookup_application_number_with_origin(&kept_origin)
            .unwrap();

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&removed_origin))
            .unwrap();
        record_use(
            &mut storage,
            anchor_number,
            "https://fresh.com".to_string(),
            None,
            3_000,
        )
        .unwrap();

        let fresh_number = storage
            .lookup_application_number_with_origin(&"https://fresh.com".to_string())
            .unwrap();
        assert_ne!(fresh_number, removed_number);
        assert_ne!(fresh_number, kept_number);
        assert_eq!(
            storage.lookup_application_number_with_origin(&kept_origin),
            Some(kept_number)
        );
    }

    #[test]
    fn a_removed_origin_signed_into_again_gets_a_fresh_application() {
        let (mut storage, anchor_number, _) = storage_with_anchors();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        let first_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        record_use(&mut storage, anchor_number, origin.clone(), None, 2_000).unwrap();

        let second_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        assert_ne!(second_number, first_number);
        assert_ne!(
            storage.stored_account_references(anchor_number, second_number),
            None
        );
    }

    #[test]
    fn an_application_holding_a_named_account_survives_a_default_leaving() {
        let (mut storage, anchor_number, other_anchor_number) = storage_with_anchors();
        let origin = "https://example.com".to_string();
        storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        storage
            .write_account_state_for_testing(
                other_anchor_number,
                write_at(
                    &origin,
                    vec![AccountReference::new(None, Some(1_000))],
                    None,
                ),
            )
            .unwrap();

        storage
            .write_account_state_for_testing(other_anchor_number, remove_at(&origin))
            .unwrap();

        assert_eq!(
            storage.lookup_application_number_with_origin(&origin),
            Some(application_number)
        );
    }

    #[test]
    fn only_a_lone_tracked_default_may_be_pruned() {
        let (mut storage, anchor_number, _) = storage_with_anchors();
        let origin = "https://example.com".to_string();
        // A default alongside a named account. Retiring the list would drop a reference
        // nothing else records, so it is refused even though the caller asked.
        storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        let before = storage
            .stored_account_references(anchor_number, application_number)
            .unwrap();

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            Some(before)
        );
    }

    #[test]
    fn a_tombstone_is_never_pruned() {
        let (mut storage, anchor_number, _) = storage_with_anchors();
        let origin = "https://example.com".to_string();
        let application_number = application_number_for(&mut storage, &origin);
        // Taking the list away would make the moved-away default reconstructible again,
        // which is the one thing the tombstone exists to prevent.
        storage.stable_account_reference_list_memory.insert(
            (anchor_number, application_number),
            StorableAccountReferenceList::tombstone_for_testing(),
        );

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert_eq!(
            storage.stored_account_references(anchor_number, application_number),
            Some(vec![])
        );
    }

    #[test]
    fn removal_leaves_no_config_list_behind() {
        let (mut storage, anchor_number, _) = storage_with_anchors();
        let origin = "https://example.com".to_string();
        // A lone tracked default, which is the only thing a list may be retired for, and
        // the config that goes with it. Nobody else holds this application, so retiring
        // the list retires it too.
        storage
            .set_default_account(anchor_number, origin.clone(), None)
            .unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .expect("the write created the application");
        assert!(storage
            .stable_anchor_application_config_memory
            .get(&(anchor_number, application_number))
            .is_some());

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert!(storage
            .stable_anchor_application_config_memory
            .get(&(anchor_number, application_number))
            .is_none());
        assert!(storage
            .lookup_application_number_with_origin(&origin)
            .is_none());
    }

    #[test]
    fn removal_leaves_an_origin_that_was_reallocated_alone() {
        let (mut storage, anchor_number, _) = storage_with_anchors();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        let reallocated = application_number + 7;
        storage
            .lookup_application_with_origin_memory
            .insert(StorableOriginSha256::from_origin(&origin), reallocated);

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert_eq!(
            storage.lookup_application_number_with_origin(&origin),
            Some(reallocated)
        );
    }
}

mod account_principal_index_tests {
    use super::record_use;
    use super::remove_at;
    use super::write_at;
    use crate::delegation::canister_sig_principal;
    use crate::storage::account::{Account, AccountReference};
    use crate::storage::storable::account_key::StorableAccountKey;
    use crate::storage::{canister_id, StorageError};
    use crate::Storage;
    use candid::Principal;
    use ic_stable_structures::VectorMemory;
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use pretty_assertions::assert_eq;

    const SALT: [u8; 32] = [17u8; 32];

    fn storage_with_anchor() -> (Storage<VectorMemory>, AnchorNumber) {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        storage.update_salt(SALT);
        let anchor = storage.allocate_anchor(0).unwrap();
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap();
        (storage, anchor_number)
    }

    fn default_account_principal(anchor_number: AnchorNumber, origin: &str) -> Principal {
        let account = Account::new(anchor_number, origin.to_string(), None, None);
        canister_sig_principal(
            canister_id(),
            account.calculate_seed_with_salt(&SALT).to_vec(),
        )
    }

    #[test]
    fn tracking_a_default_account_indexes_its_principal() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();

        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        assert_eq!(
            storage
                .lookup_account_with_principal_memory
                .get(&default_account_principal(anchor_number, &origin)),
            Some(StorableAccountKey {
                anchor_number,
                application_number,
                account_number: None,
            })
        );
    }

    #[test]
    fn materializing_a_default_updates_the_locator_under_the_same_principal() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        let principal = default_account_principal(anchor_number, &origin);
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();

        let materialized = storage
            .write_account(Account::new(
                anchor_number,
                origin.clone(),
                Some("named default".to_string()),
                None,
            ))
            .unwrap();

        assert_eq!(
            storage.lookup_account_with_principal_memory.get(&principal),
            Some(StorableAccountKey {
                anchor_number,
                application_number,
                account_number: materialized.account_number,
            })
        );
    }

    #[test]
    fn a_named_account_gets_its_own_entry() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();

        let named = storage
            .create_account(anchor_number, origin.clone(), "named".to_string())
            .unwrap();

        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        let named_principal = canister_sig_principal(
            canister_id(),
            named.calculate_seed_with_salt(&SALT).to_vec(),
        );
        assert_eq!(
            storage
                .lookup_account_with_principal_memory
                .get(&named_principal),
            Some(StorableAccountKey {
                anchor_number,
                application_number,
                account_number: named.account_number,
            })
        );
        assert!(storage
            .lookup_account_with_principal_memory
            .get(&default_account_principal(anchor_number, &origin))
            .is_some());
        assert_ne!(
            named_principal,
            default_account_principal(anchor_number, &origin)
        );
    }

    #[test]
    fn distinct_anchors_and_origins_derive_distinct_principals() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let other = storage.allocate_anchor(0).unwrap();
        let other_anchor_number = other.anchor_number();
        storage.write(other).unwrap();

        record_use(
            &mut storage,
            anchor_number,
            "https://a.com".to_string(),
            None,
            1,
        )
        .unwrap();
        record_use(
            &mut storage,
            anchor_number,
            "https://b.com".to_string(),
            None,
            2,
        )
        .unwrap();
        record_use(
            &mut storage,
            other_anchor_number,
            "https://a.com".to_string(),
            None,
            3,
        )
        .unwrap();

        let same_anchor_other_origin = default_account_principal(anchor_number, "https://b.com");
        let other_anchor_same_origin =
            default_account_principal(other_anchor_number, "https://a.com");
        let base = default_account_principal(anchor_number, "https://a.com");

        assert_ne!(base, same_anchor_other_origin);
        assert_ne!(base, other_anchor_same_origin);
        assert_eq!(
            storage
                .lookup_account_with_principal_memory
                .get(&other_anchor_same_origin)
                .unwrap()
                .anchor_number,
            other_anchor_number
        );
    }

    #[test]
    fn eviction_removes_the_index_entry() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        let principal = default_account_principal(anchor_number, &origin);
        let _application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert_eq!(
            storage.lookup_account_with_principal_memory.get(&principal),
            None
        );
    }

    #[test]
    fn removing_an_application_leaves_no_dangling_index_entries() {
        let (mut storage, anchor_number) = storage_with_anchor();
        for index in 0..5 {
            record_use(
                &mut storage,
                anchor_number,
                format!("https://dapp-{index}.com"),
                None,
                1,
            )
            .unwrap();
        }
        let application_numbers: Vec<_> = (0..5)
            .map(|index| {
                storage
                    .lookup_application_number_with_origin(&format!("https://dapp-{index}.com"))
                    .unwrap()
            })
            .collect();

        // One call holding every victim, the way eviction does it.
        storage
            .write_account_state_for_testing(
                anchor_number,
                (0..5)
                    .map(|index| (format!("https://dapp-{index}.com"), None))
                    .collect(),
            )
            .unwrap();

        assert_eq!(storage.lookup_account_with_principal_memory.len(), 0);
        for application_number in &application_numbers {
            assert!(storage
                .stable_application_memory
                .get(application_number)
                .is_none());
        }
    }

    #[test]
    fn a_write_without_a_salt_is_refused() {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        let anchor = storage.allocate_anchor(0).unwrap();
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap();
        // No salt is set, and nothing arranges one: storing an account reference list
        // derives principals for it, so the write cannot be completed without one.
        let origin = "https://example.com".to_string();

        let result = storage.write_account_state_for_testing(
            anchor_number,
            write_at(&origin, vec![AccountReference::new(Some(1), Some(1))], None),
        );

        assert!(matches!(result, Err(StorageError::SaltNotSet)));
        // Refused before anything was written, application included.
        assert_eq!(storage.lookup_application_number_with_origin(&origin), None);
    }

    #[test]
    fn removing_an_entry_owned_by_another_anchor_is_refused() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        let principal = default_account_principal(anchor_number, &origin);
        let application_number = storage
            .lookup_application_number_with_origin(&origin)
            .unwrap();
        let other_anchor_number = anchor_number + 1;
        storage.lookup_account_with_principal_memory.insert(
            principal,
            StorableAccountKey {
                anchor_number: other_anchor_number,
                application_number,
                account_number: None,
            },
        );

        storage
            .write_account_state_for_testing(anchor_number, remove_at(&origin))
            .unwrap();

        assert_eq!(
            storage
                .lookup_account_with_principal_memory
                .get(&principal)
                .unwrap()
                .anchor_number,
            other_anchor_number
        );
    }

    #[test]
    fn a_principal_resolves_to_the_account_it_was_derived_for() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://example.com".to_string();
        record_use(&mut storage, anchor_number, origin.clone(), None, 1_000).unwrap();
        let principal = default_account_principal(anchor_number, &origin);

        let key = storage.lookup_account_with_principal(principal).unwrap();

        assert_eq!(key.anchor_number, anchor_number);
        assert_eq!(key.origin, origin);
        assert_eq!(key.account_number, None);
    }

    #[test]
    fn a_principal_that_was_never_derived_resolves_to_nothing() {
        let (storage, _) = storage_with_anchor();

        assert_eq!(
            storage.lookup_account_with_principal(Principal::anonymous()),
            None
        );
    }
}

mod session_record_tests {
    use super::record_use;
    use super::{application_number_for, write_at};
    use crate::storage::account::{AccountReference, SessionRecord};
    use crate::storage::storable::account_reference::StorableAccountReference;
    use crate::storage::MAX_EVICTABLE_DEFAULT_ACCOUNTS;
    use crate::{Storage, DAY_NS, MINUTE_NS};
    use ic_stable_structures::{Storable, VectorMemory};
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use pretty_assertions::assert_eq;

    /// A bound so far out that only `valid_till_ns` can end these records, which is
    /// what the tests about the absolute bound want.
    const NEVER_IDLE: u64 = u64::MAX;

    fn session(session_id: u64, created_at_ns: u64, valid_till_ns: u64) -> SessionRecord {
        SessionRecord {
            created_at_ns,
            valid_till_ns,
            max_idle_ns: NEVER_IDLE,
            last_refreshed_ns: None,
            browser_id: 1,
            read_only: false,
            session_id,
        }
    }

    fn storage_with_anchor() -> (Storage<VectorMemory>, AnchorNumber) {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        storage.update_salt([17u8; 32]);
        let anchor = storage.allocate_anchor(0).unwrap();
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap();
        (storage, anchor_number)
    }

    #[test]
    fn a_reference_with_sessions_round_trips() {
        // Every field differs, inside a record and between the two, so a pair of fields
        // swapped on the way through either conversion reads back as a mismatch rather
        // than as a value that happens to match the one it was swapped with.
        let reference = AccountReference {
            account_number: Some(3),
            last_used: Some(9),
            sessions: vec![
                SessionRecord {
                    created_at_ns: 11,
                    valid_till_ns: 22,
                    max_idle_ns: 33,
                    last_refreshed_ns: Some(44),
                    browser_id: 55,
                    read_only: false,
                    session_id: 66,
                },
                SessionRecord {
                    created_at_ns: 77,
                    valid_till_ns: 88,
                    max_idle_ns: 99,
                    last_refreshed_ns: None,
                    browser_id: 111,
                    read_only: true,
                    session_id: 122,
                },
            ],
        };

        let stored = StorableAccountReference::from(reference.clone());
        let decoded =
            AccountReference::from(StorableAccountReference::from_bytes(stored.to_bytes()));

        assert_eq!(decoded, reference);
    }

    #[test]
    fn a_bound_further_out_than_the_session_never_bites() {
        let record = session(1, 0, DAY_NS);

        assert!(!record.is_over(0));
        // Past its own lifetime, so over on the other bound — which is the point:
        // one question, answered by whichever bound is reached first.
        assert!(record.is_over(DAY_NS));
    }

    #[test]
    fn a_session_is_idle_once_nothing_has_minted_for_its_bound() {
        let record = SessionRecord {
            max_idle_ns: 30 * MINUTE_NS,
            last_refreshed_ns: Some(10 * MINUTE_NS),
            ..session(1, 0, DAY_NS)
        };

        assert!(!record.is_over(39 * MINUTE_NS));
        // Still inside its absolute lifetime, and over anyway: either bound ends it.
        assert!(record.is_over(40 * MINUTE_NS));
    }

    #[test]
    fn a_session_that_never_minted_is_measured_from_its_creation() {
        let record = SessionRecord {
            max_idle_ns: 30 * MINUTE_NS,
            last_refreshed_ns: None,
            ..session(1, 5 * MINUTE_NS, DAY_NS)
        };

        // Otherwise a session abandoned straight after sign-in would sit unbounded
        // until its lifetime ran out, which is the case the bound exists for.
        assert!(!record.is_over(34 * MINUTE_NS));
        assert!(record.is_over(35 * MINUTE_NS));
    }

    #[test]
    fn a_reference_written_before_sessions_existed_decodes_with_none() {
        let stored = StorableAccountReference {
            account_number: Some(1),
            last_used: Some(5),
            sessions: None,
        };

        let decoded =
            AccountReference::from(StorableAccountReference::from_bytes(stored.to_bytes()));

        assert_eq!(decoded.sessions, vec![]);
        assert_eq!(decoded.account_number, Some(1));
        assert_eq!(decoded.last_used, Some(5));
    }

    #[test]
    fn an_empty_session_list_is_not_stored() {
        let reference = AccountReference::new(Some(1), None);

        assert_eq!(StorableAccountReference::from(reference).sessions, None);
    }

    /// A list is evictable on its shape alone. Sparing one because it holds a live session
    /// would leave the user with access that settings cannot show them, and a session
    /// nobody can find is a session nobody can revoke.
    #[test]
    fn a_list_holding_a_session_is_evictable_like_any_other() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let origin = "https://has-a-session.com".to_string();
        let _application_number = application_number_for(&mut storage, &origin);
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(
                    &origin,
                    vec![AccountReference {
                        account_number: None,
                        last_used: Some(1),
                        sessions: vec![session(0, 0, u64::MAX)],
                    }],
                    None,
                ),
            )
            .unwrap();

        assert_eq!(storage.evictable_default_lists(anchor_number).len(), 1);
    }

    /// Eviction orders on the list's `last_used`, which every refresh stamps, so a session
    /// in use keeps its list at the newest end and survives the cap on its own.
    #[test]
    fn a_refreshed_session_keeps_its_list_and_a_stale_one_does_not() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let stale = "https://never-came-back.com".to_string();
        let refreshed = "https://still-in-use.com".to_string();
        for (origin, last_used) in [(&stale, 1), (&refreshed, u64::MAX)] {
            storage
                .write_account_state_for_testing(
                    anchor_number,
                    write_at(
                        origin,
                        vec![AccountReference {
                            account_number: None,
                            last_used: Some(last_used),
                            sessions: vec![session(1, 1, u64::MAX)],
                        }],
                        None,
                    ),
                )
                .unwrap();
        }
        let stale_application = storage
            .lookup_application_number_with_origin(&stale)
            .unwrap();
        let refreshed_application = storage
            .lookup_application_number_with_origin(&refreshed)
            .unwrap();

        for index in 0..MAX_EVICTABLE_DEFAULT_ACCOUNTS {
            record_use(
                &mut storage,
                anchor_number,
                format!("https://app-{index}.com"),
                None,
                index + 2,
            )
            .unwrap();
        }

        assert_eq!(
            storage.stored_account_references(anchor_number, stale_application),
            None
        );
        assert_ne!(
            storage.stored_account_references(anchor_number, refreshed_application),
            None
        );
    }

    #[test]
    fn a_session_over_by_idleness_reclaims_like_a_dead_one() {
        let now = 100 * DAY_NS;
        let idle = SessionRecord {
            max_idle_ns: DAY_NS,
            last_refreshed_ns: Some(now - 10 * DAY_NS),
            ..session(1, now - 20 * DAY_NS, now + DAY_NS)
        };
        let live = SessionRecord {
            last_refreshed_ns: Some(now - 1),
            ..session(2, now - 20 * DAY_NS, now + DAY_NS)
        };

        // Both are inside their lifetime, so ranking on that alone would have them
        // compete for a slot. One of them is finished.
        assert!(idle.reclaim_order(now) < live.reclaim_order(now));
    }

    #[test]
    fn reclaim_order_ranks_dead_sessions_first() {
        let now = 1_000;
        let expired = session(1, 1, 500);
        let live = SessionRecord {
            max_idle_ns: NEVER_IDLE,
            last_refreshed_ns: Some(900),
            ..session(2, 400, 10_000)
        };
        let live_untouched = session(3, 400, 10_000);

        assert!(expired.reclaim_order(now) < live.reclaim_order(now));
        assert!(expired.reclaim_order(now) < live_untouched.reclaim_order(now));
    }

    #[test]
    fn a_flood_of_unused_sessions_cannot_displace_a_used_one() {
        let now = 100 * DAY_NS;
        let held = SessionRecord {
            max_idle_ns: NEVER_IDLE,
            last_refreshed_ns: Some(now - DAY_NS),
            ..session(501, now - 20 * DAY_NS, now + DAY_NS)
        };
        // Created after the session it would have to outrank, which under a plain recency
        // order would protect it.
        let flood: Vec<SessionRecord> = (0..500)
            .map(|index| SessionRecord {
                browser_id: index,
                ..session(index as u64 + 1, now - 1, now + DAY_NS)
            })
            .collect();

        assert!(flood
            .iter()
            .all(|session| session.reclaim_order(now) < held.reclaim_order(now)));
    }

    #[test]
    fn an_app_in_weekly_use_outranks_one_opened_once_yesterday() {
        let now = 100 * DAY_NS;
        // Signed in three months ago, still being opened every few days.
        let weekly = SessionRecord {
            max_idle_ns: NEVER_IDLE,
            last_refreshed_ns: Some(now - 3 * DAY_NS),
            ..session(1, now - 90 * DAY_NS, now + DAY_NS)
        };
        // Signed in yesterday, used for five minutes, never opened again.
        let one_sitting = SessionRecord {
            max_idle_ns: NEVER_IDLE,
            last_refreshed_ns: Some(now - DAY_NS + 5 * MINUTE_NS),
            ..session(2, now - DAY_NS, now + DAY_NS)
        };

        assert!(
            one_sitting.reclaim_order(now) < weekly.reclaim_order(now),
            "the more recently touched session goes first, having stayed in service for minutes"
        );
    }
}

mod session_creation_tests {
    use super::application_number_for;
    use super::held_references;
    use super::write_at;
    use super::{params, params_at};
    use crate::delegation::calculate_session_seed_with_salt;
    use crate::storage::account::{
        AccountReference, SessionRecord, DEFAULT_SESSION_IDLE_NS, MIN_SESSION_IDLE_NS,
    };
    use crate::storage::anchor::MAX_BROWSERS;
    use crate::storage::StorageError;
    use crate::storage::{
        CreateSessionParams, MAX_SESSIONS_PER_ANCHOR, SESSIONS_WATERMARK_PER_ANCHOR,
    };
    use crate::{Storage, DAY_NS, MINUTE_NS};
    use ic_stable_structures::VectorMemory;
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use pretty_assertions::assert_eq;

    const SALT: [u8; 32] = [17u8; 32];
    const ORIGIN: &str = "https://example.com";

    fn storage_with_anchor() -> (Storage<VectorMemory>, AnchorNumber) {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        storage.update_salt(SALT);
        let anchor = storage.allocate_anchor(0).unwrap();
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap();
        (storage, anchor_number)
    }

    /// A browser the registry gave up takes its sessions with it, wherever they were, in
    /// the write that made room for the browser replacing it.
    ///
    /// One write, not a loop at the caller: registering the browser, storing the anchor,
    /// then one `revoke_browser_sessions` per browser dropped would end a browser in one
    /// write and its sessions in others — and on the IC an `Err` from a later one commits
    /// the earlier ones, so a browser could be left gone with its sessions still live.
    #[test]
    fn a_dropped_browser_takes_its_sessions_with_it() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let elsewhere = "https://elsewhere.example".to_string();

        // The browser that will be given up, holding a session at each of two origins, so
        // this also shows the sweep is not limited to the origin being written. Its second
        // sign-in presents the successor it announced at its first.
        storage
            .create_session(params(anchor_number, 7, 1_000))
            .unwrap();
        storage
            .create_session(CreateSessionParams {
                origin: elsewhere.clone(),
                ..params_at(anchor_number, 7, 1, 1_000)
            })
            .unwrap();
        assert_eq!(storage.read(anchor_number).unwrap().session_count, 2);

        // Fill the registry, so the browser above is the least recently used when the one
        // that does not fit arrives. Nothing here says which browser is given up, or that
        // its sessions go with it: the write works both out.
        for index in 0..MAX_BROWSERS {
            storage
                .create_session(params(anchor_number, 100 + index as u8, 2_000))
                .unwrap();
        }

        let held: Vec<u32> = storage
            .account_state(anchor_number)
            .into_values()
            .flatten()
            .flat_map(|(account_references, _)| account_references)
            .flat_map(|write| write.account_reference.sessions)
            .map(|session| session.browser_id)
            .collect();
        assert!(
            !held.contains(&0),
            "the dropped browser's sessions outlived it: {held:?}"
        );
        assert_eq!(
            held.len(),
            MAX_BROWSERS,
            "one session each for the browsers still registered"
        );
        assert_eq!(
            storage.read(anchor_number).unwrap().session_count as usize,
            MAX_BROWSERS,
            "and the count followed in the same write"
        );
    }

    /// A list that predates the principal index, which is every list an existing user has:
    /// the index is written only where a list's set of account numbers changes. Emptied
    /// here to stand in for one of those lists.
    fn forget_account_principals(storage: &mut Storage<VectorMemory>) {
        let principals: Vec<_> = storage
            .lookup_account_with_principal_memory
            .iter()
            .map(|(principal, _)| principal)
            .collect();
        for principal in principals {
            storage
                .lookup_account_with_principal_memory
                .remove(&principal);
        }
    }

    /// Creating a session indexes its own account. A session handle names its account by
    /// principal, so a handle whose account is not indexed resolves to nothing — which
    /// would be every returning user, presenting as "sign in again".
    #[test]
    fn creating_a_session_indexes_the_account_it_belongs_to() {
        let (mut storage, anchor_number) = storage_with_anchor();
        storage
            .create_session(params(anchor_number, 1, 1_000))
            .unwrap();
        forget_account_principals(&mut storage);

        storage
            .create_session(params(anchor_number, 2, 2_000))
            .unwrap();

        let application_number = storage
            .lookup_application_number_with_origin(&ORIGIN.to_string())
            .unwrap();
        assert_eq!(
            storage
                .lookup_account_with_principal_memory
                .iter()
                .map(|(_, stored)| (
                    stored.anchor_number,
                    stored.application_number,
                    stored.account_number
                ))
                .collect::<Vec<_>>(),
            vec![(anchor_number, application_number, None)]
        );
    }

    /// Every session the identity holds, wherever it holds it.
    fn all_sessions_of(
        storage: &Storage<VectorMemory>,
        anchor_number: AnchorNumber,
    ) -> Vec<SessionRecord> {
        storage
            .account_state(anchor_number)
            .into_values()
            .flatten()
            .flat_map(|(account_references, _)| account_references)
            .flat_map(|write| write.account_reference.sessions)
            .collect()
    }

    fn sessions_of(
        storage: &Storage<VectorMemory>,
        anchor_number: AnchorNumber,
    ) -> Vec<SessionRecord> {
        let application_number = storage
            .lookup_application_number_with_origin(&ORIGIN.to_string())
            .unwrap();
        held_references(storage, anchor_number, application_number)
            .into_iter()
            .find(|reference| reference.account_number.is_none())
            .unwrap()
            .sessions
    }

    #[test]
    fn an_idle_bound_is_kept_as_asked_for_when_it_is_in_range() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let asked = 20 * MINUTE_NS;

        let session = storage
            .create_session(CreateSessionParams {
                max_idle_ns: Some(asked),
                valid_till_ns: DAY_NS,
                ..params(anchor_number, 1, 0)
            })
            .unwrap()
            .1;

        assert_eq!(session.max_idle_ns, asked);
    }

    #[test]
    fn an_idle_bound_below_the_floor_is_raised_to_it() {
        let (mut storage, anchor_number) = storage_with_anchor();

        let session = storage
            .create_session(CreateSessionParams {
                max_idle_ns: Some(MINUTE_NS),
                valid_till_ns: DAY_NS,
                ..params(anchor_number, 1, 0)
            })
            .unwrap()
            .1;

        // An app delegation lasts five minutes, so a bound under that would end a
        // session between two mints of one that is plainly in use.
        assert_eq!(session.max_idle_ns, MIN_SESSION_IDLE_NS);
    }

    #[test]
    fn an_idle_bound_longer_than_the_session_is_cut_to_it() {
        let (mut storage, anchor_number) = storage_with_anchor();

        let session = storage
            .create_session(CreateSessionParams {
                max_idle_ns: Some(400 * DAY_NS),
                valid_till_ns: DAY_NS,
                ..params(anchor_number, 1, 0)
            })
            .unwrap()
            .1;

        // A bound it could never reach says something about the session that is not
        // true, so it is stored as the life the session actually got.
        assert_eq!(session.max_idle_ns, DAY_NS);
    }

    #[test]
    fn asking_for_no_idle_bound_gets_the_default() {
        let (mut storage, anchor_number) = storage_with_anchor();

        let session = storage
            .create_session(CreateSessionParams {
                valid_till_ns: 30 * DAY_NS,
                ..params(anchor_number, 1, 0)
            })
            .unwrap()
            .1;

        // Every session gets a bound now. A week of nobody touching the application
        // ends the sign-in, well inside the thirty days it could otherwise live.
        assert_eq!(session.max_idle_ns, DEFAULT_SESSION_IDLE_NS);
        assert!(!session.is_over(6 * DAY_NS));
        assert!(session.is_over(7 * DAY_NS));
    }

    #[test]
    fn a_session_shorter_than_the_idle_floor_is_bounded_by_its_own_life() {
        let (mut storage, anchor_number) = storage_with_anchor();

        // Under the floor the range inverts, and clamping in one call would trap.
        let session = storage
            .create_session(CreateSessionParams {
                valid_till_ns: MINUTE_NS,
                max_idle_ns: Some(30 * MINUTE_NS),
                ..params(anchor_number, 1, 0)
            })
            .unwrap()
            .1;

        assert_eq!(session.max_idle_ns, MINUTE_NS);
    }

    #[test]
    fn creating_a_session_tracks_the_account_and_stores_the_record() {
        let (mut storage, anchor_number) = storage_with_anchor();

        let session = storage
            .create_session(params(anchor_number, 1, 1_000))
            .unwrap()
            .1;

        assert_eq!(session.created_at_ns, 1_000);
        assert_eq!(session.valid_till_ns, 11_000);
        assert_eq!(session.last_refreshed_ns, None);
        // The registry minted it; the caller presented a key, not an id.
        assert_eq!(session.browser_id, 0);
        assert_eq!(sessions_of(&storage, anchor_number), vec![session]);
    }

    /// The write path is handed the identity record, so it stores it — including what the
    /// resolution above changed on it, not only the session count it moves itself.
    ///
    /// A browser signing in again at an account it already holds a session at *replaces*
    /// that session, so the count does not move. A write that stored the record only where
    /// the count moved would throw the key rotation away on exactly those sign-ins, leaving
    /// a key that is good for one sign-in usable for good — and the next rotation, which
    /// presents the successor of a key that was never stored, would be refused.
    #[test]
    fn a_rotation_survives_a_sign_in_that_moves_no_session_count() {
        let (mut storage, anchor_number) = storage_with_anchor();

        // Registers the browser. One session added, so the count moves.
        storage
            .create_session(params(anchor_number, 1, 1_000))
            .unwrap();
        // Replaces it: one session out, one in, and the count stays where it was.
        storage
            .create_session(params_at(anchor_number, 1, 1, 2_000))
            .unwrap();

        // Only reachable if the rotation the write above performed was stored.
        let third = storage
            .create_session(params_at(anchor_number, 1, 2, 3_000))
            .expect("the rotation from a count-neutral sign-in was not stored");
        assert_eq!(third.1.browser_id, 0, "still the one registry entry");
        assert_eq!(storage.read(anchor_number).unwrap().browsers().len(), 1);
    }

    /// A session whose life has already run out is refused rather than created, because the
    /// sweep that prunes dead sessions runs in this same call and would take it straight
    /// back out — leaving this returning `Ok` for a session no list holds.
    #[test]
    fn a_session_that_is_already_over_is_refused() {
        let (mut storage, anchor_number) = storage_with_anchor();

        let mut expired = params(anchor_number, 1, 5_000);
        expired.valid_till_ns = 5_000;

        assert!(matches!(
            storage.create_session(expired),
            Err(StorageError::SessionAlreadyOver { .. })
        ));
        assert_eq!(storage.read(anchor_number).unwrap().browsers().len(), 0);
    }

    /// A ceremony replaces the browser's session rather than reusing it, so a copy of the
    /// old one stops working at the user's next sign-in instead of at its expiry.
    #[test]
    fn the_same_device_replaces_its_session() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let first = storage
            .create_session(params(anchor_number, 1, 1_000))
            .unwrap()
            .1;

        let again = storage
            .create_session(params_at(anchor_number, 1, 1, 5_000))
            .unwrap()
            .1;

        assert_eq!(
            again.browser_id, first.browser_id,
            "the same registry entry"
        );
        assert_ne!(again.created_at_ns, first.created_at_ns);
        assert_eq!(sessions_of(&storage, anchor_number).len(), 1);
    }

    #[test]
    fn a_different_device_gets_its_own_session() {
        let (mut storage, anchor_number) = storage_with_anchor();
        storage
            .create_session(params(anchor_number, 1, 1_000))
            .unwrap();

        storage
            .create_session(params(anchor_number, 2, 1_000))
            .unwrap();

        assert_eq!(sessions_of(&storage, anchor_number).len(), 2);
    }

    #[test]
    fn expired_sessions_are_pruned_when_the_list_is_written() {
        let (mut storage, anchor_number) = storage_with_anchor();
        for seed in 0..3 {
            storage
                .create_session(params(anchor_number, seed, 1_000))
                .unwrap();
        }

        let latest = storage
            .create_session(params(anchor_number, 9, 20_000))
            .unwrap()
            .1;

        let sessions = sessions_of(&storage, anchor_number);
        assert_eq!(sessions.len(), 1);
        assert_eq!(sessions[0].browser_id, latest.browser_id);
    }

    /// There is no per-reference cap: one browser holds one session per account, so the
    /// reference is bounded by the browser registry rather than by a number of its own.
    #[test]
    fn one_reference_holds_one_session_per_browser() {
        let (mut storage, anchor_number) = storage_with_anchor();
        for seed in 0..12u8 {
            let mut p = params(anchor_number, seed, 1_000);
            p.valid_till_ns = 1_000_000;
            storage.create_session(p).unwrap();
        }

        let sessions = sessions_of(&storage, anchor_number);
        assert_eq!(sessions.len(), 12);
        assert!(sessions.iter().any(|s| s.browser_id == 0));
    }

    /// The per-identity cap reclaims to a watermark rather than blocking, taking expired
    /// records first and then the least recently used.
    #[test]
    fn an_over_counting_anchor_is_corrected_rather_than_denied() {
        let (mut storage, anchor_number) = storage_with_anchor();
        storage
            .create_session(params(anchor_number, 1, 1_000))
            .unwrap();

        // Nothing observes a session expiring, so the count drifts up. The cap must be
        // enforced against what the lists hold, not against the drift.
        let mut anchor = storage.read(anchor_number).unwrap();
        anchor.session_count = MAX_SESSIONS_PER_ANCHOR;
        storage.write(anchor).unwrap();

        storage
            .create_session(params(anchor_number, 2, 2_000))
            .unwrap();

        assert_eq!(sessions_of(&storage, anchor_number).len(), 2);
        assert_eq!(storage.read(anchor_number).unwrap().session_count, 2);
    }

    #[test]
    fn the_cap_is_never_exceeded_however_many_sign_ins_arrive() {
        let (mut storage, anchor_number) = storage_with_anchor();

        // One browser, rotating as it goes, signing in at a fresh app each time: the
        // session cap counts sessions, and one browser holds one per account, so reaching
        // it means many origins rather than many browsers.
        for sign_in in 0..(MAX_SESSIONS_PER_ANCHOR + 120) {
            let mut params = params_at(anchor_number, 1, sign_in as u16, 600_000 + sign_in as u64);
            params.origin = format!("https://app-{sign_in}.example.com");
            params.valid_till_ns = 100_000_000;
            storage.create_session(params).unwrap();

            let stored = all_sessions_of(&storage, anchor_number).len();
            assert!(
                stored <= MAX_SESSIONS_PER_ANCHOR as usize,
                "{stored} stored after {sign_in} sign-ins"
            );
            assert_eq!(
                storage.read(anchor_number).unwrap().session_count as usize,
                stored,
                "the counter parted ways with the lists after {sign_in} sign-ins"
            );
        }
    }

    #[test]
    fn the_session_cap_reclaims_to_the_watermark() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let _session = storage
            .create_session(params(anchor_number, 7, 1_000))
            .unwrap()
            .1;
        let _application_number = storage
            .lookup_application_number_with_origin(&ORIGIN.to_string())
            .unwrap();

        // Nothing observes a session expiring, so the count drifts up. The cap must be
        // enforced against what the lists hold, not against the drift.
        let mut anchor = storage.read(anchor_number).unwrap();
        anchor.session_count = MAX_SESSIONS_PER_ANCHOR;
        storage.write(anchor).unwrap();

        storage
            .create_session(params(anchor_number, 2, 2_000))
            .unwrap();

        assert_eq!(sessions_of(&storage, anchor_number).len(), 2);
        assert_eq!(storage.read(anchor_number).unwrap().session_count, 2);
    }

    /// Two lists, both holding a default account, and both holding sessions for the same
    /// browser ids. Reclaiming must take only the sessions it selected.
    #[test]
    fn reclaiming_spans_every_list_and_takes_the_expired_ones_first() {
        const OTHER_ORIGIN: &str = "https://other.example";
        let (mut storage, anchor_number) = storage_with_anchor();

        // Two lists that put the identity exactly at the cap, so the next sign-in has to
        // make room. Each holds one expired session, which is what a sweep gives up first.
        //
        // The ids here sit above anything the registry mints, so the browser that signs in
        // below is told apart from the ones these lists stand for.
        const FABRICATED: u32 = 1_000;
        const PER_LIST: u32 = MAX_SESSIONS_PER_ANCHOR / 2;
        let list = |id_base: u64, expired_device: u32| -> Vec<AccountReference> {
            let sessions = (0..PER_LIST)
                .map(|browser_id| browser_id + FABRICATED)
                .map(|browser_id| SessionRecord {
                    created_at_ns: 1,
                    // The expired one, and the live ones ordered so the highest browser id
                    // is the freshest and so the last to be given up.
                    valid_till_ns: if browser_id == expired_device + FABRICATED {
                        2
                    } else {
                        100_000_000
                    },
                    max_idle_ns: u64::MAX,
                    last_refreshed_ns: Some(500_000 + browser_id as u64),
                    browser_id,
                    read_only: false,
                    session_id: id_base + browser_id as u64,
                })
                .collect();
            vec![AccountReference {
                account_number: None,
                last_used: Some(1),
                sessions,
            }]
        };

        for (origin, id_base, expired) in [
            (ORIGIN.to_string(), 1_000, 0),
            (OTHER_ORIGIN.to_string(), 2_000, 1),
        ] {
            storage
                .write_account_state_for_testing(
                    anchor_number,
                    write_at(&origin, list(id_base, expired), None),
                )
                .unwrap();
        }

        let mut params = params(anchor_number, 199, 600_000);
        params.valid_till_ns = 100_000_000;
        storage.create_session(params).unwrap();

        let held = |origin: &str| -> Vec<u32> {
            let application_number = storage
                .lookup_application_number_with_origin(&origin.to_string())
                .unwrap();
            let mut ids: Vec<u32> = held_references(&storage, anchor_number, application_number)
                .into_iter()
                .flat_map(|reference| reference.sessions)
                .map(|session| session.browser_id)
                .collect();
            ids.sort_unstable();
            ids
        };
        let first_devices = held(ORIGIN);
        let second_devices = held(OTHER_ORIGIN);

        // The sweep reaches both lists, not only the one being written.
        assert!(
            !first_devices.contains(&FABRICATED),
            "the expired session in the first list should be gone"
        );
        assert!(
            !second_devices.contains(&(FABRICATED + 1)),
            "the expired session in the second list should be gone"
        );
        // And it gives up the least used, so the freshest in each list is still there.
        assert!(first_devices.contains(&(FABRICATED + PER_LIST - 1)));
        assert!(second_devices.contains(&(FABRICATED + PER_LIST - 1)));
        assert_eq!(
            first_devices.len() + second_devices.len(),
            SESSIONS_WATERMARK_PER_ANCHOR as usize + 1,
            "cleared to the watermark across both lists, then the session it made room for"
        );
    }

    #[test]
    fn a_flood_of_unused_sessions_cannot_displace_a_used_one() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let _application_number = application_number_for(&mut storage, &ORIGIN.to_string());

        let mut sessions = vec![SessionRecord {
            created_at_ns: 1_000,
            valid_till_ns: 100_000_000,
            max_idle_ns: u64::MAX,
            last_refreshed_ns: Some(400_000),
            browser_id: 1,
            read_only: false,
            session_id: 1,
        }];
        sessions.extend(
            (2..=MAX_SESSIONS_PER_ANCHOR).map(|browser_id| SessionRecord {
                created_at_ns: 500_000,
                valid_till_ns: 100_000_000,
                max_idle_ns: u64::MAX,
                last_refreshed_ns: None,
                browser_id,
                read_only: false,
                session_id: browser_id as u64,
            }),
        );
        storage
            .write_account_state_for_testing(
                anchor_number,
                write_at(
                    &ORIGIN.to_string(),
                    vec![AccountReference {
                        account_number: None,
                        last_used: Some(1),
                        sessions,
                    }],
                    None,
                ),
            )
            .unwrap();
        let mut anchor = storage.read(anchor_number).unwrap();
        anchor.session_count = MAX_SESSIONS_PER_ANCHOR;
        storage.write(anchor).unwrap();

        let mut params = params(anchor_number, 199, 600_000);
        params.valid_till_ns = 100_000_000;
        storage.create_session(params).unwrap();

        let remaining = sessions_of(&storage, anchor_number);
        assert!(
            remaining.iter().any(|session| session.browser_id == 1),
            "the session that was kept alive was reclaimed"
        );
        assert!(
            remaining.len() < MAX_SESSIONS_PER_ANCHOR as usize,
            "nothing was reclaimed, so the test proves nothing"
        );
    }

    #[test]
    fn a_named_account_can_hold_its_own_sessions() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let named = storage
            .create_account(anchor_number, ORIGIN.to_string(), "named".to_string())
            .unwrap();
        let mut p = params(anchor_number, 1, 1_000);
        p.account_number = named.account_number;

        storage.create_session(p).unwrap();

        assert_eq!(sessions_of(&storage, anchor_number).len(), 0);
        let application_number = storage
            .lookup_application_number_with_origin(&ORIGIN.to_string())
            .unwrap();
        let references = held_references(&storage, anchor_number, application_number);
        let named_reference = references
            .iter()
            .find(|r| r.account_number == named.account_number)
            .unwrap();
        assert_eq!(named_reference.sessions.len(), 1);
    }

    #[test]
    fn a_session_for_an_account_the_anchor_does_not_hold_is_refused() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let mut p = params(anchor_number, 1, 1_000);
        p.account_number = Some(4_242);

        let result = storage.create_session(p);

        assert!(result.is_err());
    }

    /// The hazard the session id exists for: `time()` is constant across a consensus
    /// round, so two records created in one round agree on every field that describes
    /// them. If identity came from those fields, the second would sign as the first —
    /// and a chain issued against a session that has since been replaced would verify
    /// again.
    #[test]
    fn a_session_replaced_in_the_same_round_does_not_inherit_its_identity() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let same_round = |seed, generation: u16| CreateSessionParams {
            valid_till_ns: 10_000,
            ..params_at(anchor_number, seed, generation, 1_000)
        };

        let first = storage.create_session(same_round(1, 0)).unwrap().1;
        // The same browser again, presenting the successor it announced a moment ago.
        let replacement = storage.create_session(same_round(1, 1)).unwrap().1;
        let sibling = storage.create_session(same_round(2, 0)).unwrap().1;

        assert_eq!(first.created_at_ns, replacement.created_at_ns);
        assert_eq!(first.browser_id, replacement.browser_id);
        assert_ne!(first.session_id, replacement.session_id);
        assert_ne!(replacement.session_id, sibling.session_id);
    }

    /// Creating twice from one browser at one account replaces, so there is never a second
    /// record to collide with in the same round.
    #[test]
    fn creating_twice_in_one_round_from_one_browser_yields_one_session() {
        let (mut storage, anchor_number) = storage_with_anchor();
        // One browser, rotating as it must, signing in three times in the same round.
        let attempt = |generation: u16, read_only| CreateSessionParams {
            valid_till_ns: u64::MAX,
            read_only,
            ..params_at(anchor_number, 1, generation, 1_000)
        };

        let first = storage.create_session(attempt(0, false)).unwrap().1;
        storage.create_session(attempt(1, false)).unwrap();
        assert_eq!(sessions_of(&storage, anchor_number).len(), 1);

        let replaced = storage.create_session(attempt(2, true)).unwrap().1;
        assert_ne!(replaced.read_only, first.read_only);
        assert_eq!(sessions_of(&storage, anchor_number).len(), 1);
    }

    #[test]
    fn the_session_seed_binds_the_account_and_the_session_id() {
        use crate::storage::account::Account;

        let account = Account::new(10_000, ORIGIN.to_string(), None, None);
        let account_seed = account.calculate_seed_with_salt(&SALT);
        let other_account = Account::new(10_001, ORIGIN.to_string(), None, None);
        let other_seed = other_account.calculate_seed_with_salt(&SALT);

        let base = calculate_session_seed_with_salt(&SALT, &account_seed, 1);

        assert_ne!(
            base,
            calculate_session_seed_with_salt(&SALT, &other_seed, 1)
        );
        assert_ne!(
            base,
            calculate_session_seed_with_salt(&SALT, &account_seed, 2)
        );
        assert_ne!(
            base,
            calculate_session_seed_with_salt(&[18u8; 32], &account_seed, 1)
        );
        assert_eq!(
            base,
            calculate_session_seed_with_salt(&SALT, &account_seed, 1)
        );
    }

    #[test]
    fn a_session_seed_is_distinct_from_the_account_seed_it_belongs_to() {
        use crate::storage::account::Account;

        let account = Account::new(10_000, ORIGIN.to_string(), None, None);
        let account_seed = account.calculate_seed_with_salt(&SALT);
        let session_seed = calculate_session_seed_with_salt(&SALT, &account_seed, 1);

        assert_ne!(account_seed, session_seed);
    }

    /// Naming a default account keeps its principal, so it must keep its sessions too.
    #[test]
    fn naming_a_default_account_leaves_its_session_identity_unchanged() {
        use crate::storage::account::Account;

        let default = Account::new(10_000, ORIGIN.to_string(), None, None);
        let before =
            calculate_session_seed_with_salt(&SALT, &default.calculate_seed_with_salt(&SALT), 1);

        let named = Account::new_full(
            10_000,
            ORIGIN.to_string(),
            Some("work".to_string()),
            Some(7),
            None,
            Some(10_000),
        );
        let after =
            calculate_session_seed_with_salt(&SALT, &named.calculate_seed_with_salt(&SALT), 1);

        assert_eq!(before, after);
    }
}

mod session_consent_change_tests {
    use super::held_references;
    use super::params_at;
    use crate::storage::CreateSessionParams;
    use crate::Storage;
    use ic_stable_structures::VectorMemory;
    use internet_identity_interface::internet_identity::types::AnchorNumber;
    use pretty_assertions::assert_eq;

    const ORIGIN: &str = "https://example.com";

    fn storage_with_anchor() -> (Storage<VectorMemory>, AnchorNumber) {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        storage.update_salt([17u8; 32]);
        let anchor = storage.allocate_anchor(0).unwrap();
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap();
        (storage, anchor_number)
    }

    /// One browser signing in again, presenting the successor it announced last time.
    fn create(
        storage: &mut Storage<VectorMemory>,
        anchor_number: AnchorNumber,
        generation: u16,
        read_only: bool,
        now: u64,
    ) -> u64 {
        storage
            .create_session(CreateSessionParams {
                valid_till_ns: u64::MAX,
                read_only,
                ..params_at(anchor_number, 1, generation, now)
            })
            .unwrap()
            .1
            .created_at_ns
    }

    fn sessions(storage: &Storage<VectorMemory>, anchor_number: AnchorNumber) -> Vec<bool> {
        let application_number = storage
            .lookup_application_number_with_origin(&ORIGIN.to_string())
            .unwrap();
        held_references(storage, anchor_number, application_number)
            .into_iter()
            .find(|reference| reference.account_number.is_none())
            .unwrap()
            .sessions
            .into_iter()
            .map(|session| session.read_only)
            .collect()
    }

    #[test]
    fn the_same_consent_still_replaces_the_session() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let first = create(&mut storage, anchor_number, 0, false, 1_000);

        let again = create(&mut storage, anchor_number, 1, false, 2_000);

        assert_ne!(again, first);
        assert_eq!(sessions(&storage, anchor_number), vec![false]);
    }

    #[test]
    fn a_downgraded_consent_replaces_the_session() {
        let (mut storage, anchor_number) = storage_with_anchor();
        let full_access = create(&mut storage, anchor_number, 0, false, 1_000);

        let read_only = create(&mut storage, anchor_number, 1, true, 2_000);

        assert_ne!(read_only, full_access);
        assert_eq!(sessions(&storage, anchor_number), vec![true]);
    }

    #[test]
    fn an_upgraded_consent_replaces_the_session() {
        let (mut storage, anchor_number) = storage_with_anchor();
        create(&mut storage, anchor_number, 0, true, 1_000);

        create(&mut storage, anchor_number, 1, false, 2_000);

        assert_eq!(sessions(&storage, anchor_number), vec![false]);
    }

    #[test]
    fn a_consent_change_leaves_another_browser_alone() {
        let (mut storage, anchor_number) = storage_with_anchor();
        storage
            .create_session(CreateSessionParams {
                valid_till_ns: u64::MAX,
                ..params_at(anchor_number, 2, 0, 1_000)
            })
            .unwrap();
        create(&mut storage, anchor_number, 0, false, 1_000);

        create(&mut storage, anchor_number, 1, true, 2_000);

        let mut held = sessions(&storage, anchor_number);
        held.sort_unstable();
        assert_eq!(held, vec![false, true]);
    }
}

mod browser_session_count_tests {
    use super::{params, params_at};
    use crate::storage::anchor::MAX_BROWSERS;
    use crate::storage::CreateSessionParams;
    use crate::Storage;
    use ic_stable_structures::VectorMemory;
    use internet_identity_interface::internet_identity::types::{AnchorNumber, BrowserId};
    use pretty_assertions::assert_eq;
    use std::collections::BTreeMap;

    const SALT: [u8; 32] = [17u8; 32];

    fn storage_with_anchor() -> (Storage<VectorMemory>, AnchorNumber) {
        let mut storage = Storage::new((10_000, 3_784_873), VectorMemory::default());
        storage.update_salt(SALT);
        let anchor = storage.allocate_anchor(0).unwrap();
        let anchor_number = anchor.anchor_number();
        storage.write(anchor).unwrap();
        (storage, anchor_number)
    }

    /// What each registered browser says it holds, and what the identity says it holds.
    /// Asserted together throughout: the per-browser counts are the same fact as the
    /// identity's total at a finer grain, and a test that checked one without the other
    /// would pass while they disagreed.
    fn counts(
        storage: &Storage<VectorMemory>,
        anchor_number: AnchorNumber,
    ) -> (BTreeMap<BrowserId, u32>, u32) {
        let anchor = storage.read(anchor_number).unwrap();
        let per_browser = anchor
            .browsers()
            .iter()
            .map(|browser| (browser.id, browser.session_count))
            .collect();
        (per_browser, anchor.session_count)
    }

    #[test]
    fn a_session_counts_against_the_browser_it_came_from() {
        let (mut storage, anchor_number) = storage_with_anchor();

        storage
            .create_session(params(anchor_number, 7, 1_000))
            .unwrap();
        assert_eq!(
            counts(&storage, anchor_number),
            (BTreeMap::from([(0, 1)]), 1)
        );

        // A second browser, which is a second entry rather than a second session on the
        // first.
        storage
            .create_session(params(anchor_number, 9, 2_000))
            .unwrap();
        assert_eq!(
            counts(&storage, anchor_number),
            (BTreeMap::from([(0, 1), (1, 1)]), 2)
        );

        // The first browser signing in again at the same origin, presenting the successor
        // it announced. A ceremony replaces what that browser held there rather than
        // adding to it, so the counts stand still — which is the case a counter kept by
        // incrementing at the call site would get wrong.
        storage
            .create_session(params_at(anchor_number, 7, 1, 3_000))
            .unwrap();
        assert_eq!(
            counts(&storage, anchor_number),
            (BTreeMap::from([(0, 1), (1, 1)]), 2)
        );
    }

    #[test]
    fn sessions_at_several_origins_add_up_on_one_browser() {
        let (mut storage, anchor_number) = storage_with_anchor();

        storage
            .create_session(params(anchor_number, 7, 1_000))
            .unwrap();
        storage
            .create_session(CreateSessionParams {
                origin: "https://elsewhere.example".to_string(),
                ..params_at(anchor_number, 7, 1, 2_000)
            })
            .unwrap();

        // One entry, two origins: the count belongs to the browser, not to a list.
        assert_eq!(
            counts(&storage, anchor_number),
            (BTreeMap::from([(0, 2)]), 2)
        );
    }

    #[test]
    fn revoking_a_browsers_sessions_returns_its_count_to_zero() {
        let (mut storage, anchor_number) = storage_with_anchor();

        storage
            .create_session(params(anchor_number, 7, 1_000))
            .unwrap();
        storage
            .create_session(CreateSessionParams {
                origin: "https://elsewhere.example".to_string(),
                ..params_at(anchor_number, 7, 1, 2_000)
            })
            .unwrap();
        storage
            .create_session(params(anchor_number, 9, 3_000))
            .unwrap();

        assert_eq!(
            storage.revoke_browser_sessions(anchor_number, 0).unwrap(),
            2
        );

        // Zero is the whole point of the counter: it is what the settings page reads to
        // say a browser is signed in to nothing, and it survives a reload because it was
        // stored here rather than remembered by the page.
        assert_eq!(
            counts(&storage, anchor_number),
            (BTreeMap::from([(0, 0), (1, 1)]), 1)
        );
    }

    #[test]
    fn a_browser_the_registry_gave_up_leaves_no_count_behind() {
        let (mut storage, anchor_number) = storage_with_anchor();

        // Two sessions on the browser that will be given up, so a count that outlived its
        // entry would be visible rather than indistinguishable from a fresh one.
        storage
            .create_session(params(anchor_number, 7, 1_000))
            .unwrap();
        storage
            .create_session(params_at(anchor_number, 7, 1, 1_000))
            .unwrap();

        for index in 0..MAX_BROWSERS {
            storage
                .create_session(params(anchor_number, 100 + index as u8, 2_000))
                .unwrap();
        }

        let (per_browser, total) = counts(&storage, anchor_number);
        assert!(
            !per_browser.contains_key(&0),
            "the dropped browser is still counted: {per_browser:?}"
        );
        assert_eq!(per_browser.len(), MAX_BROWSERS);
        assert!(
            per_browser.values().all(|count| *count == 1),
            "one session each for the browsers still registered: {per_browser:?}"
        );
        assert_eq!(total as usize, MAX_BROWSERS);
    }
}
