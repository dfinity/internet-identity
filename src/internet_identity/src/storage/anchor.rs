use crate::email_inbound::MAX_VERIFIED_EMAILS_PER_ANCHOR;
use crate::ii_domain::IIDomain;
use crate::notifications::app::{NotificationAppId, NotificationAppRef};
use crate::openid::{Domain, OpenIdCredential, OpenIdCredentialKey};
use crate::storage::storable::anchor::StorableAnchor;
use crate::storage::storable::browser::StorableBrowser;
use crate::storage::storable::browser_description::StorableBrowserDescription;
use crate::storage::storable::email_recovery_credential::StorableEmailRecoveryCredential;
use crate::storage::storable::fixed_anchor::StorableFixedAnchor;
use crate::storage::storable::notification_app::StorableNotificationAppRef;
use crate::storage::storable::notifications::browser_queue::{
    StorableNotificationEvent, StorableQueuedNotification,
};
use crate::storage::storable::notifications::webpush::jwt_pool::StorableWebPushJwtPool;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
use crate::storage::storable::passkey_credential::StorablePasskeyCredential;
use crate::storage::storable::recovery_key::StorableRecoveryKey;
use crate::storage::storable::special_device_migration::SpecialDeviceMigration;
use crate::storage::storable::verified_email::StorableVerifiedEmail;
use crate::{IC0_APP_ORIGIN, ID_AI_ORIGIN, INTERNETCOMPUTER_ORG_ORIGIN};
use candid::{CandidType, Deserialize, Principal};
use internet_identity_interface::archive::types::DeviceDataWithoutAlias;
use internet_identity_interface::internet_identity::types::email_recovery::EmailRecoveryCredential;
use internet_identity_interface::internet_identity::types::openid::OpenIdCredentialData;
use internet_identity_interface::internet_identity::types::verified_email::VerifiedEmail;
use internet_identity_interface::internet_identity::types::*;
use serde_bytes::ByteBuf;
use std::collections::{BTreeMap, HashMap};
use std::fmt;

#[cfg(test)]
mod tests;

/// Internal representation of the anchor.
/// The anchor has limited visibility for the constructor to make sure it is loaded from storage.
/// The devices can only be modified by the exposed functions which keeps invariant checking local
/// to this module.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Anchor {
    pub(crate) anchor_number: AnchorNumber,
    pub(crate) devices: Vec<Device>,
    pub(crate) openid_credentials: Vec<OpenIdCredential>,
    /// Bound recovery emails (see `docs/ongoing/email-recovery.md`).
    /// The current canister API enforces at most one entry; the field
    /// is a `Vec` so the data model can carry multiple in the future
    /// without another schema bump.
    pub(crate) email_recovery: Vec<EmailRecoveryCredential>,
    /// Capped by `MAX_VERIFIED_EMAILS_PER_ANCHOR`.
    pub(crate) verified_emails: Vec<VerifiedEmail>,
    /// Capped by `MAX_BROWSERS`.
    pub(crate) browsers: Vec<Browser>,
    pub(crate) next_browser_id: BrowserId,
    pub(crate) session_count: u32,
    pub(crate) metadata: Option<HashMap<String, MetadataEntry>>,
    pub(crate) name: Option<String>,
    pub(crate) created_at: Option<Timestamp>,
}

/// Bounds the device list, which rides on the anchor blob.
pub const MAX_BROWSERS: usize = 20;

/// Why a browser's presented keys cannot be resolved.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BrowserError {
    /// The announced successor is a key another browser of this anchor already holds.
    ///
    /// Presented keys are visible on the wire, so without this a caller could announce a
    /// key another browser is about to present and take over its entry when it does.
    SuccessorAlreadyInUse,
    /// The announced successor is the key being presented.
    ///
    /// Rotation is what stops a leaked key from being useful for longer than one sign-in,
    /// so a browser that named itself its own successor would keep the key alive for as
    /// long as it kept asking — and whoever leaked it would too.
    SuccessorMatchesCurrent,
    /// The presented key is one this anchor has already retired: an entry holds it as the
    /// key it was last proven with, and now awaits that entry's successor.
    ///
    /// A browser reaches this only when it never learned that its last sign-in succeeded,
    /// so it is still proving with the key it announced a successor for. The answer is for
    /// the browser to promote its own successor and present that — it is the only party
    /// holding both keys. Registering it as a new browser instead would turn every dropped
    /// response into a second list for one browser, and accepting it would leave a leaked
    /// key useful for longer than the one sign-in rotation allows it.
    StaleBrowserKey,
}

/// A browser this anchor has signed in from, as it described itself when it registered.
///
/// The description is fixed at registration: a sign-in that reports something else is
/// treated as a browser this anchor has not seen, and the client is the party that
/// decides so by presenting a key pair no entry holds.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Browser {
    pub id: BrowserId,
    /// The browser's own public key, DER-encoded. What the entry is looked up by.
    pub current_browser_key: PublicKey,
    /// The successor the browser announced at its last sign-in, also accepted as a proof.
    pub next_browser_key: PublicKey,
    /// What this browser reported about itself when it registered.
    pub description: BrowserDescription,
    pub created_at: Timestamp,
    pub last_used: Timestamp,
    /// Sessions this browser holds. Maintained by the write that changes the reference
    /// lists holding them, so it counts stored records rather than live ones.
    pub session_count: u32,
    /// What this browser registered for Web Push, if anything.
    pub webpush_subscription: Option<WebPushSubscription>,
    /// What this identity's service worker has yet to show on this device, oldest first.
    ///
    /// Held here rather than on the subscription, because the address may be the app's
    /// and shared with every other identity pointing at it, while what is waiting is
    /// this identity's alone.
    pub notifications: Vec<QueuedNotification>,
    /// When this entry was last woken again for something it could not show yet.
    pub woken_again_at_ns: Option<Timestamp>,
    /// The probe this entry last sent to the app it points at, which the install page
    /// watches for leaving the queue.
    pub pending_probe: Option<NotificationId>,
    /// The Home Screen app this browser installed or claimed, where it has one.
    ///
    /// A reference rather than an entry of its own: one installed app is one record
    /// however many identities point at it, and it is not a browser, so it holds no
    /// session, is signed out of nothing, and costs the user no slot in this list. The
    /// reference outlives signing out and turning notifications off, both of which stop
    /// routing without forgetting which app this browser has.
    pub notification_app: Option<NotificationAppRef>,
}

/// A notification a browser's service worker has yet to take.
///
/// Sent by an app, which the service worker fetches the text from, or by Internet
/// Identity itself, which has no app behind it and carries what it has to say.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueuedNotification {
    /// `None` where Internet Identity sent this itself.
    pub application_number: Option<ApplicationNumber>,
    /// The canister that sent it, which for an app's notification is what the service
    /// worker fetches the text from.
    pub sender: Principal,
    pub notification_id: NotificationId,
    pub expires_at_ns: Timestamp,
    /// `None` is the unreserved default account, and is always `None` for Internet
    /// Identity's own.
    pub account_number: Option<AccountNumber>,
    /// Set where Internet Identity sent this itself, and what it has to say.
    pub event: Option<NotificationEvent>,
}

impl From<QueuedNotification> for StorableQueuedNotification {
    fn from(value: QueuedNotification) -> Self {
        StorableQueuedNotification {
            application_number: value.application_number,
            event: value.event.map(StorableNotificationEvent::from),
            sender: value.sender.as_slice().to_vec(),
            notification_id: value.notification_id,
            expires_at_ns: value.expires_at_ns,
            account_number: value.account_number,
        }
    }
}

/// A browser's Web Push registration and the pool of VAPID JWTs it signed for it.
///
/// Held on the browser entry rather than on its own, so signing the browser out or
/// letting the registry evict it takes the registration with it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebPushSubscription {
    pub endpoint: String,
    pub created_at_ns: Timestamp,
    pub vapid_public_key: Vec<u8>,
    /// Raw ECDSA P-256 signatures, in window order.
    pub jwt_signatures: Vec<Vec<u8>>,
    pub jwt_issued_at_ns: Timestamp,
}

/// Whether a write replaces the JWT pool on the address that is already registered,
/// rather than moving to a different one.
///
/// What every holder of a registration decides the same way: a browser's entry and a
/// Home Screen app's record both keep what is queued across a pool replacement, and both
/// drop it where the address changes, is removed, or arrives where there was none, since
/// a queue entry stands for a wake-up posted to one address and nothing at another can
/// show it.
pub fn keeps_endpoint(
    registered: Option<&WebPushSubscription>,
    replacing: Option<&WebPushSubscription>,
) -> bool {
    match (registered, replacing) {
        (Some(registered), Some(replacing)) => registered.endpoint == replacing.endpoint,
        _ => false,
    }
}

impl From<StorableWebPushSubscription> for WebPushSubscription {
    fn from(value: StorableWebPushSubscription) -> Self {
        WebPushSubscription {
            endpoint: value.endpoint,
            created_at_ns: value.created_at_ns,
            vapid_public_key: value.vapid_public_key,
            jwt_signatures: value
                .jwt_pool
                .signatures
                .into_iter()
                .map(Vec::from)
                .collect(),
            jwt_issued_at_ns: value.jwt_pool.issued_at_ns,
        }
    }
}

impl From<WebPushSubscription> for StorableWebPushSubscription {
    fn from(value: WebPushSubscription) -> Self {
        StorableWebPushSubscription {
            endpoint: value.endpoint,
            created_at_ns: value.created_at_ns,
            vapid_public_key: value.vapid_public_key,
            jwt_pool: StorableWebPushJwtPool {
                signatures: value.jwt_signatures.into_iter().map(Into::into).collect(),
                issued_at_ns: value.jwt_issued_at_ns,
            },
            // The entry holds both. They are read from here for a record that still
            // carries them, and never written here.
            notifications: None,
            woken_again_at_ns: None,
        }
    }
}

impl From<StorableBrowser> for Browser {
    fn from(value: StorableBrowser) -> Self {
        Browser {
            id: value.id,
            current_browser_key: ByteBuf::from(value.current_browser_key),
            next_browser_key: ByteBuf::from(value.next_browser_key),
            description: BrowserDescription::from(value.description),
            created_at: value.created_at,
            last_used: value.last_used,
            session_count: value.session_count,
            webpush_subscription: value
                .webpush_subscription
                .clone()
                .map(WebPushSubscription::from),
            // From the entry, or from the subscription for a record that carries it
            // there.
            notifications: value
                .notifications
                .or_else(|| {
                    value
                        .webpush_subscription
                        .as_ref()
                        .and_then(|subscription| subscription.notifications.clone())
                })
                .unwrap_or_default()
                .into_iter()
                .filter_map(|queued| {
                    Some(QueuedNotification {
                        application_number: queued.application_number,
                        event: queued.event.map(NotificationEvent::from),
                        sender: Principal::try_from_slice(&queued.sender).ok()?,
                        notification_id: queued.notification_id,
                        expires_at_ns: queued.expires_at_ns,
                        account_number: queued.account_number,
                    })
                })
                .collect(),
            woken_again_at_ns: value.woken_again_at_ns.or_else(|| {
                value
                    .webpush_subscription
                    .as_ref()
                    .and_then(|subscription| subscription.woken_again_at_ns)
            }),
            notification_app: value.notification_app.map(NotificationAppRef::from),
            pending_probe: value.pending_probe,
        }
    }
}

impl From<Browser> for StorableBrowser {
    fn from(value: Browser) -> Self {
        StorableBrowser {
            id: value.id,
            current_browser_key: value.current_browser_key.into_vec(),
            next_browser_key: value.next_browser_key.into_vec(),
            description: StorableBrowserDescription::from(value.description),
            created_at: value.created_at,
            last_used: value.last_used,
            session_count: value.session_count,
            webpush_subscription: value
                .webpush_subscription
                .map(StorableWebPushSubscription::from),
            notification_app: value.notification_app.map(StorableNotificationAppRef::from),
            notifications: (!value.notifications.is_empty()).then(|| {
                value
                    .notifications
                    .into_iter()
                    .map(StorableQueuedNotification::from)
                    .collect()
            }),
            woken_again_at_ns: value.woken_again_at_ns,
            pending_probe: value.pending_probe,
        }
    }
}

impl Device {
    /// Applies the values of `device_data` to self while leaving the other fields intact.
    pub fn apply_device_data(&mut self, device_data: DeviceData) {
        let DeviceData {
            pubkey,
            alias,
            credential_id,
            aaguid,
            purpose,
            key_type,
            protection,
            origin,
            metadata,
        } = device_data;

        self.pubkey = pubkey;
        self.alias = alias;
        self.credential_id = credential_id;
        self.aaguid = aaguid;
        self.purpose = purpose;
        self.key_type = key_type;
        self.protection = protection;
        self.origin = origin;
        self.metadata = metadata;
    }
}

impl From<DeviceData> for Device {
    fn from(device_data: DeviceData) -> Self {
        Self {
            pubkey: device_data.pubkey,
            alias: device_data.alias,
            credential_id: device_data.credential_id,
            aaguid: device_data.aaguid,
            purpose: device_data.purpose,
            key_type: device_data.key_type,
            protection: device_data.protection,
            origin: device_data.origin,
            last_usage_timestamp: None,
            metadata: device_data.metadata,
        }
    }
}

impl From<Device> for DeviceData {
    fn from(device: Device) -> Self {
        Self {
            pubkey: device.pubkey,
            alias: device.alias,
            credential_id: device.credential_id,
            aaguid: device.aaguid,
            purpose: device.purpose,
            key_type: device.key_type,
            protection: device.protection,
            origin: device.origin,
            metadata: device.metadata,
        }
    }
}

impl From<Device> for DeviceWithUsage {
    fn from(device: Device) -> Self {
        Self {
            pubkey: device.pubkey,
            alias: device.alias,
            credential_id: device.credential_id,
            aaguid: device.aaguid,
            purpose: device.purpose,
            key_type: device.key_type,
            protection: device.protection,
            origin: device.origin,
            last_usage: device.last_usage_timestamp,
            metadata: device.metadata,
        }
    }
}

impl From<Device> for DeviceDataWithoutAlias {
    fn from(device_data: Device) -> Self {
        Self {
            pubkey: device_data.pubkey,
            credential_id: device_data.credential_id,
            purpose: device_data.purpose,
            key_type: device_data.key_type,
            protection: device_data.protection,
            origin: device_data.origin,
            metadata_keys: device_data
                .metadata
                .as_ref()
                .map(|m| m.keys().cloned().collect()),
        }
    }
}

impl From<OpenIdCredential> for OpenIdCredentialData {
    fn from(openid_credential: OpenIdCredential) -> Self {
        Self {
            iss: openid_credential.iss,
            sub: openid_credential.sub,
            aud: openid_credential.aud,
            last_usage_timestamp: openid_credential.last_usage_timestamp,
            metadata: openid_credential.metadata,
            sso_domain: openid_credential.sso_domain.map(String::from),
            sso_name: openid_credential.sso_name,
        }
    }
}

impl From<OpenIdCredentialData> for OpenIdCredential {
    fn from(openid_credential: OpenIdCredentialData) -> Self {
        Self {
            iss: openid_credential.iss,
            sub: openid_credential.sub,
            aud: openid_credential.aud,
            last_usage_timestamp: openid_credential.last_usage_timestamp,
            metadata: openid_credential.metadata,
            sso_domain: openid_credential.sso_domain.map(Domain::from_stored),
            sso_name: openid_credential.sso_name,
            // The candid `OpenIdCredentialData` deliberately has no `stable_id`;
            // the SSO write path sets it explicitly on the primary credential.
            stable_id: None,
        }
    }
}

impl From<Anchor> for (StorableFixedAnchor, StorableAnchor) {
    fn from(anchor: Anchor) -> Self {
        let Anchor {
            devices,
            openid_credentials,
            email_recovery,
            verified_emails,
            browsers,
            next_browser_id,
            session_count,
            metadata,
            name,
            created_at,
            anchor_number,
        } = anchor;

        let openid_credentials = openid_credentials.into_iter().map(Into::into).collect();
        let email_recovery = Some(
            email_recovery
                .into_iter()
                .map(StorableEmailRecoveryCredential::from)
                .collect(),
        );
        let verified_emails = Some(
            verified_emails
                .into_iter()
                .map(StorableVerifiedEmail::from)
                .collect(),
        );
        let next_browser_id = Some(next_browser_id);
        let browsers = Some(browsers.into_iter().map(StorableBrowser::from).collect());

        let (mut passkey_credentials, mut recovery_keys, mut recovery_devices) =
            (vec![], vec![], vec![]);

        for Device {
            key_type,
            pubkey,
            alias,
            credential_id,
            aaguid,
            purpose,
            protection,
            origin,
            last_usage_timestamp,
            metadata: _,
        } in &devices
        {
            // Marshall common fields into the new storage format.
            let pubkey = pubkey.clone().into_vec();
            let last_usage_timestamp_ns = *last_usage_timestamp;
            let credential_id = credential_id.clone().map(ByteBuf::into_vec);

            // Usually, we take `credential_id` as is. But in some special cases, we need to
            // plug in a dummy `credential_id` in order to be able to later migrate the device
            // without a credential ID as a passkey.
            let (credential_id, special_device_migration) = match (
                credential_id,
                purpose,
                key_type,
                origin,
            ) {
                // Happy case: clearly a valid recovery phrase
                (None, Purpose::Recovery, KeyType::SeedPhrase, None) => (None, None),

                // Happy case: clearly a valid passkey
                (
                    Some(credential_id),
                    Purpose::Authentication,
                    KeyType::Platform | KeyType::CrossPlatform | KeyType::Unknown,
                    Some(_),
                ) => {
                    if protection == &DeviceProtection::Protected {
                        ic_cdk::println!(
                            "Warning: Passkey with device protection found. PubKey: {:?}, \
                             Anchor: {}",
                            hex::encode(&pubkey),
                            anchor_number,
                        );
                    }
                    (Some(credential_id), None)
                }

                // Special case: valid passkey but without an origin
                (
                    credential_id @ Some(_),
                    Purpose::Authentication,
                    key_type @ (KeyType::Platform | KeyType::CrossPlatform | KeyType::Unknown),
                    None,
                ) => {
                    let special_device_migration = Some(SpecialDeviceMigration::from((
                        &credential_id,
                        &Purpose::Authentication,
                        key_type,
                        &None::<String>,
                    )));
                    if protection == &DeviceProtection::Protected {
                        ic_cdk::println!(
                            "Warning: Passkey without origin with device protection found. \
                             PubKey: {:?}, Anchor: {}",
                            hex::encode(&pubkey),
                            anchor_number,
                        );
                    }
                    (credential_id, special_device_migration)
                }

                // Special case: recovery passkey
                (
                    Some(credential_id),
                    Purpose::Recovery,
                    KeyType::Platform | KeyType::CrossPlatform | KeyType::Unknown,
                    origin,
                ) => {
                    let credential_id = Some(credential_id);
                    let special_device_migration = Some(SpecialDeviceMigration::from((
                        &credential_id,
                        purpose,
                        key_type,
                        origin,
                    )));

                    if protection == &DeviceProtection::Protected {
                        ic_cdk::println!(
                            "Warning: Recovery passkey with device protection found. PubKey: {:?}, \
                             Anchor: {}",
                            hex::encode(&pubkey),
                            anchor_number,
                        );
                    }

                    // No logging for a legitimate legacy recovery device.

                    (credential_id, special_device_migration)
                }

                // Special case: legacy pin-flow
                (None, purpose, KeyType::BrowserStorageKey, origin) => {
                    let special_device_migration = Some(SpecialDeviceMigration::from((
                        &None, purpose, key_type, origin,
                    )));

                    if matches!(purpose, Purpose::Recovery) {
                        ic_cdk::println!(
                            "Missing credential_id for BrowserStorageKey (looks like a key from \
                             the legacy pin-based flow); adding dummy credential_id for migration.\
                             PubKey: {:?}, Purpose: {:?}, Anchor: {}",
                            hex::encode(&pubkey),
                            purpose,
                            anchor_number,
                        );
                    }

                    // purpose == Purpose::Authentication: No logging for a legitimate legacy flow.

                    (Some(vec![0xde, 0xad, 0xbe, 0xef]), special_device_migration)
                }

                // All other special cases
                (credential_id, purpose, key_type, origin) => {
                    let special_device_migration = Some(SpecialDeviceMigration::from((
                        &credential_id,
                        purpose,
                        key_type,
                        origin,
                    )));

                    ic_cdk::println!(
                        "Fallthrough case: PubKey: {:?}, Anchor: {}, special_device_migration: {:?}",
                        hex::encode(&pubkey),
                        anchor_number,
                        special_device_migration,
                    );

                    (credential_id, special_device_migration)
                }
            };

            if let Some(credential_id) = credential_id {
                let alias = if alias.is_empty() {
                    None
                } else {
                    Some(alias.clone())
                };

                // Without an origin, a passkey cannot be used, which means it's conceptually
                // non-optional. Since some passkeys might not have an origin (for historical
                // reasons; we used to have just one supported domain, which was hard coded),
                // this code needs to set the default value. To that end, we default
                // to `II_LEGACY_ORIGIN` from src/frontend/src/lib/legacy/constants.ts.
                let origin = origin
                    .clone()
                    .unwrap_or_else(|| "https://identity.ic0.app".to_string());

                let aaguid = aaguid.map(Vec::from);

                let passkey = StorablePasskeyCredential {
                    pubkey,
                    credential_id,
                    origin,
                    last_usage_timestamp_ns,
                    alias,
                    aaguid,

                    // Not available yet
                    created_at_ns: None,

                    special_device_migration,
                };

                if purpose == &Purpose::Recovery {
                    recovery_devices.push(passkey);
                } else {
                    passkey_credentials.push(passkey);
                }
            } else {
                // No credential id ==> add this key as recovery key

                let is_protected = if matches!(protection, DeviceProtection::Protected) {
                    Some(true)
                } else {
                    None
                };

                recovery_keys.push(StorableRecoveryKey {
                    pubkey,
                    last_usage_timestamp_ns,
                    is_protected,

                    // Not available yet
                    created_at_ns: None,

                    special_device_migration,
                });
            }
        }

        // Recovery devices are also passkeys, but we add them to the end of the list for user
        // convenience (in some flows, the frontend may give preference to the passkeys that
        // appear earlier in the list).
        passkey_credentials.extend(recovery_devices);

        let passkey_credentials = Some(passkey_credentials);

        // Currently, the frontend does not support more than one recovery key. Thus, we log
        // a warning if there are multiple recovery keys for observability.
        if recovery_keys.len() > 1 {
            ic_cdk::println!(
                "Warning: Anchor {} has multiple ({}) recovery keys.",
                anchor_number,
                recovery_keys.len()
            );
        }

        let recovery_keys = Some(recovery_keys);

        (
            StorableFixedAnchor {
                devices,
                metadata,
                created_at,
            },
            StorableAnchor {
                name,
                created_at_ns: created_at,
                openid_credentials,
                passkey_credentials,
                recovery_keys,
                email_recovery,
                verified_emails,
                browsers,
                next_browser_id,
                session_count: Some(session_count),
            },
        )
    }
}

impl From<(AnchorNumber, StorableAnchor)> for Anchor {
    fn from((anchor_number, storable_anchor): (AnchorNumber, StorableAnchor)) -> Self {
        let StorableAnchor {
            name,
            created_at_ns: created_at,
            openid_credentials,
            passkey_credentials,
            recovery_keys,
            email_recovery,
            verified_emails,
            browsers,
            next_browser_id,
            session_count,
        } = storable_anchor;

        let name = name.clone();

        let openid_credentials = openid_credentials
            .into_iter()
            .map(OpenIdCredential::from)
            .collect();
        let email_recovery = email_recovery
            .unwrap_or_default()
            .into_iter()
            .map(EmailRecoveryCredential::from)
            .collect();
        let verified_emails = verified_emails
            .unwrap_or_default()
            .into_iter()
            .map(VerifiedEmail::from)
            .collect();
        let browsers = browsers
            .unwrap_or_default()
            .into_iter()
            .map(Browser::from)
            .collect();
        let next_browser_id = next_browser_id.unwrap_or_default();

        let mut devices = passkey_credentials
            .unwrap_or_default()
            .into_iter()
            .map(
                |StorablePasskeyCredential {
                     pubkey,
                     credential_id,
                     origin,
                     created_at_ns: _,
                     last_usage_timestamp_ns,
                     alias,
                     aaguid,
                     special_device_migration,
                 }| {
                    let (credential_id, purpose, key_type, origin) =
                        if let Some(special_device_migration) = special_device_migration {
                            // Special case: trust what we had before
                            special_device_migration.into()
                        } else {
                            // Happy case: clearly a valid passkey
                            (
                                Some(ByteBuf::from(credential_id)),
                                Purpose::Authentication,
                                KeyType::CrossPlatform,
                                Some(origin),
                            )
                        };

                    Device {
                        pubkey: DeviceKey::from(pubkey),
                        alias: alias.unwrap_or_default(),
                        credential_id,
                        aaguid: aaguid.map(|src| {
                            let mut aaguid_array = [0u8; 16];
                            aaguid_array.copy_from_slice(&src);
                            aaguid_array
                        }),
                        purpose,
                        key_type,
                        protection: DeviceProtection::Unprotected, // Defaulting to Unprotected; we don't store protection in stable memory for passkeys
                        origin,
                        last_usage_timestamp: last_usage_timestamp_ns,
                        metadata: None, // Not stored in stable memory
                    }
                },
            )
            .collect::<Vec<_>>();

        devices.extend(recovery_keys.unwrap_or_default().into_iter().map(
            |StorableRecoveryKey {
                 pubkey,
                 created_at_ns: _,
                 last_usage_timestamp_ns,
                 is_protected,
                 special_device_migration,
             }| {
                let (credential_id, purpose, key_type, origin) =
                    if let Some(special_device_migration) = special_device_migration {
                        // Special case: trust what we had before
                        special_device_migration.into()
                    } else {
                        // Happy case: clearly a valid recovery phrase
                        (None, Purpose::Recovery, KeyType::SeedPhrase, None)
                    };

                Device {
                    credential_id,
                    purpose,
                    key_type,
                    alias: "Recovery Key".to_string(),
                    pubkey: DeviceKey::from(pubkey),
                    aaguid: None,
                    protection: if is_protected.unwrap_or(false) {
                        DeviceProtection::Protected
                    } else {
                        DeviceProtection::Unprotected
                    },
                    origin,
                    last_usage_timestamp: last_usage_timestamp_ns,
                    metadata: None, // Not stored in stable memory
                }
            },
        ));

        let metadata = None; // Deprecated field; not stored in stable memory

        Anchor {
            anchor_number,
            name,
            created_at,
            openid_credentials,
            email_recovery,
            verified_emails,
            browsers,
            next_browser_id,
            session_count: session_count.unwrap_or_default(),
            devices,
            metadata,
        }
    }
}

impl From<(AnchorNumber, StorableFixedAnchor, Option<StorableAnchor>)> for Anchor {
    fn from(
        (anchor_number, storable_fixed_anchor, storable_anchor): (
            AnchorNumber,
            StorableFixedAnchor,
            Option<StorableAnchor>,
        ),
    ) -> Self {
        let StorableFixedAnchor {
            devices,
            metadata,
            created_at,
        } = storable_fixed_anchor;

        let Some(storable_anchor) = storable_anchor else {
            return Anchor {
                name: None,
                session_count: 0,
                openid_credentials: vec![],
                email_recovery: vec![],
                verified_emails: vec![],
                browsers: vec![],
                next_browser_id: 0,
                anchor_number,
                devices,
                metadata,
                created_at,
            };
        };

        let name = storable_anchor.name.clone();

        let openid_credentials = storable_anchor
            .openid_credentials
            .into_iter()
            .map(OpenIdCredential::from)
            .collect();
        let email_recovery = storable_anchor
            .email_recovery
            .unwrap_or_default()
            .into_iter()
            .map(EmailRecoveryCredential::from)
            .collect();
        let verified_emails = storable_anchor
            .verified_emails
            .unwrap_or_default()
            .into_iter()
            .map(VerifiedEmail::from)
            .collect();
        let browsers = storable_anchor
            .browsers
            .unwrap_or_default()
            .into_iter()
            .map(Browser::from)
            .collect();

        Anchor {
            anchor_number,
            devices,
            openid_credentials,
            email_recovery,
            verified_emails,
            browsers,
            next_browser_id: storable_anchor.next_browser_id.unwrap_or_default(),
            session_count: storable_anchor.session_count.unwrap_or_default(),
            metadata,
            name,
            created_at,
        }
    }
}

impl Anchor {
    pub fn browsers(&self) -> &[Browser] {
        &self.browsers
    }

    /// The browser a caller signing as `principal` is, if the identity is signed in
    /// from it.
    ///
    /// The successor slot only. A browser overwrites its stored keypair with that
    /// successor as soon as a sign-in is accepted, so the retired key in the other slot
    /// is one no browser still holds, and matching it would keep a copied key usable
    /// until the next sign-in.
    pub fn browser_by_principal(&self, principal: Principal) -> Option<BrowserId> {
        self.browsers
            .iter()
            .find(|browser| principal == Principal::self_authenticating(&browser.next_browser_key))
            .map(|browser| browser.id)
    }

    /// The browser of this anchor that signed a link token, if any still holds the key
    /// it was signed with.
    ///
    /// The successor slot only, as in [`Self::browser_by_principal`]. A holder of the
    /// retired private key can mint a token of its own at any time, so accepting that
    /// slot would let them pass the expiry bounds and repoint the victim's browser at a
    /// record they control, which hands them the anchor number and moves delivery to
    /// their subscription.
    ///
    /// The price is that a sign-in in another tab during the install window retires the
    /// key the install page minted its token with. The launch carries on without it, by
    /// looking the anchor up instead, and lands on a screen from which a fresh token is
    /// one press away.
    pub fn browser_that_signed_link(
        &self,
        signature: &[u8],
        anchor_number: AnchorNumber,
        expires_at_ns: Timestamp,
    ) -> Option<BrowserId> {
        self.browsers
            .iter()
            .find(|browser| {
                crate::browser_key::verify_notification_app_link(
                    &browser.next_browser_key,
                    signature,
                    anchor_number,
                    expires_at_ns,
                )
            })
            .map(|browser| browser.id)
    }
    /// The browser of this anchor that signed a claim token for `claiming_anchor_number`,
    /// if any still holds the key it was signed with.
    ///
    /// The successor slot only, as in [`Self::browser_that_signed_link`]: a holder of a
    /// retired private key can mint a token of its own at any time, so accepting that
    /// slot would let them hand the app this identity has to an identity they control,
    /// moving its delivery to a subscription of their own.
    pub fn browser_that_signed_claim(
        &self,
        signature: &[u8],
        claiming_anchor_number: AnchorNumber,
        expires_at_ns: Timestamp,
    ) -> Option<BrowserId> {
        self.browsers
            .iter()
            .find(|browser| {
                crate::browser_key::verify_notification_app_claim(
                    &browser.next_browser_key,
                    signature,
                    self.anchor_number,
                    claiming_anchor_number,
                    expires_at_ns,
                )
            })
            .map(|browser| browser.id)
    }


    /// Whether a browser of this anchor already signs with `key`.
    ///
    /// An app key is generated in a partition no browser can reach, so a collision here
    /// is not a reinstall: it is a key being presented as two different things.
    pub fn browser_holds_key(&self, key: &PublicKey) -> bool {
        self.browsers
            .iter()
            .any(|browser| browser.current_browser_key == *key || browser.next_browser_key == *key)
    }

    /// The browser of this identity that points at `app_id`, where one does.
    ///
    /// At most one, because [`Anchor::point_at_notification_app`] takes the reference off
    /// every other entry of this identity as it hands it to one.
    pub fn browser_pointing_at_notification_app(
        &self,
        app_id: NotificationAppId,
    ) -> Option<BrowserId> {
        self.browsers
            .iter()
            .find(|browser| {
                browser
                    .notification_app
                    .is_some_and(|reference| reference.app_id == app_id)
            })
            .map(|browser| browser.id)
    }

    /// Points `browser_id` at `app_id`, replacing whatever it pointed at before and
    /// taking that record off every other entry of this identity, with whatever each of
    /// those had waiting at its address.
    ///
    /// One entry per record, because the app resolves its calls through the one entry it
    /// finds: a browser that lost its key is a second entry here while the app keeps the
    /// key it generated, so without this the app would answer for an entry nothing is
    /// queued on any more.
    ///
    /// Answers the record this browser pointed at before, where that was a different one.
    /// Pointing at the record this browser already points at changes nothing, switch
    /// included: the app replays its install URL on every cold launch for as long as the
    /// token lives, and a replay is not the user asking for delivery back on. Pointing at
    /// a different one drops what this entry had waiting: every entry of it stands for a
    /// wake-up posted to the old app's address, and nothing at the new one can take them.
    pub fn point_at_notification_app(
        &mut self,
        browser_id: BrowserId,
        app_id: NotificationAppId,
    ) -> Option<NotificationAppId> {
        if !self.browsers.iter().any(|browser| browser.id == browser_id) {
            return None;
        }
        for browser in &mut self.browsers {
            if browser.id != browser_id
                && browser
                    .notification_app
                    .is_some_and(|reference| reference.app_id == app_id)
            {
                browser.notification_app = None;
                // The address belongs to another entry now, so nothing here can show
                // what it had waiting, the probe included.
                browser.notifications.clear();
                browser.woken_again_at_ns = None;
                browser.pending_probe = None;
            }
        }
        let browser = self
            .browsers
            .iter_mut()
            .find(|browser| browser.id == browser_id)?;
        if browser
            .notification_app
            .is_some_and(|reference| reference.app_id == app_id)
        {
            return None;
        }
        let previous = browser
            .notification_app
            .replace(NotificationAppRef {
                app_id,
                enabled: true,
            })
            .map(|previous| previous.app_id);
        if previous.is_some() {
            // Every entry waiting stands for a wake-up posted to the old app's address,
            // and the app that answers now is a different one holding a different
            // registration. Nothing there can take them, and the probe asked of the app
            // that is gone has nobody left to answer it.
            browser.notifications.clear();
            browser.woken_again_at_ns = None;
            browser.pending_probe = None;
        }
        previous
    }

    /// Whether this identity wants the app it points at to deliver for it.
    ///
    /// Never a subscription removal: the subscription is the app's and is shared with
    /// every other identity pointing at it.
    pub fn set_notification_app_enabled(&mut self, browser_id: BrowserId, enabled: bool) {
        if let Some(browser) = self
            .browsers
            .iter_mut()
            .find(|browser| browser.id == browser_id)
        {
            if let Some(reference) = browser.notification_app.as_mut() {
                reference.enabled = enabled;
            }
        }
    }

    /// Stops `browser_id` pointing at the app it had, answering which one that was.
    ///
    /// What the user does to turn notifications off for good on this device, and what an
    /// evicted entry takes with it. The record itself is not this list's to delete: the
    /// caller is the one that can see whether anything still points at it.
    pub fn stop_pointing_at_notification_app(
        &mut self,
        browser_id: BrowserId,
    ) -> Option<NotificationAppId> {
        self.browsers
            .iter_mut()
            .find(|browser| browser.id == browser_id)?
            .notification_app
            .take()
            .map(|reference| reference.app_id)
    }

    /// The app this browser points at, where it has one.
    pub fn notification_app(&self, browser_id: BrowserId) -> Option<NotificationAppRef> {
        self.browsers
            .iter()
            .find(|browser| browser.id == browser_id)
            .and_then(|browser| browser.notification_app)
    }

    /// What this browser registered for Web Push, if anything.
    pub fn webpush_subscription(&self, browser_id: BrowserId) -> Option<&WebPushSubscription> {
        self.browsers
            .iter()
            .find(|browser| browser.id == browser_id)
            .and_then(|browser| browser.webpush_subscription.as_ref())
    }

    /// Replaces what this browser registered. A browser the registry no longer lists
    /// has nothing to register against, so the write is dropped.
    pub fn set_webpush_subscription(
        &mut self,
        browser_id: BrowserId,
        subscription: Option<WebPushSubscription>,
    ) {
        if let Some(browser) = self.browsers.iter_mut().find(|one| one.id == browser_id) {
            browser.webpush_subscription = subscription;
        }
    }

    /// What this entry has waiting, empty for one with nothing.
    pub fn notifications(&self, browser_id: BrowserId) -> &[QueuedNotification] {
        self.browsers
            .iter()
            .find(|browser| browser.id == browser_id)
            .map_or(&[], |browser| &browser.notifications)
    }

    pub fn notifications_mut(
        &mut self,
        browser_id: BrowserId,
    ) -> Option<&mut Vec<QueuedNotification>> {
        self.browsers
            .iter_mut()
            .find(|browser| browser.id == browser_id)
            .map(|browser| &mut browser.notifications)
    }

    /// Stamps that this entry has been woken again for something still waiting.
    pub fn set_woken_again(&mut self, browser_id: BrowserId, at_ns: Option<Timestamp>) {
        if let Some(browser) = self.browsers.iter_mut().find(|one| one.id == browser_id) {
            browser.woken_again_at_ns = at_ns;
        }
    }

    /// Records the probe this entry has just sent to the app it points at.
    pub fn set_pending_probe(&mut self, browser_id: BrowserId, notification_id: NotificationId) {
        if let Some(browser) = self.browsers.iter_mut().find(|one| one.id == browser_id) {
            browser.pending_probe = Some(notification_id);
        }
    }

    /// Forgets a probe whose notification has left the queue, so a field that means
    /// "still outstanding" stops naming something gone. Called wherever an entry is
    /// taken out: what it names was taken by the app, evicted by a full queue, or
    /// dropped because its relay post failed.
    ///
    /// An entry still queued but past its expiry is left named, because the only thing
    /// that prunes it is the next write to this queue, and [`Anchor::pending_probe`] is
    /// what decides that it is no longer pending.
    pub fn clear_pending_probe_if_gone(&mut self, browser_id: BrowserId) {
        let Some(browser) = self.browsers.iter_mut().find(|one| one.id == browser_id) else {
            return;
        };
        let Some(probe) = browser.pending_probe else {
            return;
        };
        let queued = browser
            .notifications
            .iter()
            .any(|queued| queued.notification_id == probe && queued.event.is_some());
        if !queued {
            browser.pending_probe = None;
        }
    }

    /// The probe this entry sent that is still queued for the app to take.
    ///
    /// Answered against the queue rather than remembered on its own, so the answer is
    /// about this identity on this device: the app's record is shared with every other
    /// identity pointing at it, and an app on another phone answering its own probe
    /// leaves this one exactly where it was.
    ///
    /// An entry past its expiry is not pending. The app is never shown one, so counting
    /// it would answer the same dead id for good and never post again, and the only
    /// thing that prunes it is another notification for this browser, which on a phone
    /// whose app is gone never comes.
    ///
    /// `None` says the entry is no longer queued and nothing more: it was taken,
    /// evicted by a full queue, or dropped because its relay post failed. It is not
    /// evidence that the app took anything. What proves the app is alive is its
    /// `last_used`, stamped on the call only a live app makes.
    pub fn pending_probe(
        &self,
        browser_id: BrowserId,
        now_ns: Timestamp,
    ) -> Option<NotificationId> {
        let browser = self.browsers.iter().find(|one| one.id == browser_id)?;
        let probe = browser.pending_probe?;
        browser
            .notifications
            .iter()
            .any(|queued| {
                queued.notification_id == probe
                    && queued.event.is_some()
                    && queued.expires_at_ns > now_ns
            })
            .then_some(probe)
    }

    /// Moves each browser's session count by what a write added to or took from it.
    ///
    /// A delta against a browser no entry holds is dropped: the cap can retire an entry
    /// while sessions it opened are still stored, and a count belongs to an entry that
    /// exists.
    pub fn move_browser_session_counts(&mut self, deltas: &BTreeMap<BrowserId, i64>) {
        for browser in &mut self.browsers {
            let Some(delta) = deltas.get(&browser.id) else {
                continue;
            };
            browser.session_count = match delta {
                0.. => browser.session_count.saturating_add(*delta as u32),
                _ => browser
                    .session_count
                    .saturating_sub(delta.unsigned_abs() as u32),
            };
        }
    }

    /// Advances a browser's `last_used`, where the anchor holds that browser and the
    /// stamp moves it forward. A browser no entry names, or a repeat inside one message,
    /// leaves the registry as it is.
    pub fn stamp_browser_use(&mut self, browser_id: BrowserId, now: Timestamp) {
        if let Some(browser) = self
            .browsers
            .iter_mut()
            .find(|browser| browser.id == browser_id)
        {
            if browser.last_used < now {
                browser.last_used = now;
            }
        }
    }

    /// What a caller outside storage may know about this anchor's browsers: an
    /// identifier, what the browser said it was, and when. The keys stay here — they
    /// are how a sign-in proves which entry it is, so handing them out would let
    /// anyone who can read an identity's browsers claim one.
    ///
    /// `None` rather than an empty list, because that is the shape the interface
    /// carries and no caller wants the difference.
    ///
    /// What an app has registered, and when it last called, are asked for rather than
    /// read here: the record lives outside any anchor, because one app serves however
    /// many identities point at it.
    pub fn browsers_info(
        &self,
        app_info: impl Fn(NotificationAppId, bool) -> NotificationAppInfo,
    ) -> Option<Vec<BrowserInfo>> {
        if self.browsers.is_empty() {
            return None;
        }
        Some(
            self.browsers
                .iter()
                .map(|browser| BrowserInfo {
                    id: browser.id,
                    description: browser.description.clone(),
                    created_at: browser.created_at,
                    last_used: browser.last_used,
                    session_count: browser.session_count,
                    notification_app: browser
                        .notification_app
                        .map(|reference| app_info(reference.app_id, reference.enabled)),
                })
                .collect(),
        )
    }

    /// Resolves the browser a sign-in came from by the public key it proved possession of.
    ///
    /// An entry is reached only by the successor it announced. Presenting it promotes that
    /// successor, retires the key it replaces, and leaves the entry awaiting
    /// `next_browser_key` — what the browser presents at its next sign-in. A key some entry
    /// has already retired is refused with [`BrowserError::StaleBrowserKey`] rather
    /// than accepted or registered afresh, so a key is good for exactly one sign-in and a
    /// browser that lost a response is told to promote its own successor instead of
    /// becoming a second list. A key no entry holds at all registers a new browser.
    ///
    /// `description` is taken only where this registers a new browser. An entry that is
    /// advanced keeps the description it was registered with, so what a browser reports is
    /// a fact about a registration rather than about the last sign-in.
    ///
    /// At the cap the least recently used records are dropped, and their ids returned so
    /// the caller can end their sessions too.
    pub fn resolve_browser(
        &mut self,
        current_browser_key: PublicKey,
        next_browser_key: PublicKey,
        description: BrowserDescription,
        now: Timestamp,
    ) -> Result<(BrowserId, Vec<BrowserId>), BrowserError> {
        if current_browser_key == next_browser_key {
            return Err(BrowserError::SuccessorMatchesCurrent);
        }

        // Two questions, and they are not the same one. An entry is *advanced* only by the
        // successor it is waiting for; an entry *holds* a key in either slot, which is what
        // a successor must not collide with.
        let entry_awaiting = |candidate: &PublicKey| {
            self.browsers
                .iter()
                .position(|browser| browser.next_browser_key == *candidate)
        };
        let entry_holding = |candidate: &PublicKey| {
            self.browsers.iter().position(|browser| {
                browser.current_browser_key == *candidate || browser.next_browser_key == *candidate
            })
        };

        let advances = entry_awaiting(&current_browser_key);
        // The entry this request belongs to, which is not always one it can advance: a
        // browser retrying a lost sign-in still belongs to the entry that retired its key,
        // and re-announcing the successor it announced then is not stealing anyone's key.
        let owner = entry_holding(&current_browser_key);
        let successor_holder = entry_holding(&next_browser_key);
        if successor_holder.is_some() && successor_holder != owner {
            return Err(BrowserError::SuccessorAlreadyInUse);
        }

        if let Some(index) = advances {
            let browser = &mut self.browsers[index];
            browser.current_browser_key = current_browser_key;
            browser.next_browser_key = next_browser_key;
            browser.last_used = now;
            return Ok((browser.id, vec![]));
        }

        if owner.is_some() {
            return Err(BrowserError::StaleBrowserKey);
        }

        let id = self.next_browser_id;
        self.next_browser_id = self.next_browser_id.saturating_add(1);
        self.browsers.push(Browser {
            id,
            current_browser_key,
            next_browser_key,
            description,
            created_at: now,
            last_used: now,
            session_count: 0,
            webpush_subscription: None,
            notifications: Vec::new(),
            woken_again_at_ns: None,
            pending_probe: None,
            notification_app: None,
        });

        let mut dropped = vec![];
        while self.browsers.len() > MAX_BROWSERS {
            let least_recently_used = self
                .browsers
                .iter()
                .enumerate()
                .min_by_key(|(_, browser)| (browser.last_used, browser.id))
                .map(|(index, _)| index);
            match least_recently_used {
                Some(index) => {
                    dropped.push(self.browsers.remove(index).id);
                }
                None => break,
            }
        }

        Ok((id, dropped))
    }

    /// Creation of new anchors is restricted in order to make sure that the device checks are
    /// not accidentally bypassed.
    pub fn new(anchor_number: AnchorNumber, created_at: Timestamp) -> Anchor {
        Self {
            anchor_number,
            created_at: Some(created_at),
            session_count: 0,
            devices: vec![],
            openid_credentials: vec![],
            email_recovery: vec![],
            verified_emails: vec![],
            browsers: vec![],
            next_browser_id: 0,
            metadata: None,
            name: None,
        }
    }

    pub fn anchor_number(&self) -> AnchorNumber {
        self.anchor_number
    }

    pub fn add_device(&mut self, device: Device) -> Result<(), AnchorError> {
        if self.devices.iter().any(|e| e.pubkey == device.pubkey) {
            return Err(AnchorError::DuplicateDevice {
                device_key: device.pubkey,
            });
        }
        check_device_invariants(&device)?;
        // Check the new set of devices is consistent
        let mut devices = self.devices.clone();
        devices.push(device);
        check_anchor_invariants(&devices[..], &self.metadata)?;
        self.devices = devices;
        Ok(())
    }

    /// Removes a device from this anchor.
    /// **Note:** Does not check invariants, based on the assumption that no invariant can be
    /// violated by removing a device. See also the documentation on
    /// [check_invariants](Anchor::check_invariants).
    pub fn remove_device(&mut self, device_key: &DeviceKey) -> Result<(), AnchorError> {
        let index = self.device_index(device_key)?;
        check_mutation_allowed(&self.devices[index])?;

        self.devices.remove(index);

        // We do _not_ check invariants here, because there might be anchors that do not fulfill
        // the invariants still stored in stable memory (e.g. anchors with multiple recovery phrases).
        // By allowing the removal of devices on such anchors, they can be made conforming to the invariants
        // by removing devices.
        Ok(())
    }

    pub fn modify_device(
        &mut self,
        device_key: &DeviceKey,
        modified_device: Device,
    ) -> Result<(), AnchorError> {
        if device_key != &modified_device.pubkey {
            return Err(AnchorError::CannotModifyDeviceKey);
        }
        check_device_invariants(&modified_device)?;
        // Ensure the old device can be mutated
        let index = self.device_index(device_key)?;
        check_mutation_allowed(&self.devices[index])?;

        // Check the new set of devices is consistent
        let mut devices = self.devices.clone();
        devices[index] = modified_device;
        check_anchor_invariants(&devices[..], &self.metadata)?;

        // Replace devices
        self.devices = devices;
        Ok(())
    }

    fn device_index(&self, device_key: &DeviceKey) -> Result<usize, AnchorError> {
        let Some(index) = self.devices.iter().position(|e| e.pubkey == device_key) else {
            return Err(AnchorError::NotFound {
                device_key: device_key.clone(),
            });
        };
        Ok(index)
    }

    /// Returns a reference to the device given the key.
    pub fn device(&self, device_key: &DeviceKey) -> Option<&Device> {
        self.devices.iter().find(|e| e.pubkey == device_key)
    }

    /// Returns a reference to the list of devices.
    pub fn devices(&self) -> &Vec<Device> {
        &self.devices
    }

    /// Consumes self and exposes the devices.
    pub fn into_devices(self) -> Vec<Device> {
        self.devices
    }

    /// Sets the timestamp on the given device.
    /// **Note:** Does not check invariants, based on the assumption that no invariant can be
    /// violated by changing the last usage timestamp on a device. See also the documentation on
    /// [check_invariants](Anchor::check_invariants).
    pub fn set_device_usage_timestamp(
        &mut self,
        device_key: &DeviceKey,
        time: Timestamp,
    ) -> Result<(), AnchorError> {
        let Some(device) = self.devices.iter_mut().find(|d| d.pubkey == device_key) else {
            return Err(AnchorError::NotFound {
                device_key: device_key.clone(),
            });
        };
        device.last_usage_timestamp = Some(time);
        Ok(())
    }

    /// Returns the timestamp of the last known activity, if any.
    pub fn last_activity(&self) -> Option<Timestamp> {
        self.devices
            .iter()
            .filter_map(|d| d.last_usage_timestamp)
            .chain(
                self.openid_credentials
                    .iter()
                    .filter_map(|c| c.last_usage_timestamp),
            )
            .max()
    }

    /// Returns information about the domains this anchor was active on since the given timestamp.
    /// Activity on unknown / other domain will be dropped if there is also activity on an II domain
    /// for the following reasons:
    /// * no information is most likely caused by the device having been added before we started
    ///   collecting domain information
    /// * combinations of an unknown domain and an II domain shows that the anchor is at least partially
    ///   active on the II domain (but also does non-standard / unsupported things to their anchor).
    ///   If we are interested in this user group, we might consider extending this function to give
    ///   them their own [DomainActivity] value.
    pub fn domain_activity_since(&self, timestamp: Timestamp) -> DomainActivity {
        #[derive(Default)]
        struct Accumulator {
            ic0_app: bool,
            internet_computer_org: bool,
            id_ai: bool,
            non_ii: bool,
        }

        let result = self
            .devices
            .iter()
            // filter devices with no activity
            .filter(|d| {
                d.last_usage_timestamp
                    .map(|t| t >= timestamp)
                    .unwrap_or(false)
            })
            // assign domain activity
            .fold(Accumulator::default(), |mut acc, device| {
                let Some(ref origin) = device.origin else {
                    acc.non_ii = true;
                    return acc;
                };
                match origin.as_str() {
                    IC0_APP_ORIGIN => acc.ic0_app = true,
                    INTERNETCOMPUTER_ORG_ORIGIN => acc.internet_computer_org = true,
                    ID_AI_ORIGIN => acc.id_ai = true,
                    _ => acc.non_ii = true,
                };
                acc
            });

        // Activity on other domains is discarded if there is also activity on an II domain.
        // The reason is that II might not have complete information since domain information was
        // only introduced recently.
        match (
            result.ic0_app,
            result.internet_computer_org,
            result.id_ai,
            result.non_ii,
        ) {
            (true, true, _, _) => DomainActivity::BothIIDomains,
            (_, true, true, _) => DomainActivity::BothIIDomains,
            (true, _, true, _) => DomainActivity::BothIIDomains,
            (true, false, false, _) => DomainActivity::Ic0App,
            (false, true, false, _) => DomainActivity::InternetComputerOrg,
            (false, false, true, _) => DomainActivity::IdAi,
            (false, false, false, true) => DomainActivity::NonIIDomain,
            (false, false, false, false) => DomainActivity::None,
        }
    }

    /// Returns a reference to the OpenID credential given the key.
    pub fn openid_credential(
        &self,
        openid_credential_key: &OpenIdCredentialKey,
    ) -> Option<&OpenIdCredential> {
        self.openid_credentials()
            .iter()
            .find(|e| &e.key() == openid_credential_key)
    }

    /// Returns a reference to the list of OpenID credentials.
    pub fn openid_credentials(&self) -> &Vec<OpenIdCredential> {
        &self.openid_credentials
    }

    fn openid_credential_index(&self, key: &OpenIdCredentialKey) -> Result<usize, AnchorError> {
        self.openid_credentials
            .iter()
            .position(|entry| &entry.key() == key)
            .ok_or(AnchorError::OpenIdCredentialNotFound)
    }

    pub fn add_openid_credential(
        &mut self,
        openid_credential: OpenIdCredential,
    ) -> Result<(), AnchorError> {
        const MAX_LINKED_CREDENTIALS: usize = 100;

        if self
            .openid_credential_index(&openid_credential.key())
            .is_ok()
        {
            return Err(AnchorError::OpenIdCredentialAlreadyRegistered);
        }

        let num_credentials = self.openid_credentials().len();
        if num_credentials >= MAX_LINKED_CREDENTIALS {
            return Err(AnchorError::TooManyOpenIdCredentials {
                limit: MAX_LINKED_CREDENTIALS,
                num_credentials,
            });
        }

        self.openid_credentials.push(openid_credential);
        Ok(())
    }

    pub fn remove_openid_credential(
        &mut self,
        key: &OpenIdCredentialKey,
    ) -> Result<(), AnchorError> {
        let index = self.openid_credential_index(key)?;
        self.openid_credentials.remove(index);
        Ok(())
    }

    /// Returns `true` when appended, `false` if already present or cap
    /// reached. Never errors — the surrounding OIDC link must not fail
    /// just because the verified-emails bucket is full.
    pub fn mirror_verified_email_from_oidc(
        &mut self,
        address: &str,
        verified_at_ns: Timestamp,
    ) -> bool {
        if self
            .verified_emails
            .iter()
            .any(|e| e.address.eq_ignore_ascii_case(address))
        {
            return false;
        }
        if self.verified_emails.len() >= usize::from(MAX_VERIFIED_EMAILS_PER_ANCHOR) {
            return false;
        }
        self.verified_emails.push(VerifiedEmail {
            address: address.to_ascii_lowercase(),
            verified_at: verified_at_ns,
        });
        true
    }

    /// Refresh a stored credential from a newly verified one. `sso_domain` is
    /// part of what identifies the stored credential, so it has to match.
    pub fn update_openid_credential(
        &mut self,
        openid_credential: OpenIdCredential,
    ) -> Result<(), AnchorError> {
        let index = self.openid_credential_index(&openid_credential.key())?;
        if self.openid_credentials[index].sso_domain != openid_credential.sso_domain {
            return Err(AnchorError::OpenIdCredentialSsoDomainMismatch);
        }
        self.openid_credentials[index] = OpenIdCredential {
            // Don't update last usage timestamp, below `set_openid_credential_usage_timestamp`
            // method should be used instead to update the timestamp explicitly.
            //
            // This is to make sure that only bookkeeping updates the last usage timestamp at the
            // correct moment. If the timestamp is updated at the wrong moment, the activity stats
            // implementation will likely break and report incorrect OpenID credential usage.
            //
            // This is in line with the behavior of last usage timestamp in devices.
            last_usage_timestamp: self.openid_credentials[index].last_usage_timestamp,
            ..openid_credential
        };
        Ok(())
    }

    pub fn set_openid_credential_usage_timestamp(
        &mut self,
        key: &OpenIdCredentialKey,
        timestamp: Timestamp,
    ) -> Result<(), AnchorError> {
        let Some(openid_credential) = self.openid_credentials.iter_mut().find(|c| &c.key() == key)
        else {
            return Err(AnchorError::OpenIdCredentialNotFound);
        };
        openid_credential.last_usage_timestamp = Some(timestamp);
        Ok(())
    }

    /// Returns a reference to the optional identity metadata map
    /// (which is independent of devices / authentication methods).
    pub fn identity_metadata(&self) -> &Option<HashMap<String, MetadataEntry>> {
        &self.metadata
    }

    /// Replaces the existing identity metadata map (which is independent of devices / authentication
    /// methods) with the provided one.
    pub fn replace_identity_metadata(
        &mut self,
        metadata: HashMap<String, MetadataEntry>,
    ) -> Result<(), AnchorError> {
        let metadata = Some(metadata);
        check_anchor_invariants(&self.devices[..], &metadata)?;
        self.metadata = metadata;
        Ok(())
    }

    pub fn set_name(&mut self, name: Option<String>) -> Result<(), AnchorError> {
        const MAX_NAME_LENGTH: usize = 128;
        if name.as_ref().is_some_and(|n| n.len() > MAX_NAME_LENGTH) {
            return Err(AnchorError::NameTooLong {
                limit: MAX_NAME_LENGTH,
            });
        }
        self.name = name;
        Ok(())
    }

    pub fn name(&self) -> Option<String> {
        self.name.clone()
    }

    pub fn created_at(&self) -> Option<Timestamp> {
        self.created_at
    }
}

/// Possible outcomes of domain bound activity for an anchor since a specific timestamp.
pub enum DomainActivity {
    // no activity at all
    None,
    // only active on non-ii domains
    NonIIDomain,
    // only active on the identity.ic0.app domain
    Ic0App,
    // only active on the identity.internetcomputer.org domain
    InternetComputerOrg,
    // only active on the id.ai domain
    IdAi,
    // activity on more than one II domain
    BothIIDomains,
}

/// This is an internal version of `DeviceData` useful to provide a
/// backwards compatible level between device data stored in stable memory.
/// It is similar to `DeviceDataInternal` but with redundant options removed
/// (which is possible due to the stable memory candid schema migration).
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct Device {
    pub pubkey: DeviceKey,
    pub alias: String,
    pub credential_id: Option<CredentialId>,
    pub aaguid: Option<[u8; 16]>,
    pub purpose: Purpose,
    pub key_type: KeyType,
    pub protection: DeviceProtection,
    pub origin: Option<String>,
    pub last_usage_timestamp: Option<Timestamp>,
    pub metadata: Option<HashMap<String, MetadataEntry>>,
}

impl Device {
    pub fn variable_fields_len(&self) -> usize {
        self.alias.len()
            + self.pubkey.len()
            + self.credential_id.as_ref().map(|id| id.len()).unwrap_or(0)
            + self.origin.as_ref().map(|origin| origin.len()).unwrap_or(0)
            + self.metadata.as_ref().map(metadata_len).unwrap_or(0)
    }

    pub fn ii_domain(&self) -> Option<IIDomain> {
        self.origin
            .as_ref()
            .and_then(|origin| IIDomain::try_from(origin.as_str()).ok())
    }

    pub fn is_recovery_phrase(&self) -> bool {
        self.purpose == Purpose::Recovery && self.key_type == KeyType::SeedPhrase
    }
}

fn check_mutation_allowed(device: &Device) -> Result<(), AnchorError> {
    match device.protection {
        DeviceProtection::Unprotected => (),
        DeviceProtection::Protected => {
            if caller() != Principal::self_authenticating(&device.pubkey) {
                return Err(AnchorError::MutationNotAllowed {
                    actual_principal: caller(),
                    authorized_principal: Principal::self_authenticating(&device.pubkey),
                });
            }
        }
    };
    Ok(())
}

fn metadata_len(metadata: &HashMap<String, MetadataEntry>) -> usize {
    metadata
        .iter()
        .map(|(key, value)| key.len() + metadata_entry_len(value))
        .sum()
}

fn metadata_entry_len(entry: &MetadataEntry) -> usize {
    match entry {
        MetadataEntry::String(value) => value.len(),
        MetadataEntry::Bytes(value) => value.len(),
        MetadataEntry::Map(data) => metadata_len(data),
    }
}

#[cfg(not(test))]
fn caller() -> Principal {
    ic_cdk::caller()
}

/// This is required because [ic_cdk::caller()] traps when executed in a non-canister environment.
#[cfg(test)]
fn caller() -> Principal {
    tests::test_caller()
}

/// This checks anchor invariants, in particular:
///   * Max number of devices
///   * Sum of sizes of all variable length fields does not exceed limit
///   * There can only be one recovery phrase
///
/// **Important:**
/// Do **not** introduce new invariants that can be violated by _removing_ devices. The reason
/// is that there might still be devices in stable memory that do not fulfill the invariants.
/// In order to not break those anchors, they need to have a path back to satisfying the invariants.
/// To allow that transition, [remove_device](Anchor::remove_device) does _not_ check the invariants based on the assumption
/// that the state of an anchor cannot get worse by removing a device.
fn check_anchor_invariants(
    devices: &[Device],
    identity_metadata: &Option<HashMap<String, MetadataEntry>>,
) -> Result<(), AnchorError> {
    /// The number of devices is limited. The front-end limits the devices further
    /// by only allowing 16 devices with purpose `authentication` to make sure there is always
    /// a slot for the recovery devices.
    /// Note however, that a free device slot does not guarantee that it will fit the anchor
    /// due to the `VARIABLE_FIELDS_LIMIT`.
    const MAX_DEVICES_PER_ANCHOR: usize = 20;

    /// One device can fill more than one tenth of the available space for a single anchor (4 KB)
    /// with the variable length fields alone.
    /// In order to not give away all the anchor space to the device vector and identity metadata,
    /// we limit the sum of the size of all variable fields of all devices plus the identity metadata.
    /// This ensures that we have the flexibility to expand or change anchors in the future.
    /// The limit is chosen to accommodate pre-memory-migration anchors (limited to 2048 bytes)
    /// while leaving additional room for new fields introduced since.
    const VARIABLE_FIELDS_LIMIT: usize = 5000;

    if devices.len() > MAX_DEVICES_PER_ANCHOR {
        return Err(AnchorError::TooManyDevices {
            num_devices: devices.len(),
            limit: MAX_DEVICES_PER_ANCHOR,
        });
    }

    let variable_fields_size = devices
        .iter()
        .map(|device| device.variable_fields_len())
        .sum::<usize>()
        + identity_metadata.as_ref().map_or(0, metadata_len);

    if variable_fields_size > VARIABLE_FIELDS_LIMIT {
        return Err(AnchorError::CumulativeDataLimitExceeded {
            length: variable_fields_size,
            limit: VARIABLE_FIELDS_LIMIT,
        });
    }

    // check that there is only a single recovery phrase
    if devices
        .iter()
        .filter(|device| device.key_type == KeyType::SeedPhrase)
        .count()
        > 1
    {
        return Err(AnchorError::MultipleRecoveryPhrases);
    }

    Ok(())
}

/// This checks device invariants, in particular:
///   * Sizes of various fields do not exceed limits
///   * Only recovery phrases can be protected
///   * Recovery phrases cannot have a credential id
///   * Metadata does not contain reserved keys
///
///  NOTE: while in the future we may lift this restriction, for now we do ensure that
///  protected devices are limited to recovery phrases, which the webapp expects.
fn check_device_invariants(device: &Device) -> Result<(), AnchorError> {
    const RESERVED_KEYS: &[&str] = &[
        "pubkey",
        "alias",
        "credential_id",
        "purpose",
        "key_type",
        "protection",
        "origin",
        "last_usage_timestamp",
        "metadata",
        "usage",
        "authenticator_attachment",
    ];

    if let Some(metadata) = &device.metadata {
        for key in RESERVED_KEYS {
            if metadata.contains_key(*key) {
                return Err(AnchorError::ReservedMetadataKey {
                    key: key.to_string(),
                });
            }
        }
    }

    check_device_limits(device)?;

    if device.key_type == KeyType::SeedPhrase && device.credential_id.is_some() {
        return Err(AnchorError::RecoveryPhraseCredentialIdMismatch);
    }

    if device.protection == DeviceProtection::Protected && device.key_type != KeyType::SeedPhrase {
        return Err(AnchorError::InvalidDeviceProtection {
            key_type: device.key_type.clone(),
        });
    }
    Ok(())
}

fn check_device_limits(device: &Device) -> Result<(), AnchorError> {
    const ORIGIN_LEN_LIMIT: usize = 100;
    const ALIAS_LEN_LIMIT: usize = 64;
    const PK_LEN_LIMIT: usize = 300;
    const CREDENTIAL_ID_LEN_LIMIT: usize = 350;

    let n = device.alias.len();
    if n > ALIAS_LEN_LIMIT {
        return Err(AnchorError::DeviceLimitExceeded {
            field: "alias".to_string(),
            length: n,
            limit: ALIAS_LEN_LIMIT,
        });
    }

    let n = device.pubkey.len();
    if n > PK_LEN_LIMIT {
        return Err(AnchorError::DeviceLimitExceeded {
            field: "pubkey".to_string(),
            length: n,
            limit: PK_LEN_LIMIT,
        });
    }

    let n = device
        .credential_id
        .as_ref()
        .map(|bytes| bytes.len())
        .unwrap_or_default();
    if n > CREDENTIAL_ID_LEN_LIMIT {
        return Err(AnchorError::DeviceLimitExceeded {
            field: "credential_id".to_string(),
            length: n,
            limit: CREDENTIAL_ID_LEN_LIMIT,
        });
    }

    let n = device
        .origin
        .as_ref()
        .map(|bytes| bytes.len())
        .unwrap_or_default();
    if n > ORIGIN_LEN_LIMIT {
        return Err(AnchorError::DeviceLimitExceeded {
            field: "origin".to_string(),
            length: n,
            limit: ORIGIN_LEN_LIMIT,
        });
    }
    Ok(())
}

#[derive(Debug, Eq, PartialEq)]
pub enum AnchorError {
    TooManyDevices {
        limit: usize,
        num_devices: usize,
    },
    DeviceLimitExceeded {
        field: String,
        length: usize,
        limit: usize,
    },
    CumulativeDataLimitExceeded {
        length: usize,
        limit: usize,
    },
    InvalidDeviceProtection {
        key_type: KeyType,
    },
    RecoveryPhraseCredentialIdMismatch,
    MutationNotAllowed {
        authorized_principal: Principal,
        actual_principal: Principal,
    },
    MultipleRecoveryPhrases,
    CannotModifyDeviceKey,
    NotFound {
        device_key: DeviceKey,
    },
    DuplicateDevice {
        device_key: DeviceKey,
    },
    OpenIdCredentialAlreadyRegistered,
    OpenIdCredentialNotFound,
    OpenIdCredentialSsoDomainMismatch,
    ReservedMetadataKey {
        key: String,
    },
    NameTooLong {
        limit: usize,
    },
    TooManyOpenIdCredentials {
        limit: usize,
        num_credentials: usize,
    },
}

impl fmt::Display for AnchorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnchorError::TooManyDevices { num_devices, limit } => write!(
                f,
                "Anchor device limit exceeded: num devices {num_devices}, limit {limit}"
            ),
            AnchorError::DeviceLimitExceeded {
                field,
                length,
                limit,
            } => write!(
                f,
                "{field} limit exceeded: length {length}, limit {limit}"
            ),
            AnchorError::CumulativeDataLimitExceeded { length, limit } => write!(
                f,
                "Cumulative size of variable sized fields exceeds limit: length {length}, limit {limit}."
            ),
            AnchorError::InvalidDeviceProtection { key_type } => write!(
                f,
                "Only recovery phrases can be locked but key type is {key_type:?}"
            ),
            AnchorError::MutationNotAllowed { actual_principal, authorized_principal } => write!(
                f,
                "Device is locked. Must be authenticated with this device to mutate: authorized principal {authorized_principal}, actual principal {actual_principal}"
            ),
            AnchorError::MultipleRecoveryPhrases => write!(f, "There is already a recovery phrase and only one is allowed."),
            AnchorError::CannotModifyDeviceKey => write!(f, "Device key cannot be updated."),
            AnchorError::NotFound { device_key } => write!(f, "Device with key {} not found.", hex::encode(device_key)),
            AnchorError::DuplicateDevice { device_key } => write!(f, "Device with key {} already exists on this anchor.", hex::encode(device_key)),
            AnchorError::ReservedMetadataKey { key } => write!(f, "Metadata key '{key}' is reserved and cannot be used."),
            AnchorError::RecoveryPhraseCredentialIdMismatch => write!(f, "Devices with key type seed_phrase must not have a credential id."),
            AnchorError::OpenIdCredentialAlreadyRegistered => write!(f, "OpenID credential has already been registered on this or another anchor."),
            AnchorError::OpenIdCredentialNotFound => write!(f, "OpenID credential not found."),
            AnchorError::OpenIdCredentialSsoDomainMismatch => write!(f, "OpenID credential is stored for a different SSO discovery domain."),
            AnchorError::NameTooLong {limit} => write!(f, "Name is too long. Maximum length of name is {limit}."),
            AnchorError::TooManyOpenIdCredentials { limit, num_credentials } => write!(f, "Too many OpenID credentials. Maximum number of OpenID credentials is {limit}. Current number of OpenID credentials is {num_credentials}."),
        }
    }
}
