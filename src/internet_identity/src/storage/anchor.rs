use crate::email_inbound::MAX_VERIFIED_EMAILS_PER_ANCHOR;
use crate::ii_domain::IIDomain;
use crate::notifications::installed_app::{NotificationApp, NotificationAppId, NotificationAppRef};
use crate::notifications::webpush::{PayloadIkm, PayloadKeyId, PushEndpoint};
use crate::openid::{Domain, OpenIdCredential, OpenIdCredentialKey};
use crate::storage::storable::anchor::StorableAnchor;
use crate::storage::storable::browser::StorableBrowser;
use crate::storage::storable::browser_description::StorableBrowserDescription;
use crate::storage::storable::email_recovery_credential::StorableEmailRecoveryCredential;
use crate::storage::storable::fixed_anchor::StorableFixedAnchor;
use crate::storage::storable::notification_app::StorableNotificationAppRef;
use crate::storage::storable::notifications::browser_inbox::{
    StorableInboxNotification, StorableNotificationEvent,
};
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
    pub notifications: Vec<InboxNotification>,
    /// What the next entry deposited in that inbox is named. Never rewound, so an id is
    /// not reused while a wake-up announcing the entry that held it is still in flight.
    pub next_inbox_notification_id: InboxNotificationId,
    /// The Home Screen app this browser installed or claimed, where it has one. The
    /// reference outlives signing out and turning notifications off.
    pub notification_app: Option<NotificationAppRef>,
}

impl Browser {
    /// Whether this entry points at `app_id`. Pointing is which app this browser has,
    /// not where its wake-ups went.
    pub fn is_pointing_at(&self, app_id: NotificationAppId) -> bool {
        self.notification_app
            .is_some_and(|reference| reference.linked_app() == Some(app_id))
    }

    /// Whether this entry points at `app_id` with delivery switched off, which is this
    /// identity's answer about this device rather than about the record carrying it.
    pub fn is_delivery_disabled_for(&self, app_id: NotificationAppId) -> bool {
        let Some(reference) = self.notification_app else {
            return false;
        };
        reference.linked_app() == Some(app_id) && !reference.is_delivering()
    }
}

/// A notification a browser's service worker has yet to take.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InboxNotification {
    /// Ours, assigned at deposit and unique within the inbox holding it. A wake-up names
    /// its entry by this, so the worker takes the notification the push was sent for
    /// rather than whichever one happens to be at the head.
    pub inbox_notification_id: InboxNotificationId,
    /// `None` where Internet Identity sent this itself.
    pub application_number: Option<ApplicationNumber>,
    /// The canister that sent it, which the service worker fetches the text from.
    pub sender: Principal,
    pub notification_id: NotificationId,
    pub expires_at_ns: Timestamp,
    /// `None` is the unreserved default account, and always `None` for our own.
    pub account_number: Option<AccountNumber>,
    /// Set where Internet Identity sent this itself, and what it has to say.
    pub event: Option<NotificationEvent>,
}

impl From<InboxNotification> for StorableInboxNotification {
    fn from(value: InboxNotification) -> Self {
        StorableInboxNotification {
            inbox_notification_id: value.inbox_notification_id,
            application_number: value.application_number,
            event: value.event.map(StorableNotificationEvent::from),
            sender: value.sender.as_slice().to_vec(),
            notification_id: value.notification_id,
            expires_at_ns: value.expires_at_ns,
            account_number: value.account_number,
        }
    }
}

/// A browser's Web Push registration: where to reach it, and what to seal for it.
///
/// Held on the browser entry rather than on its own, so signing the browser out or
/// letting the registry evict it takes the registration with it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WebPushSubscription {
    pub endpoint: PushEndpoint,
    pub created_at_ns: Timestamp,
    /// What a payload through this subscription is sealed with, derived by the browser.
    pub payload_ikm: PayloadIkm,
    pub payload_keyid: PayloadKeyId,
}

impl TryFrom<StorableWebPushSubscription> for WebPushSubscription {
    type Error = String;

    /// A row this canister wrote, read back. Every newtype below asserts shape alone, so
    /// the only way one refuses is a row that is not what it claims to be — which is the
    /// same failure as CBOR that will not decode, and belongs to the read.
    fn try_from(value: StorableWebPushSubscription) -> Result<Self, Self::Error> {
        Ok(WebPushSubscription {
            endpoint: PushEndpoint::try_from(value.endpoint.as_str())?,
            created_at_ns: value.created_at_ns,
            payload_ikm: PayloadIkm::try_from(value.payload_ikm)?,
            payload_keyid: PayloadKeyId::try_from(value.payload_keyid)?,
        })
    }
}

impl From<WebPushSubscription> for StorableWebPushSubscription {
    fn from(value: WebPushSubscription) -> Self {
        StorableWebPushSubscription {
            endpoint: value.endpoint.into_string(),
            created_at_ns: value.created_at_ns,
            payload_ikm: value.payload_ikm.into_vec(),
            payload_keyid: value.payload_keyid.into_vec(),
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
            // A registration that will not read back reads as none, which is a state this
            // entry can already be in and the one thing it recovers from by itself: the
            // browser subscribes again and writes a row that does read. Refusing the
            // whole anchor instead would take an identity down over a push address.
            webpush_subscription: value.webpush_subscription.clone().and_then(|stored| {
                WebPushSubscription::try_from(stored)
                    .inspect_err(|reason| {
                        ic_cdk::println!(
                            "ERROR: dropped the registration of browser {} as unreadable: {reason}",
                            value.id
                        );
                    })
                    .ok()
            }),
            notifications: value
                .notifications
                .unwrap_or_default()
                .into_iter()
                .filter_map(|queued| {
                    Some(InboxNotification {
                        inbox_notification_id: queued.inbox_notification_id,
                        application_number: queued.application_number,
                        event: queued.event.map(NotificationEvent::from),
                        sender: Principal::try_from_slice(&queued.sender).ok()?,
                        notification_id: queued.notification_id,
                        expires_at_ns: queued.expires_at_ns,
                        account_number: queued.account_number,
                    })
                })
                .collect(),
            next_inbox_notification_id: value.next_inbox_notification_id.unwrap_or_default(),
            notification_app: value.notification_app.map(NotificationAppRef::from),
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
            next_inbox_notification_id: Some(value.next_inbox_notification_id),
            notifications: (!value.notifications.is_empty()).then(|| {
                value
                    .notifications
                    .into_iter()
                    .map(StorableInboxNotification::from)
                    .collect()
            }),
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

    /// The browser that signed a claim token for `claiming_anchor_number`, if any still
    /// holds the key. The successor slot only, as in [`Self::browser_by_principal`].
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

    /// Whether a browser of this anchor already signs with `key`. An app key comes from a
    /// partition no browser can reach, so a collision here is not a reinstall.
    /// Whether a browser of this identity already signs as `principal`, which an app may
    /// not take over: the record would answer for an entry that is not its own.
    pub fn browser_signs_as(&self, principal: Principal) -> bool {
        self.browsers.iter().any(|browser| {
            principal == Principal::self_authenticating(&browser.current_browser_key)
                || principal == Principal::self_authenticating(&browser.next_browser_key)
        })
    }

    /// The browser of this identity that points at `app_id`, where one does. At most one:
    /// [`Anchor::point_at_notification_app`] takes the reference off every other entry.
    pub fn browser_pointing_at_notification_app(
        &self,
        app_id: NotificationAppId,
    ) -> Option<BrowserId> {
        self.browsers
            .iter()
            .find(|browser| {
                browser
                    .notification_app
                    .is_some_and(|reference| reference.linked_app() == Some(app_id))
            })
            .map(|browser| browser.id)
    }

    /// Records that `browser_id` has started an install, naming the only caller that may
    /// complete it. Replaces any earlier pending install of that browser, so minting a
    /// second install URL retires the first.
    ///
    /// `link_principal` is the principal of the one-shot key the browser generated and
    /// put in the install URL's fragment. The app added from that URL signs as it to
    /// link, once: nothing else is stored between the two halves, so that signature is
    /// the whole of the authority.
    pub fn start_notification_app_link(
        &mut self,
        browser_id: BrowserId,
        link_principal: Principal,
        expires_at_ns: Timestamp,
    ) {
        let Some(browser) = self.browsers.iter_mut().find(|b| b.id == browser_id) else {
            return;
        };
        browser.notification_app = Some(NotificationAppRef::Pending {
            link_principal,
            expires_at_ns,
        });
    }

    /// The browser of this anchor whose pending install names `caller`, where the slot has
    /// not expired.
    pub fn browser_awaiting_link(&self, caller: Principal, now_ns: Timestamp) -> Option<BrowserId> {
        self.browsers
            .iter()
            .find(|browser| match browser.notification_app {
                Some(NotificationAppRef::Pending {
                    link_principal,
                    expires_at_ns,
                }) => link_principal == caller && expires_at_ns > now_ns,
                _ => false,
            })
            .map(|browser| browser.id)
    }

    /// Points `browser_id` at `app_id`, which also spends the pending install that
    /// authorized it, and takes that record off every other entry of this identity.
    ///
    /// One entry per record, because the app resolves its calls through the one entry it
    /// finds: a browser that lost its key is a second entry here while the app keeps the
    /// key it generated, so without this the app would answer for an entry nothing is
    /// queued on any more.
    ///
    /// Pointing at the record this browser already points at changes nothing, switch
    /// included: a claim token may be spent twice, and a replay is not the user asking
    /// for delivery back on.
    pub fn point_at_notification_app(&mut self, browser_id: BrowserId, app_id: NotificationAppId) {
        let Some(browser) = self
            .browsers
            .iter()
            .find(|browser| browser.id == browser_id)
        else {
            return;
        };
        if browser.is_pointing_at(app_id) {
            return;
        }
        // Whether this identity wants the app to deliver is the user's answer about the
        // app, not about whichever entry happens to hold the reference, so a rotation
        // that moves it between this identity's browsers carries their answer along. A
        // first link has nobody to carry it from and starts delivering, because every
        // caller that makes one is the user asking for this app.
        let enabled = self
            .browsers
            .iter()
            .find_map(|browser| match browser.notification_app {
                Some(NotificationAppRef::Linked {
                    app_id: held,
                    enabled,
                }) if held == app_id => Some(enabled),
                _ => None,
            })
            .unwrap_or(true);
        for browser in &mut self.browsers {
            // Every OTHER entry lets go, never this one: the entry being pointed is what
            // the check above reads, and stripping it here would make re-pointing at the
            // app this entry already has look like a fresh one and silently switch
            // delivery back on.
            if browser.id != browser_id
                && browser
                    .notification_app
                    .is_some_and(|reference| reference.linked_app() == Some(app_id))
            {
                browser.notification_app = None;
                // The record's address belongs to the entry being pointed now, and this
                // one has no address of its own to have been posted at.
                browser.notifications.clear();
            }
        }
        let Some(browser) = self
            .browsers
            .iter_mut()
            .find(|browser| browser.id == browser_id)
        else {
            return;
        };
        // A browser has one route. Pointing at a record leaves behind whatever address it
        // registered for itself, and what is waiting goes with it: the entry only reaches
        // here because the record it points at changed, so everything queued was posted
        // somewhere this browser no longer reads.
        browser.webpush_subscription = None;
        browser.notifications.clear();
        browser.notification_app = Some(NotificationAppRef::Linked { app_id, enabled });
    }

    /// This identity asking the app it points at to deliver for it. Never a subscription
    /// write: the subscription is the app's and is shared.
    pub fn enable_notification_app(&mut self, browser_id: BrowserId) {
        self.set_notification_app_delivery(browser_id, true);
    }

    /// This identity asking the app it points at to stop delivering for it. Never a
    /// subscription removal: the subscription is the app's and is shared with every other
    /// identity pointing at it.
    pub fn disable_notification_app(&mut self, browser_id: BrowserId) {
        self.set_notification_app_delivery(browser_id, false);
    }

    fn set_notification_app_delivery(&mut self, browser_id: BrowserId, enabled: bool) {
        let Some(browser) = self
            .browsers
            .iter_mut()
            .find(|browser| browser.id == browser_id)
        else {
            return;
        };
        // An install that has not completed has no delivery to switch.
        let Some(NotificationAppRef::Linked {
            enabled: current, ..
        }) = browser.notification_app.as_mut()
        else {
            return;
        };
        *current = enabled;
        if enabled {
            return;
        }
        // What is queued was posted to an address this entry has just stopped being
        // delivered at. Nothing drains it while delivery is off and nothing joins it, so
        // leaving it would only mean the first wake-up after switching back on shows
        // something from before the switch-off instead of what caused it.
        browser.notifications.clear();
    }

    /// Forgets `app_id` on every entry of this identity that points at it, and what the
    /// record's address was carrying for them.
    ///
    /// The record itself is not this list's to delete: only a caller holding every anchor
    /// can see whether another identity still points at it.
    pub fn drop_notification_app(&mut self, app_id: NotificationAppId) {
        for browser in &mut self.browsers {
            if !browser.is_pointing_at(app_id) {
                continue;
            }
            browser.notification_app = None;
            // Everything waiting went to the record's address, because a browser pointing
            // at a record has no address of its own, and nothing can read that one once
            // the record is gone.
            browser.notifications.clear();
        }
    }

    /// The entry of this identity whose calls an app signing as `app_id` acts on: one
    /// pointing at it with delivery on. Switching delivery off is this identity saying
    /// the app does not act for it, so there is no entry to find.
    ///
    /// At most one: pointing an entry at a record takes that record off every other entry
    /// of the identity, which is what lets an app resolve its calls to a single entry.
    pub fn browser_delivering_through(&self, app_id: NotificationAppId) -> Option<BrowserId> {
        self.browsers
            .iter()
            .find(|browser| {
                browser.is_pointing_at(app_id) && !browser.is_delivery_disabled_for(app_id)
            })
            .map(|browser| browser.id)
    }

    /// Clears everything this browser has waiting, which is all of it: a browser pointing
    /// at a record has no address of its own, so every entry was bound for the record's
    /// address and dies with it. `true` where anything went.
    pub fn clear_notifications_for_browser(&mut self, browser_id: BrowserId) -> bool {
        let Some(browser) = self
            .browsers
            .iter_mut()
            .find(|browser| browser.id == browser_id)
        else {
            return false;
        };
        let had_any = !browser.notifications.is_empty();
        browser.notifications.clear();
        had_any
    }

    /// This browser leaving the record it points at, which is what registering an address
    /// of its own amounts to. Answers which record it left, so the caller can see whether
    /// anything still points at that one.
    ///
    /// An install still running is left alone: it is not a route yet, and a browser whose
    /// install never completes must keep the address it has.
    pub fn leave_notification_app(&mut self, browser_id: BrowserId) -> Option<NotificationAppId> {
        let browser = self.browsers.iter_mut().find(|b| b.id == browser_id)?;
        let NotificationAppRef::Linked { app_id, .. } = browser.notification_app? else {
            return None;
        };
        browser.notification_app = None;
        Some(app_id)
    }

    /// The entry this id names, where the registry still lists one.
    pub fn browser(&self, browser_id: BrowserId) -> Option<&Browser> {
        self.browsers
            .iter()
            .find(|browser| browser.id == browser_id)
    }

    /// The app this browser points at, where it has one.
    pub fn notification_app(&self, browser_id: BrowserId) -> Option<NotificationAppRef> {
        self.browser(browser_id)
            .and_then(|browser| browser.notification_app)
    }

    /// What this browser reads about the app it points at.
    ///
    /// The record lives outside any anchor, because one app serves however many
    /// identities point at it, so it is asked for rather than read here.
    ///
    /// `None` where this browser points at nothing and where `app` finds no record: a
    /// reference naming a record that is gone is no app this identity has, and
    /// `last_seen_ns` is the only evidence an app still exists at all.
    fn linked_app_info(
        &self,
        browser_id: BrowserId,
        app: impl Fn(NotificationAppId) -> Option<NotificationApp>,
    ) -> Option<NotificationAppInfo> {
        let NotificationAppRef::Linked { app_id, enabled } = self.notification_app(browser_id)?
        else {
            return None;
        };
        let app = app(app_id)?;
        Some(NotificationAppInfo {
            subscribed: app.webpush_subscription.is_some(),
            enabled,
            last_seen_ns: app.last_used_ns,
        })
    }

    /// An install whose key is still spendable reads as neither linked nor nothing: the
    /// app it was handed to can still finish, and only the user can open it. Past that
    /// point it reads as nothing, because the URL on their Home Screen is frozen and no
    /// launch of it can ever complete.
    pub fn notification_app_status(
        &self,
        browser_id: BrowserId,
        now_ns: Timestamp,
        app: impl Fn(NotificationAppId) -> Option<NotificationApp>,
    ) -> NotificationAppStatus {
        match self.notification_app(browser_id) {
            Some(NotificationAppRef::Pending { expires_at_ns, .. }) if expires_at_ns > now_ns => {
                NotificationAppStatus::AwaitingInstall(expires_at_ns)
            }
            Some(NotificationAppRef::Pending { .. }) => NotificationAppStatus::NoApp,
            Some(NotificationAppRef::Linked { .. }) => {
                match self.linked_app_info(browser_id, app) {
                    Some(info) => NotificationAppStatus::Linked(info),
                    None => NotificationAppStatus::NoApp,
                }
            }
            None => NotificationAppStatus::NoApp,
        }
    }

    /// What this browser registered for Web Push, if anything.
    pub fn webpush_subscription(&self, browser_id: BrowserId) -> Option<&WebPushSubscription> {
        self.browser(browser_id)
            .and_then(|browser| browser.webpush_subscription.as_ref())
    }

    /// Replaces what this browser registered. A browser the registry no longer lists
    /// has nothing to register against, so the write is dropped.
    ///
    /// A browser has one route, so registering an address of its own leaves the record it
    /// points at, and the record it left is the answer: the caller is the one that can see
    /// whether anything still points at it. Removing a registration asks for nothing in
    /// its place and leaves the reference where it is.
    pub fn set_webpush_subscription(
        &mut self,
        browser_id: BrowserId,
        subscription: Option<WebPushSubscription>,
    ) -> Option<NotificationAppId> {
        let left = match subscription {
            Some(_) => self.leave_notification_app(browser_id),
            None => None,
        };
        if let Some(browser) = self.browsers.iter_mut().find(|one| one.id == browser_id) {
            browser.webpush_subscription = subscription;
        }
        left
    }

    /// What this entry has waiting, empty for one with nothing.
    pub fn notifications_for_browser(&self, browser_id: BrowserId) -> &[InboxNotification] {
        self.browser(browser_id)
            .map_or(&[], |browser| &browser.notifications)
    }

    pub fn notifications_for_browser_mut(
        &mut self,
        browser_id: BrowserId,
    ) -> Option<&mut Vec<InboxNotification>> {
        self.browsers
            .iter_mut()
            .find(|browser| browser.id == browser_id)
            .map(|browser| &mut browser.notifications)
    }

    /// The inbox together with the id its next entry takes, which a deposit needs both of.
    pub fn inbox_for_browser_mut(
        &mut self,
        browser_id: BrowserId,
    ) -> Option<(&mut Vec<InboxNotification>, &mut InboxNotificationId)> {
        self.browsers
            .iter_mut()
            .find(|browser| browser.id == browser_id)
            .map(|browser| {
                (
                    &mut browser.notifications,
                    &mut browser.next_inbox_notification_id,
                )
            })
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
    /// `app` answers what a record holds, as it does for
    /// [`Anchor::linked_app_info`], which is what keeps the two reads of an
    /// installed app answering the same thing.
    pub fn browsers_info(
        &self,
        app: impl Fn(NotificationAppId) -> Option<NotificationApp>,
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
                    // A probe is read where it was sent, not off the identity's app list.
                    notification_app: self.linked_app_info(browser.id, &app),
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
            next_inbox_notification_id: 0,
            id,
            current_browser_key,
            next_browser_key,
            description,
            created_at: now,
            last_used: now,
            session_count: 0,
            webpush_subscription: None,
            notifications: Vec::new(),
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
