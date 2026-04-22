// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).
use crate::http::security_headers;
use crate::state;
use asset_util::{Asset, CertifiedAssets, ContentEncoding, ContentType};
use candid::Encode;
use internet_identity_interface::internet_identity::types::{
    InternetIdentityInit, InternetIdentitySynchronizedConfig,
};

// used both in init and post_upgrade
pub fn init_assets(config: &InternetIdentityInit) {
    state::assets_mut(|certified_assets| {
        let assets = get_static_assets(config);
        let shared_headers = security_headers(config.related_origins.clone());
        *certified_assets = CertifiedAssets::certify_assets(assets, &shared_headers);
    });
}

// Gets the static assets. All static assets are prepared only once (like injecting the canister ID).
pub fn get_static_assets(config: &InternetIdentityInit) -> Vec<Asset> {
    // Required to make the synchronized config available to the II frontend
    // canister. The synchronized config is the subset of state the frontend
    // needs on page load — SSO provider registrations (`oidc_configs`) are
    // user-driven through the `add_discoverable_oidc_config` update call and
    // the frontend does not need the list in advance, so we don't ship it.
    let synchronized = InternetIdentitySynchronizedConfig {
        openid_configs: config.openid_configs.clone(),
    };
    let mut assets = vec![Asset {
        url_path: "/.config.did.bin".to_string(),
        content: Encode!(&synchronized).unwrap(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::OCTETSTREAM,
    }];

    // Required to make II available on custom domains (e.g. backend.id.ai).
    // See https://internetcomputer.org/docs/current/developer-docs/production/custom-domain/#custom-domains-on-the-boundary-nodes
    assets.push(Asset {
        url_path: "/.well-known/ic-domains".to_string(),
        content: config
            .related_origins
            .clone()
            .unwrap_or_default()
            .into_iter()
            .map(|origin| origin.replace("https://", ""))
            .collect::<Vec<_>>()
            .join("\n")
            .into_bytes(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::OCTETSTREAM,
    });

    assets
}
