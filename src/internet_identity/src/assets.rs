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
    // Required to make the synchronized config available to the II frontend canister.
    // The synchronized config is the part of `InternetIdentityInit` that is needed for both the II backend and frontend.
    let mut assets = vec![Asset {
        url_path: "/.config.did.bin".to_string(),
        content: Encode!(&InternetIdentitySynchronizedConfig::from(config)).unwrap(),
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
