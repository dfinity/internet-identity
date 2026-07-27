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
    //
    // The file lists every custom domain the canister serves, one per line.
    // Besides the related origins (the passkey RP origins), we also include
    // the backend origin so the II backend canister is reachable on its own
    // custom domain (e.g. `backend.beta.id.ai`).
    let mut ic_domains: Vec<String> = config
        .related_origins
        .clone()
        .unwrap_or_default()
        .iter()
        .map(|origin| strip_origin_scheme(origin))
        .collect();
    if let Some(backend_origin) = &config.backend_origin {
        let backend_domain = strip_origin_scheme(backend_origin);
        // Skip when empty or already present (e.g. the backend origin is also
        // listed among the related origins) to avoid blank or duplicate lines.
        if !backend_domain.is_empty() && !ic_domains.contains(&backend_domain) {
            ic_domains.push(backend_domain);
        }
    }
    assets.push(Asset {
        url_path: "/.well-known/ic-domains".to_string(),
        content: ic_domains.join("\n").into_bytes(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::OCTETSTREAM,
    });

    assets
}

/// Strips the URL scheme (`https://` or `http://`) from an origin, leaving the
/// bare host (and port, if any) as required by the `ic-domains` file format.
fn strip_origin_scheme(origin: &str) -> String {
    origin
        .strip_prefix("https://")
        .or_else(|| origin.strip_prefix("http://"))
        .unwrap_or(origin)
        .to_string()
}
