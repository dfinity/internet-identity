This is Internet Identity release [release-2025-02-07](https://github.com/dfinity/internet-identity/releases/tag/release-2025-02-07) for commit [d3d11ec3ada74ed919a5c60f091301b638b7ffae](https://github.com/dfinity/internet-identity/commit/d3d11ec3ada74ed919a5c60f091301b638b7ffae).
The sha256 of production asset [internet_identity_production.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-02-07/internet_identity_production.wasm.gz) is [8e9ed096c1ce86f45feed6e8142c3ba47a6f06920bb621d3e941ef5f6b1cd06b](https://github.com/dfinity/internet-identity/actions/runs/13199969919/job/36849557784#step:7:1).

This release includes bugfixes, many new improvements and additions to the upcoming domain compatibility feature (currently behind feature flag) as well as a small copy change on the landing page.

## What's Changed

- Use RP ID in WebAuthIdentity sign by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2827
- Remove persisted cancelled RP IDs by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2830
- Helper to find webauthn steps by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2833
- Iframe webauthn postmessage by @sea-snake in https://github.com/dfinity/internet-identity/pull/2825
- Use iframe workaround when needed by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2834
- Rename WebAuthnSteps to WebAuthnFlows by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2836
- Rename PossiblyWrongRPID to PossiblyWrongWebAuthnFlow by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2835
- Use devices origin if ROR is supported by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2840
- Improve post message interface by @sea-snake in https://github.com/dfinity/internet-identity/pull/2838
- Change copy by @LXIF in https://github.com/dfinity/internet-identity/pull/2841
- Fix forward error from iframe by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2843
- Check for Password Manager extensions when adding new devices by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2842

**Full Changelog**: https://github.com/dfinity/internet-identity/compare/release-2025-01-31...release-2025-02-07

## Build flavors

For more information please see the [Build flavors](https://github.com/dfinity/internet-identity/tree/release-2025-02-07#build-features-and-flavors) section of the README.

| Filename                                                                                                                                                       | sha256 (links to CI Run)                                                                                                                                             |
| -------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [internet_identity_production.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-02-07/internet_identity_production.wasm.gz) | [`8e9ed096c1ce86f45feed6e8142c3ba47a6f06920bb621d3e941ef5f6b1cd06b`](https://github.com/dfinity/internet-identity/actions/runs/13199969919/job/36849557784#step:7:1) |
| [internet_identity_dev.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-02-07/internet_identity_dev.wasm.gz)               | [`7dacedd06e14a3f68f3242277ca332b6417ef74cb2a7d9ff113c63d4dce6a880`](https://github.com/dfinity/internet-identity/actions/runs/13199969919/job/36849558245#step:7:1) |
| [internet_identity_test.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-02-07/internet_identity_test.wasm.gz)             | [`f85abd40e66db8c47bde210167e21d3dcdf93db8c89f17d67e9de05c50e7139a`](https://github.com/dfinity/internet-identity/actions/runs/13199969919/job/36849558012#step:7:1) |
| [archive.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-02-07/archive.wasm.gz)                                           | [`ee9666e45c01d9c94ff79fd3a5fc1bba804f4eb9cd8e484b248ac6bb6baf5ad5`](https://github.com/dfinity/internet-identity/actions/runs/13199969919/job/36849557520#step:6:1) |
| [vc_demo_issuer.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-02-07/vc_demo_issuer.wasm.gz)                             | [`3b00761e7c1138c70692d0c9ebf1a580d13746260248af3d496c37f79ca70882`](https://github.com/dfinity/internet-identity/actions/runs/13199969919/job/36849547223#step:8:1) |

## Wasm Verification

To build the wasm modules yourself and verify their hashes, run the following commands from the root of the [Internet Identity repository](https://github.com/dfinity/internet-identity):

```
git pull # to ensure you have the latest changes.
git checkout d3d11ec3ada74ed919a5c60f091301b638b7ffae
./scripts/verify-hash --ii-hash 8e9ed096c1ce86f45feed6e8142c3ba47a6f06920bb621d3e941ef5f6b1cd06b
```

Make sure to compare the hashes also with the proposal payload when verifying canister upgrade proposals.

## Argument Verification

Run the following command to verify the upgrade argument hash:

```
didc encode '(null)' | xxd -r -p | sha256sum
```

The output should match the argument hash.
