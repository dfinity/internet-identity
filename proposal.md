This is Internet Identity release [release-2025-01-10](https://github.com/dfinity/internet-identity/releases/tag/release-2025-01-10) for commit [8671b4f4c61a3b641a08b2ffb25053201e37e512](https://github.com/dfinity/internet-identity/commit/8671b4f4c61a3b641a08b2ffb25053201e37e512).
The sha256 of production asset [internet_identity_production.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-01-10/internet_identity_production.wasm.gz) is [377b17ef2f5c83ecdc78292373fbb3a1e632755b4de3910a8d96d0e64d657753](https://github.com/dfinity/internet-identity/actions/runs/12709783528/job/35429516842#step:7:1).

This release includes a bugfix for veteran II users on Windows as well as testing functionality for upcoming features and issues arising from the recent macOS update.

## What's Changed

- Update dapps list by @gix-bot in https://github.com/dfinity/internet-identity/pull/2750
- Add tests to make sure .well-known routes are not cached by @LXIF in https://github.com/dfinity/internet-identity/pull/2749
- Refactor finding rpId for login by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2751
- Add bot approved files policy by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2752
- Retry with differen rp id by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2753
- Add nice UX when passkey is not found in that RP ID by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2754
- Bump astro from 4.16.1 to 4.16.17 by @dependabot in https://github.com/dfinity/internet-identity/pull/2756
- Append https:// in front of RP ID when excluding devices by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2755
- Bump astro from 4.16.17 to 4.16.18 by @dependabot in https://github.com/dfinity/internet-identity/pull/2758
- UI Page to add current device to the current origin by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2757
- Add new field to LoginSuccess to show add current device screen by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2759
- Fix authentication with (older) identities that have devices without a (valid) credential id. by @sea-snake in https://github.com/dfinity/internet-identity/pull/2760
- Implement mock openID actor methods by @sea-snake in https://github.com/dfinity/internet-identity/pull/2761
- Implement OpenID add/remove accounts in identity management by @sea-snake in https://github.com/dfinity/internet-identity/pull/2762
- Fix e2e CI job by downgrading runner by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2764
- Add readme.md to BOT_APPROVED_FILES by @lmuntaner in https://github.com/dfinity/internet-identity/pull/2763
- Update release in README by @gix-bot in https://github.com/dfinity/internet-identity/pull/2747
- Update dapps list by @gix-bot in https://github.com/dfinity/internet-identity/pull/2765
- Yubikey test feature by @LXIF in https://github.com/dfinity/internet-identity/pull/2769
- Andri/yubikey test feature by @LXIF in https://github.com/dfinity/internet-identity/pull/2772

**Full Changelog**: https://github.com/dfinity/internet-identity/compare/release-2024-12-13...release-2025-01-10

## Build flavors

For more information please see the [Build flavors](https://github.com/dfinity/internet-identity/tree/release-2025-01-10#build-features-and-flavors) section of the README.

| Filename                                                                                                                                                       | sha256 (links to CI Run)                                                                                                                                             |
| -------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [internet_identity_production.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-01-10/internet_identity_production.wasm.gz) | [`377b17ef2f5c83ecdc78292373fbb3a1e632755b4de3910a8d96d0e64d657753`](https://github.com/dfinity/internet-identity/actions/runs/12709783528/job/35429516842#step:7:1) |
| [internet_identity_dev.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-01-10/internet_identity_dev.wasm.gz)               | [`4ead4e43ea6e23bbc5300712c4b4cb951f51ff3a101beb732b8352921a1709df`](https://github.com/dfinity/internet-identity/actions/runs/12709783528/job/35429517542#step:7:1) |
| [internet_identity_test.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-01-10/internet_identity_test.wasm.gz)             | [`e57d3d104252d970575015a3b1b89269d17b27b720205cd759292640a1f992a1`](https://github.com/dfinity/internet-identity/actions/runs/12709783528/job/35429517218#step:7:1) |
| [archive.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-01-10/archive.wasm.gz)                                           | [`ee4c919f138ae6f6dab353bfd01f467ffa4a4d6a2967d5575af0045a06b978ee`](https://github.com/dfinity/internet-identity/actions/runs/12709783528/job/35429516468#step:6:1) |
| [vc_demo_issuer.wasm.gz](https://github.com/dfinity/internet-identity/releases/download/release-2025-01-10/vc_demo_issuer.wasm.gz)                             | [`5d9cb4b5a89d2fda42906f0c5eb260ec2770980b1a94324c7ee219e10bf5f75d`](https://github.com/dfinity/internet-identity/actions/runs/12709783528/job/35429496859#step:8:1) |

## Wasm Verification

To build the wasm modules yourself and verify their hashes, run the following commands from the root of the [Internet Identity repository](https://github.com/dfinity/internet-identity):

```
git pull # to ensure you have the latest changes.
git checkout 8671b4f4c61a3b641a08b2ffb25053201e37e512
./scripts/verify-hash --ii-hash 377b17ef2f5c83ecdc78292373fbb3a1e632755b4de3910a8d96d0e64d657753
```

Make sure to compare the hashes also with the proposal payload when verifying canister upgrade proposals.

## Argument Verification

Run the follwing command to verify the upgrade argument hash:

```
didc encode '(null)' | xxd -r -p | sha256sum
```

The output should match the argument hash.
