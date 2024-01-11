#!/usr/bin/env bash
set -euo pipefail


# Make sure we always run from the issuer root
VC_ISSUER_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$VC_ISSUER_DIR"

# Build the app
pushd ../../
npm run --workspace ./demos/vc_issuer build
popd

cargo build --release --target wasm32-unknown-unknown --manifest-path ./Cargo.toml -j1
ic-wasm "target/wasm32-unknown-unknown/release/vc_issuer.wasm" -o "./vc_demo_issuer.wasm" shrink
ic-wasm vc_demo_issuer.wasm -o vc_demo_issuer.wasm metadata candid:service -f vc_demo_issuer.did -v public
# indicate support for certificate version 1 and 2 in the canister metadata
ic-wasm vc_demo_issuer.wasm -o vc_demo_issuer.wasm metadata supported_certificate_versions -d "1,2" -v public
gzip --no-name --force "vc_demo_issuer.wasm"

