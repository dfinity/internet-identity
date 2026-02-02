#!/usr/bin/env bash
set -euo pipefail

# Try to run from the script location
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

pushd ../../
npm run --workspace ./demos/google-signin-demo build
popd

cargo build --release --target wasm32-unknown-unknown
ic-wasm "target/wasm32-unknown-unknown/release/google_signin_demo.wasm" -o "./google_signin_demo.wasm" shrink
ic-wasm google_signin_demo.wasm -o google_signin_demo.wasm metadata candid:service -f google_signin_demo.did -v public
# indicate support for certificate version 1 and 2 in the canister metadata
ic-wasm google_signin_demo.wasm -o google_signin_demo.wasm metadata supported_certificate_versions -d "1,2" -v public
