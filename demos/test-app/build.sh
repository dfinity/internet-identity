#!/usr/bin/env bash
set -euo pipefail

# Try to run from the script location
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

pushd ../../
npm run --workspace ./demos/test-app build
popd

cargo build --release --target wasm32-unknown-unknown
ic-wasm "target/wasm32-unknown-unknown/release/test_app.wasm" -o "./test_app.wasm" shrink
ic-wasm test_app.wasm -o test_app.wasm metadata candid:service -f test_app.did -v public
# indicate support for certificate version 1 and 2 in the canister metadata
ic-wasm test_app.wasm -o test_app.wasm metadata supported_certificate_versions -d "1,2" -v public
