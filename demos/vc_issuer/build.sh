#!/usr/bin/env bash
set -euo pipefail

# Try to run from the script location
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

npm ci
npm run build
cargo build --release --target wasm32-unknown-unknown --manifest-path ./Cargo.toml -j1
ic-wasm "target/wasm32-unknown-unknown/release/vc_issuer.wasm" -o "./vc_issuer.wasm" shrink
ic-wasm vc_issuer.wasm -o vc_issuer.wasm metadata candid:service -f vc_issuer.did -v public
gzip --no-name --force --keep "vc_issuer.wasm"

