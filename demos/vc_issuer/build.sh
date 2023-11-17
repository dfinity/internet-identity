#!/usr/bin/env bash
set -euo pipefail


# Make sure we always run from the issuer root
VC_ISSUER_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$VC_ISSUER_DIR"

cargo build --release --target wasm32-unknown-unknown --manifest-path ./Cargo.toml -j1
ic-wasm "target/wasm32-unknown-unknown/release/vc_issuer.wasm" -o "./vc_issuer.wasm" shrink
ic-wasm vc_issuer.wasm -o vc_issuer.wasm metadata candid:service -f vc_issuer.did -v public
gzip --no-name --force "vc_issuer.wasm"

