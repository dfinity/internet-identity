#!/usr/bin/env bash
set -euo pipefail

# Try to run from the script location
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

npm ci
npm run build
cargo build --release --target wasm32-unknown-unknown
ic-cdk-optimizer "target/wasm32-unknown-unknown/release/test_app.wasm" -o "./test_app.wasm"
