#!/usr/bin/env bash
set -euo pipefail

npm ci
npm run build
cargo build --release --target wasm32-unknown-unknown
ic-cdk-optimizer "target/wasm32-unknown-unknown/release/selenium_test_app.wasm" -o "./selenium_test_app.wasm"