#!/usr/bin/env bash
set -euo pipefail

# Run from the script's directory, wherever it is called from.
cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# The frontend first: the canister embeds `dist/` at compile time.
npm ci --no-audit --no-fund
npm run build

cargo build --release --target wasm32-unknown-unknown
ic-wasm target/wasm32-unknown-unknown/release/notification_chat.wasm -o notification_chat.wasm shrink
ic-wasm notification_chat.wasm -o notification_chat.wasm metadata candid:service -f chat.did -v public
# Declared as Internet Identity declares its own; a gateway then asks for version 2,
# which is what the asset router certifies.
ic-wasm notification_chat.wasm -o notification_chat.wasm metadata supported_certificate_versions -d "1,2" -v public
