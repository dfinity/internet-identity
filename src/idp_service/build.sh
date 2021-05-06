#!/usr/bin/env bash
set -euo pipefail

# Compile frontend assets to dist
echo Compiling frontend assets
npm run build

IDP_DIR="$(dirname "$0")"
TARGET="wasm32-unknown-unknown"

cargo build --manifest-path "$IDP_DIR/Cargo.toml" --target $TARGET --release -j1

cargo install ic-cdk-optimizer --root "$IDP_DIR"/../../target
STATUS=$?

if [ "$STATUS" -eq "0" ]; then
      "$IDP_DIR"/../../target/bin/ic-cdk-optimizer \
      "$IDP_DIR/../../target/$TARGET/release/idp_service.wasm" \
      -o "$IDP_DIR/../../target/$TARGET/release/idp_service.wasm"

  true
else
  echo Could not install ic-cdk-optimizer.
  false
fi
