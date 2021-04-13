#!/usr/bin/env bash

IDP_DIR="$(dirname "$0")"

cargo build --manifest-path "$IDP_DIR/Cargo.toml" --target wasm32-unknown-unknown --release

cargo install ic-cdk-optimizer --root "$IDP_DIR"/../../target
STATUS=$?

if [ "$STATUS" -eq "0" ]; then
      "$IDP_DIR"/../../target/bin/ic-cdk-optimizer \
      "$IDP_DIR"/../../target/wasm32-unknown-unknown/release/idp_service.wasm \
      -o "$IDP_DIR"/../../target/wasm32-unknown-unknown/release/idp_service.wasm

  true
else
  echo Could not install ic-cdk-optimizer.
  false
fi
