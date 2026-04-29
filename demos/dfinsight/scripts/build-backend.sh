#!/usr/bin/env bash
set -euo pipefail

# Build the Motoko backend canister into src/dfinsight_backend/.build/.
#
# Requires `mops` (npm i -g ic-mops) and a moc toolchain installed via
# `mops toolchain use moc`. Both are checked into mops.toml so a fresh
# clone only needs `mops install` once.

cd "$(dirname "$0")/.."
cd src/dfinsight_backend

mops install

mkdir -p .build

MOC=$(mops toolchain bin moc)
SOURCES=$(mops sources)

# `--release` strips debug info, `--public-metadata candid:service`
# embeds the .did so dapps can fetch it from the canister.
$MOC \
  $SOURCES \
  --release \
  --public-metadata candid:service \
  --idl \
  -o .build/dfinsight_backend.wasm \
  main.mo

# moc emits the .did next to the wasm — copy it where icp-cli expects.
if [ -f .build/dfinsight_backend.did ]; then
  : # already in place
elif [ -f .build/main.did ]; then
  mv .build/main.did .build/dfinsight_backend.did
fi
