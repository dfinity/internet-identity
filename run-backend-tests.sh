#!/usr/bin/env bash

#
# This script runs the backend tests against the backend.
# Does not build the wasm to for you, please build when needed
# This uses nix and the nix cache to fetch the build script for you.
# Normally, nothing should be built by this command.
#

echo "Running against ./target/wasm32-unknown-unknown/release/idp_service.wasm"
echo "Did you build? If not, Ctrl-C and run ./src/idp_service/build.sh now!"

nix run '((import (builtins.fetchGit { url = "git@github.com:dfinity-lab/ic-ref"; ref = "joachim/ic-idp-tester";}) {}).ic-ref)' -c ic-idp-test --wasm ./target/wasm32-unknown-unknown/release/idp_service.wasm "$@"
