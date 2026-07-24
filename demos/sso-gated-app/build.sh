#!/usr/bin/env bash
# Build the SSO-gated test app canister.
#
# Produces `sso_gated_app.wasm.gz` (and refreshes `sso_gated_app.did`)
# in this directory. Requires `mops` (https://mops.one, `npm i -g
# ic-mops`) — the Motoko compiler is resolved from the `[toolchain]`
# pin in `mops.toml`, falling back to `moc` on PATH (e.g. from dfx).
set -euo pipefail
cd "$(dirname "$0")"

if ! command -v mops >/dev/null 2>&1; then
  echo "error: mops not found. Install it with: npm i -g ic-mops" >&2
  exit 1
fi

# Download Motoko package sources (identity-attributes, core).
mops install

# Prefer the toolchain-pinned moc; fall back to whatever is on PATH.
MOC="$(mops toolchain bin moc 2>/dev/null || true)"
if [ -z "${MOC}" ]; then
  MOC="moc"
fi

SOURCES="$(mops sources)"

# shellcheck disable=SC2086
"${MOC}" ${SOURCES} src/main.mo -o sso_gated_app.wasm
# shellcheck disable=SC2086
"${MOC}" ${SOURCES} --idl src/main.mo -o sso_gated_app.did

gzip -nf sso_gated_app.wasm
echo "built sso_gated_app.wasm.gz ($(stat -c%s sso_gated_app.wasm.gz) bytes)"
