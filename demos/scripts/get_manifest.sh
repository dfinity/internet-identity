#!/usr/bin/env bash

set -euo pipefail

SOURCE_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
source "$SOURCE_DIR/lib.sh"

RELYING_PARTY="OpenChat"
ISSUER="Test IC issuer"
info "Requesting manifest for $ANCHOR from issuer [$ISSUER]"
REQ="(record { consent_message_request = record { preferences = record { language = \"EN-US\" } } })"
dfx canister call issuer get_manifest "$REQ"
