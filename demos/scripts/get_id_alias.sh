#!/usr/bin/env bash

set -euo pipefail

SOURCE_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
source "$SOURCE_DIR/lib.sh"

RELYING_PARTY="OpenChat"
ISSUER="Test IC issuer"
info "Requesting id_alias for anchor $ANCHOR, relying party [$RELYING_PARTY], and issuer [$ISSUER]"
ID_ALIAS_REQ="( record { identity_number = ${ANCHOR}; issuer = \"${ISSUER}\"; relying_party = \"${RELYING_PARTY}\"})"
dfx canister call internet_identity prepare_id_alias "$ID_ALIAS_REQ"
dfx canister call internet_identity get_id_alias "$ID_ALIAS_REQ"