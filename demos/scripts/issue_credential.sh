#!/usr/bin/env bash

set -euo pipefail

SOURCE_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"
source "$SOURCE_DIR/lib.sh"

RELYING_PARTY="OpenChat"
ISSUER="Test IC issuer"
ID_ALIAS="uabwj-mf4ga-w7fkm-ubymb-apo4l-nuxnz-n2pbh-u7ksv-iyshj-kjfdz-sae"
info "Requesting credential for $ANCHOR from issuer [$ISSUER]"
CREDENTIAL_REQ="( record { signed_id_alias = record {signature = blob \"\d9\d9\f7\a2kcertificate\"; id_alias = principal \"${ID_ALIAS}\"; id_dapp = principal \"u5ang-oovex-fwss5-lzr6s-j2s3i-xnl6w-ys5k6-6zge5-hr7ct-ojwwt-uae\";}; credential_spec = record {info = \"neuron owner\"} })"
dfx canister call issuer issue_credential "$CREDENTIAL_REQ"
