#!/usr/bin/env bash
set -euo pipefail

#########
# USAGE #
#########

function title() {
    echo "Build issuer canister" >&2
}

function usage() {
    cat >&2 << EOF

Usage:
  $0 [--exclude-custom-origin] [-o FILENAME}

Options:
  --exclude-custom-origin         When set, the canister will NOT support querying its derivation origin
  -o                              When set, specifies the file to write the built Wasm to
EOF
}

BUILD_OUTPUT=
ISSUER_FEATURES=()

while [[ $# -gt 0  ]]
do
    case "$1" in
        -h|--help)
            title
            usage
            exit 0
            ;;
        -o)
            BUILD_OUTPUT="${2:?missing value for '-o'}"
            BUILD_OUTPUT="$(realpath $(dirname "$BUILD_OUTPUT"))/$(basename "$BUILD_OUTPUT")"
            echo "build output is '$BUILD_OUTPUT'"
            shift; # shift past -o and value
            shift;
            ;;
        --exclude-custom-origin)
            ISSUER_FEATURES+=("exclude_custom_origin")
            shift; # shift past --exclude-custom-origin
            ;;
        *)
            echo "ERROR: unknown argument $1"
            usage
            echo
            echo "Use '$0 --help' for more information"
            exit 1
            ;;
    esac
done

# Make sure we always run from the issuer root
VC_ISSUER_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$VC_ISSUER_DIR"

# Build the app
pushd ../../
npm run --workspace ./demos/vc_issuer build
popd

tmp_wasm=$(mktemp)

cargo build --release \
    --features "${ISSUER_FEATURES[*]:-}" \
    --target wasm32-unknown-unknown --manifest-path ./Cargo.toml
ic-wasm "target/wasm32-unknown-unknown/release/vc_issuer.wasm" -o "$tmp_wasm" shrink
ic-wasm "$tmp_wasm" -o "$tmp_wasm" metadata candid:service -f vc_demo_issuer.did -v public
# indicate support for certificate version 1 and 2 in the canister metadata
ic-wasm "$tmp_wasm" -o "$tmp_wasm" metadata supported_certificate_versions -d "1,2" -v public

# NOTE: this automatically removes tmp_wasm
gzip --no-name --force "$tmp_wasm" --to-stdout > "${BUILD_OUTPUT:-./vc_demo_issuer.wasm.gz}"
