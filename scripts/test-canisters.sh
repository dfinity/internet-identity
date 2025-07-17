#!/bin/bash
set -euo pipefail

POCKET_IC_SERVER_VERSION=6.0.0
POCKET_IC_SERVER_PATH="pocket-ic"
PREVIOUS_II_WASM_PATH="internet_identity_previous.wasm.gz"
PREVIOUS_ARCHIVE_WASM_PATH="archive_previous.wasm.gz"

# Run the script from the main directory
SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPTS_DIR/.."

RUNNER_OS=${RUNNER_OS:-}
if [[ $OSTYPE == "linux-gnu"* ]] || [[ $RUNNER_OS == "Linux" ]]; then
  PLATFORM=linux
elif [[ $OSTYPE == "darwin"* ]] || [[ $RUNNER_OS == "macOS" ]]; then
  PLATFORM=darwin
else
  echo "OS not supported: ${OSTYPE:-$RUNNER_OS}"
  exit 1
fi

# Parse arguments for --no-build flag
NO_BUILD=false
filtered_args=()
for arg in "$@"; do
  if [ "$arg" != "--no-build" ]; then
    filtered_args+=("$arg")
  else
    NO_BUILD=true
  fi
done

if [ -f "./$PREVIOUS_II_WASM_PATH" ]; then
  echo "Using previous II wasm."
else
  echo "Downloading previous II wasm."
curl -sSL https://github.com/dfinity/internet-identity/releases/latest/download/internet_identity_test.wasm.gz -o ${PREVIOUS_II_WASM_PATH}
fi

if [ -f "./$PREVIOUS_ARCHIVE_WASM_PATH" ]; then
  echo "Using previous Archive wasm."
else
  echo "Downloading previous Archive wasm."
  curl -sSL https://github.com/dfinity/internet-identity/releases/latest/download/archive.wasm.gz -o ${PREVIOUS_ARCHIVE_WASM_PATH}
fi

if [ ! -f "$POCKET_IC_SERVER_PATH" ]; then
  echo "Downloading PocketIC."
  curl -sSL https://github.com/dfinity/pocketic/releases/download/${POCKET_IC_SERVER_VERSION}/pocket-ic-x86_64-${PLATFORM}.gz -o ${POCKET_IC_SERVER_PATH}.gz
  gunzip ${POCKET_IC_SERVER_PATH}.gz
  chmod +x ${POCKET_IC_SERVER_PATH}
else
  echo "PocketIC server already exists, skipping download."
fi

# Build II and Archive unless --no-build is specified
if [ "$NO_BUILD" = false ]; then
  echo "Building Internet Identity and Archive..."
  # Build II
  II_FETCH_ROOT_KEY=1 II_DUMMY_CAPTCHA=1 ./scripts/build --internet-identity

  # Build Archive
  ./scripts/build --archive
else
  echo "Skipping build due to --no-build flag."
fi

# Run tests

echo "Running integration tests."
cargo test "${filtered_args[@]:-}"
