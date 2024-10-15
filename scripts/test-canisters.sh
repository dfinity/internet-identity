#!/bin/bash
set -euo pipefail

POCKET_IC_SERVER_VERSION=6.0.0
POCKET_IC_SERVER_PATH="pocket-ic"
PREVIOUS_II_WASM_PATH="internet_identity_previous.wasm.gz"
PREVIOUS_ARCHIVE_WASM_PATH="archive_previous.wasm.gz"

# Check if the script is run from the root of the project
project_root=$(git rev-parse --show-toplevel 2>/dev/null)
if [ "$project_root" != "$(pwd)" ]; then
  echo "Please run this script from the root of the project."
  exit 1
fi

RUNNER_OS=${RUNNER_OS:-}
if [[ $OSTYPE == "linux-gnu"* ]] || [[ $RUNNER_OS == "Linux" ]]; then
  PLATFORM=linux
elif [[ $OSTYPE == "darwin"* ]] || [[ $RUNNER_OS == "macOS" ]]; then
  PLATFORM=darwin
else
  echo "OS not supported: ${OSTYPE:-$RUNNER_OS}"
  exit 1
fi

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

# Build II
II_FETCH_ROOT_KEY=1 II_DUMMY_CAPTCHA=1 ./scripts/build --internet-identity

# Build Archive
./scripts/build --archive

# Run tests

echo "Running integration tests."
cargo test "${@}"
