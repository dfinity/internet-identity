#!/usr/bin/env bash
#
# This script creates a nice markdown table with the build artifacts' shas, links to download
# the Wasm modules, and links to the `sha256sum` steps in CI for verification.

set -euo pipefail

RELEASE_TAG=${RELEASE_TAG:-${GITHUB_REF_NAME:?No value for tag}}

# Starting the "intro" section where we display a short intro
section_intro=$(mktemp)
cat > "$section_intro" << EOF
This is Internet Identity release [$RELEASE_TAG](https://github.com/dfinity/internet-identity/releases/tag/$RELEASE_TAG) for commit [$GITHUB_SHA](https://github.com/dfinity/internet-identity/commit/$GITHUB_SHA).
EOF

# "Try it out" section so readers (and downstream forum/proposal tooling) can
# reach staging and the test application without hunting for links.
section_try_it_out=$(mktemp)
cat > "$section_try_it_out" <<EOF
## Try it out
* Staging: https://beta.id.ai
* Testing application: https://try.id.ai
EOF

# Starting the artifacts section where we add the shas of all input assets
section_build_flavors=$(mktemp)

# Start the body with a paragraph and table headers
# NOTE: throughout the doc we link to the current release (not to master) because things might
# change
cat > "$section_build_flavors" <<EOF
## Artifacts

| Filename | sha256 (links to CI Run) |
| --- | --- |
EOF

# Starting the "wasm verification" section where we explain how to build the production assets
section_wasm_verification=$(mktemp)
cat > "$section_wasm_verification" <<EOF
## Wasm Verification

To build the wasm modules yourself and verify their hashes, run the following commands from the root of the [Internet Identity repository](https://github.com/dfinity/internet-identity):
\`\`\`
git pull # to ensure you have the latest changes.
git checkout $GITHUB_SHA
./scripts/verify-hash \
    --ii-hash $(shasum -a 256 "internet_identity_backend.wasm.gz" | cut -d ' ' -f1) \
    --iife-hash $(shasum -a 256 "internet_identity_frontend.wasm.gz" | cut -d ' ' -f1) \
    --archive-hash $(shasum -a 256 "archive.wasm.gz" | cut -d ' ' -f1)
\`\`\`

Make sure to compare the hashes also with the proposal payload when verifying canister upgrade proposals.
EOF

# Read all "INPUT_ASSETS" (where "ASSETS" is the input specified in action.yml)
# https://docs.github.com/en/actions/creating-actions/metadata-syntax-for-github-actions#example-specifying-inputs
while IFS= read -r filename
do
    if [ -z "$filename" ]; then continue; fi
    >&2 echo working on asset "$filename"

    # For each asset, find the ID of the job that created the asset and find the step that
    # printed the asset's checksum (will be linked in the notes)
    #
    # XXX: Unfortunately GitHub actions doesn't give us a way to find out the Job ID explicitly.
    # Instead, we find the job name that includes "$filename" and assume that's the Job ID.
    # We try matching the full filename first (for matrix jobs), then fall back to the stem
    # (for non-matrix jobs like docker-build-archive).
    # https://github.community/t/get-action-job-id/17365/7
    if [ -z "${INPUT_WORKFLOW_JOBS:-}" ]
    then
        # if not running on GitHub (e.g. locally for debugging), skip
        html_url="https://example.com"
        step="step"
    else
        # Try matching by full filename first (for matrix jobs that include the
        # full filename), then fall back to matching by stem (for non-matrix jobs
        # like docker-build-archive).
        job_id=$(jq -cMr --arg full "$filename" --arg stem "${filename%%.*}" \
                '. as $jobs | [$jobs[] | select(.name | contains($full))] | if length > 0 then .[0].id else ($jobs | map(select(.name | contains($stem))) | if length > 0 then .[0].id else error("no job found for \($full) / \($stem)") end) end' \
                <<< "$INPUT_WORKFLOW_JOBS")
                        >&2 echo "Found job id: $job_id"

        # Now get the URL that we'll link to for verification of the sha
        html_url=$(jq -cMr --argjson job_id "$job_id" \
                  '.[] | select(.id == $job_id) | .html_url'\
                  <<< "$INPUT_WORKFLOW_JOBS")
                      >&2 echo "Found html_url: $html_url"

        # Additionally grab the step number of the 'sha256sum' step
        step=$(jq -cMr --argjson job_id "$job_id" \
              --arg filename "$filename" \
              '.[] | select(.id == $job_id) | .steps[] | select(.name | endswith("sha256sum " + $filename)) | .number'\
              <<< "$INPUT_WORKFLOW_JOBS")
                  >&2 echo "Found step: $step"
    fi

    # Prepare the cells:
    # | [filename.wasm](<download link>) | [<sha256>](<run link>) |
    download_link="https://github.com/dfinity/internet-identity/releases/download/$RELEASE_TAG/$filename"
    download="[\2]($download_link)"

    run_link="$html_url#step:$step:1"
    # shellcheck disable=SC2016
    sha='[`\1`]'"($run_link)"

    # Get the shasum and capture the sha (using only POSIX sed)
    shasum -a 256 "$filename"  | sed -r "s%^([a-z0-9]+)[[:space:]][[:space:]](.*)$%|$download|$sha|%" >> "$section_build_flavors"
done <<< "$INPUT_ASSETS"

>&2 echo "Creating release notes"

# Source paths for filtering commits by canister
BACKEND_PATHS=(
  src/internet_identity/
  src/internet_identity_interface/
  src/archive/
  src/asset_util/
  src/canister_tests/
)
FRONTEND_PATHS=(
  src/internet_identity_frontend/
  src/frontend/
  src/lingui-svelte/
  src/vite-plugins/
)

# Resolve a proposal tag to the release-* tag pointing at the same commit.
# Falls back to the proposal tag itself if no matching release tag is found.
resolve_release_tag() {
  local proposal_tag="$1"
  local commit
  commit=$(git rev-parse "$proposal_tag" 2>/dev/null) || { echo "$proposal_tag"; return; }
  local release_tag
  release_tag=$(git tag --points-at "$commit" | grep "^release-" | sort | head -1 || true)
  echo "${release_tag:-$proposal_tag}"
}

# Generate per-canister "What's Changed" section using proposal tags as baselines.
# Falls back to the GitHub-generated changelog if no proposal tags exist.
section_changelog=$(mktemp)
LAST_BACKEND_TAG=$(git tag -l "proposal-backend-*" | sort -V | tail -1 || true)
LAST_FRONTEND_TAG=$(git tag -l "proposal-frontend-*" | sort -V | tail -1 || true)

if [ -n "$LAST_BACKEND_TAG" ] || [ -n "$LAST_FRONTEND_TAG" ]; then
  >&2 echo "Generating per-canister changelog (backend baseline: ${LAST_BACKEND_TAG:-none}, frontend baseline: ${LAST_FRONTEND_TAG:-none})"
  echo "## What's Changed" >> "$section_changelog"
  echo "" >> "$section_changelog"

  if [ -n "$LAST_BACKEND_TAG" ]; then
    backend_log=$(git log --format='* %s' --no-merges "${LAST_BACKEND_TAG}..${RELEASE_TAG}" -- "${BACKEND_PATHS[@]}" || true)
    if [ -n "$backend_log" ]; then
      LAST_BACKEND_RELEASE=$(resolve_release_tag "$LAST_BACKEND_TAG")
      {
        echo "### Backend Changes"
        echo "_Since [${LAST_BACKEND_RELEASE}](https://github.com/dfinity/internet-identity/releases/tag/${LAST_BACKEND_RELEASE})_"
        echo ""
        echo "$backend_log"
        echo ""
        echo "**Full Changelog**: [${LAST_BACKEND_RELEASE}...${RELEASE_TAG}](https://github.com/dfinity/internet-identity/compare/${LAST_BACKEND_RELEASE}...${RELEASE_TAG})"
        echo ""
      } >> "$section_changelog"
    fi
  fi

  if [ -n "$LAST_FRONTEND_TAG" ]; then
    frontend_log=$(git log --format='* %s' --no-merges "${LAST_FRONTEND_TAG}..${RELEASE_TAG}" -- "${FRONTEND_PATHS[@]}" || true)
    if [ -n "$frontend_log" ]; then
      LAST_FRONTEND_RELEASE=$(resolve_release_tag "$LAST_FRONTEND_TAG")
      {
        echo "### Frontend Changes"
        echo "_Since [${LAST_FRONTEND_RELEASE}](https://github.com/dfinity/internet-identity/releases/tag/${LAST_FRONTEND_RELEASE})_"
        echo ""
        echo "$frontend_log"
        echo ""
        echo "**Full Changelog**: [${LAST_FRONTEND_RELEASE}...${RELEASE_TAG}](https://github.com/dfinity/internet-identity/compare/${LAST_FRONTEND_RELEASE}...${RELEASE_TAG})"
        echo ""
      } >> "$section_changelog"
    fi
  fi
else
  >&2 echo "No proposal tags found, using GitHub-generated changelog"
  echo "$INPUT_CHANGELOG" > "$section_changelog"
fi

body=$(mktemp)
>&2 echo "Using '$body' for body"
cat "$section_intro" >> "$body" && echo >> "$body" && rm "$section_intro"
cat "$section_try_it_out" >> "$body" && echo >> "$body" && rm "$section_try_it_out"
cat "$section_changelog" >> "$body" && echo >> "$body" && rm "$section_changelog"
cat "$section_build_flavors" >> "$body" && echo >> "$body" && rm "$section_build_flavors"
cat "$section_wasm_verification" >> "$body" && echo >> "$body" && rm "$section_wasm_verification"

>&2 echo "body complete:"
cat "$body"

echo "notes-file=$body" >> "$GITHUB_OUTPUT"
