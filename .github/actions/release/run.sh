#!/usr/bin/env bash
#
# This script creates a nice markdown table with the build artifacts' shas, links to download
# the Wasm modules, and links to the `sha256sum` steps in CI for verification.

set -euo pipefail

PRODUCTION_ASSET=${INPUT_PRODUCTION_ASSET:?No production asset specified}
RELEASE_TAG=${RELEASE_TAG:-${GITHUB_REF_NAME:?No value for tag}}

# Starting the "intro" section where we display a short intro
section_intro=$(mktemp)
cat > "$section_intro" << EOF
This is Internet Identity release [$RELEASE_TAG](https://github.com/dfinity/internet-identity/releases/tag/$RELEASE_TAG) for commit [$GITHUB_SHA](https://github.com/dfinity/internet-identity/commit/$GITHUB_SHA).
EOF

# Starting the "build flavors" section where we add the shas of all input assets
section_build_flavors=$(mktemp)

# Start the body with a paragraph and table headers
# NOTE: throughout the doc we link to the current release (not to master) because things might
# change
cat > "$section_build_flavors" <<EOF
## Build flavors

For more information please see the [Build flavors](https://github.com/dfinity/internet-identity/tree/$RELEASE_TAG#build-features-and-flavors) section of the README.

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
./scripts/docker-build
sha256sum internet_identity.wasm.gz
./scripts/docker-build --archive
sha256sum archive.wasm.gz
\`\`\`
EOF

# Read all "INPUT_ASSETS" (where "ASSETS" is the input specified in action.yml)
# https://docs.github.com/en/actions/creating-actions/metadata-syntax-for-github-actions#example-specifying-inputs
while IFS= read -r filename
do
    if [ -z "$filename" ]; then continue; fi
    >&2 echo working on asset "$filename"

    # Find out the Job ID
    # XXX: Unfortunately GitHub actions doesn't give us a way to find out the Job ID explicitely.
    # Instead, we find the job name that includes "$filename" without the .wasm or .wasm.gz extension and assume that's the Job ID.
    # This works because our jobs contain the filename without extension
    # (either added manually or by build matrix which takes in the filename as argument and adds it to the job name).
    # https://github.community/t/get-action-job-id/17365/7
    if [ -z "${INPUT_WORKFLOW_JOBS:-}" ]
    then
        # if not running on GitHub (e.g. locally for debugging), skip
        html_url="https://example.com"
        step="step"
    else
        job_id=$(jq -cMr --arg search_string "${filename%%.*}" \
                '.[] | select(.name | contains($search_string)) | .id'\
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

    # Mention production asset in intro section
    if [[ "$filename" == "$PRODUCTION_ASSET" ]]
    then
        shasum -a 256 "$filename"  | sed -r "s%^([a-z0-9]+)[[:space:]][[:space:]](.*)$%The sha256 of production asset [\2]($download_link) is [\1]($run_link).%" >> "$section_intro"
    fi
done <<< "$INPUT_ASSETS"

>&2 echo "Creating release notes"

body=$(mktemp)
>&2 echo "Using '$body' for body"
cat "$section_intro" >> "$body" && echo >> "$body" && rm "$section_intro"
echo "$INPUT_CHANGELOG" >> "$body"
cat "$section_build_flavors" >> "$body" && echo >> "$body" && rm "$section_build_flavors"
cat "$section_wasm_verification" >> "$body" && echo >> "$body" && rm "$section_wasm_verification"

>&2 echo "body complete:"
cat "$body"

echo "notes-file=$body" >> "$GITHUB_OUTPUT"
