#!/usr/bin/env bash

# Compute the size of the file and write it to 'notes/file-size/<filename>'

set -euo pipefail

filename=${INPUT_FILE:?"No filename provided"}
save=${INPUT_SAVE}
name="$filename"
ref="notes/file-size/$name"

# Pipe to xargs to trim whitespaces
size="$(wc -c <"$filename" | xargs)"

>&2 echo "Size: '$size'"
echo "::set-output name=size::$size"

logged() {
    ( set -x && "$@" )
}

if [ "$save" = "true" ]
then
    >&2 echo "Saving size to git notes"
    git config user.name "file-size action"
    git config user.email "<>"

    logged git fetch origin "refs/$ref:refs/$ref" || echo "could not fetch $ref, assuming doesn't exist yet"
    logged git notes --ref "$ref" add --file <(echo -n "$size")

    logged git push origin "refs/$ref"
fi
