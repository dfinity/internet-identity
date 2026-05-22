#!/usr/bin/env bash
# Shared helper: download + cache the pinned `didc` release listed in
# `.didc-release` (the same source CI's `setup-didc` composite action
# reads), and expose it as `$PINNED_DIDC`.
#
# Every deploy / proposal script that hand-encodes Candid install args
# should source this and call `ensure_pinned_didc` before invoking the
# binary, so the encoded bytes don't drift between operators with
# different `didc` versions on `PATH`.
#
# The binary is cached under `.icp/cache/didc/` (already gitignored as
# part of the `.icp/` tool cache) keyed by release + platform, so
# multiple checkouts on the same machine share downloads.
#
# Downloads are sha256-verified against `.didc-checksums` before the
# binary becomes executable — a mismatch (or missing checksum entry)
# aborts and removes the partial download, so a tampered upstream or
# stale-pin failure mode never leaves an unverified binary on disk
# that a later run might trust by file-exists check.

# Look up the sha256 pinned for the given platform in `.didc-checksums`.
# Stdout: the lowercase hex sha256 on success; nothing on failure.
_pinned_didc_expected_sha256() {
    local checksums_file="$1"
    local platform="$2"
    if [ ! -f "$checksums_file" ]; then
        return 1
    fi
    # File format: `<platform> <sha256>` pairs, with `#`-prefixed comment
    # lines + blank lines tolerated. The awk handles inline comments by
    # only printing the second field when the first matches.
    awk -v p="$platform" '
        /^[[:space:]]*#/ { next }
        /^[[:space:]]*$/ { next }
        $1 == p { print $2; exit }
    ' "$checksums_file"
}

# Ensure $PINNED_DIDC is set to the pinned didc binary. Downloads on
# first use; subsequent calls are a no-op (file-exists check).
#
# Exits non-zero on:
#   - missing or empty `.didc-release`
#   - unsupported host OS (we only ship for macOS + linux64)
#   - download failure
#   - missing sha256 pin in `.didc-checksums`
#   - sha256 mismatch between the downloaded binary and the pin
ensure_pinned_didc() {
    local scripts_dir root_dir release platform url cache expected_sha actual_sha
    scripts_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    root_dir="$scripts_dir/.."

    if [ ! -f "$root_dir/.didc-release" ]; then
        echo "Error: $root_dir/.didc-release missing — can't pin didc version." >&2
        return 1
    fi
    # Strip comments and blank lines — matches .github/actions/setup-didc.
    release=$(sed <"$root_dir/.didc-release" 's/#.*$//' | sed '/^$/d')
    if [ -z "$release" ]; then
        echo "Error: $root_dir/.didc-release is empty after stripping comments." >&2
        return 1
    fi

    case "$(uname -s)" in
        Darwin) platform="macos" ;;
        Linux)  platform="linux64" ;;
        *)
            echo "Error: pinned didc isn't published for OS '$(uname -s)'." >&2
            echo "       The dfinity/candid release only ships macos / linux64 / arm32 — add support" >&2
            echo "       here if you need another platform." >&2
            return 1
            ;;
    esac

    expected_sha=$(_pinned_didc_expected_sha256 "$root_dir/.didc-checksums" "$platform")
    if [ -z "$expected_sha" ]; then
        echo "Error: no sha256 pin for platform '$platform' in $root_dir/.didc-checksums." >&2
        echo "       Add an entry (see the header comment in that file for instructions) before" >&2
        echo "       any caller can use the pinned didc — we refuse to execute an unverified binary." >&2
        return 1
    fi

    cache="$root_dir/.icp/cache/didc/didc-$release-$platform"
    if [ ! -f "$cache" ]; then
        echo "Downloading pinned didc $release ($platform) → $cache ..." >&2
        mkdir -p "$(dirname "$cache")"
        url="https://github.com/dfinity/candid/releases/download/$release/didc-$platform"
        # Download to `.tmp` so an interrupted download never leaves
        # `$cache` looking complete to the file-exists check above.
        if ! curl --location --fail --silent --show-error "$url" -o "$cache.tmp"; then
            rm -f "$cache.tmp"
            echo "Error: failed to download $url" >&2
            return 1
        fi
        mv "$cache.tmp" "$cache"
    fi

    # Verify the on-disk binary's sha256 against the pin on every call —
    # both cold-cache (just-downloaded) and warm-cache (long-lived) — so
    # a tampered or corrupted cached binary can't slip past the file-
    # exists short-circuit above. Cheap (~10ms for a 5MB file) and worth
    # it since the alternative is silently executing the bad binary.
    actual_sha=$(shasum -a 256 "$cache" | awk '{print $1}')
    if [ "$actual_sha" != "$expected_sha" ]; then
        echo "Error: sha256 mismatch for cached didc at $cache." >&2
        echo "       expected: $expected_sha" >&2
        echo "       got:      $actual_sha" >&2
        echo "       Removing the bad file so the next run re-downloads. If you just bumped" >&2
        echo "       .didc-release, refresh .didc-checksums to match (see the header in that file)." >&2
        rm -f "$cache"
        return 1
    fi
    chmod +x "$cache"

    PINNED_DIDC="$cache"
    export PINNED_DIDC
}
