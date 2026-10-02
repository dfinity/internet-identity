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
# The release archive and the binary extracted from it are cached under
# `.icp/cache/didc/` (already gitignored as part of the `.icp/` tool
# cache) keyed by release + target, so multiple checkouts on the same
# machine share downloads.
#
# Archives are sha256-verified against `.didc-checksums` before the
# binary is extracted — a mismatch (or missing checksum entry)
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

# Print the Rust target triple for the current host, matching the
# `didc-<target>.tar.xz` asset names on the dfinity/candid release page.
_pinned_didc_target() {
    local os arch
    case "$(uname -s)" in
        Darwin) os="apple-darwin" ;;
        Linux)  os="unknown-linux-gnu" ;;
        *) return 1 ;;
    esac
    case "$(uname -m)" in
        arm64|aarch64) arch="aarch64" ;;
        x86_64|amd64)  arch="x86_64" ;;
        *) return 1 ;;
    esac
    echo "$arch-$os"
}

# Ensure $PINNED_DIDC is set to the pinned didc binary. Downloads on
# first use; subsequent calls only re-verify the cache.
#
# Exits non-zero on:
#   - missing or empty `.didc-release`
#   - unsupported host OS / CPU (we ship for {x86_64,aarch64} × {macOS,Linux})
#   - download failure
#   - missing sha256 pin in `.didc-checksums`
#   - sha256 mismatch between the downloaded archive and the pin
ensure_pinned_didc() {
    local scripts_dir root_dir release target url cache_dir archive bin member expected_sha actual_sha
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

    if ! target=$(_pinned_didc_target); then
        echo "Error: pinned didc isn't published for '$(uname -s) $(uname -m)'." >&2
        echo "       The dfinity/candid release ships {x86_64,aarch64}-{apple-darwin,unknown-linux-gnu}" >&2
        echo "       — add support here if you need another platform." >&2
        return 1
    fi

    expected_sha=$(_pinned_didc_expected_sha256 "$root_dir/.didc-checksums" "$target")
    if [ -z "$expected_sha" ]; then
        echo "Error: no sha256 pin for target '$target' in $root_dir/.didc-checksums." >&2
        echo "       Add an entry (see the header comment in that file for instructions) before" >&2
        echo "       any caller can use the pinned didc — we refuse to execute an unverified binary." >&2
        return 1
    fi

    cache_dir="$root_dir/.icp/cache/didc/$release-$target"
    archive="$cache_dir/didc-$target.tar.xz"
    bin="$cache_dir/didc"
    member="didc-$target/didc"
    mkdir -p "$cache_dir"
    if [ ! -f "$archive" ]; then
        echo "Downloading pinned didc $release ($target) → $cache_dir ..." >&2
        url="https://github.com/dfinity/candid/releases/download/$release/didc-$target.tar.xz"
        # Download to `.tmp` so an interrupted download never leaves
        # `$archive` looking complete to the file-exists check above.
        if ! curl --location --fail --silent --show-error "$url" -o "$archive.tmp"; then
            rm -f "$archive.tmp"
            echo "Error: failed to download $url" >&2
            return 1
        fi
        mv "$archive.tmp" "$archive"
    fi

    # Verify the archive against the pin on every call, then make sure the
    # extracted binary is byte-identical to the one inside it, so a tampered
    # or corrupted cache can't slip past the file-exists short-circuit above.
    actual_sha=$(shasum -a 256 "$archive" | awk '{print $1}')
    if [ "$actual_sha" != "$expected_sha" ]; then
        echo "Error: sha256 mismatch for cached didc archive at $archive." >&2
        echo "       expected: $expected_sha" >&2
        echo "       got:      $actual_sha" >&2
        echo "       Removing the bad file so the next run re-downloads. If you just bumped" >&2
        echo "       .didc-release, refresh .didc-checksums to match (see the header in that file)." >&2
        rm -f "$archive" "$bin"
        return 1
    fi
    if ! tar -xJOf "$archive" "$member" 2>/dev/null | cmp -s - "$bin"; then
        if ! tar -xJOf "$archive" "$member" >"$bin.tmp"; then
            rm -f "$bin.tmp"
            echo "Error: failed to extract $member from $archive" >&2
            return 1
        fi
        chmod +x "$bin.tmp"
        mv "$bin.tmp" "$bin"
    fi

    PINNED_DIDC="$bin"
    export PINNED_DIDC
}
