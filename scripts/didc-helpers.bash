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

# Ensure $PINNED_DIDC is set to the pinned didc binary. Downloads on
# first use; subsequent calls are a no-op (file-exists check).
#
# Exits non-zero on:
#   - missing or empty `.didc-release`
#   - unsupported host OS (we only ship for macOS + linux64)
#   - download failure
ensure_pinned_didc() {
    local scripts_dir root_dir release platform url cache
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

    cache="$root_dir/.icp/cache/didc/didc-$release-$platform"
    if [ ! -x "$cache" ]; then
        echo "Downloading pinned didc $release ($platform) → $cache ..." >&2
        mkdir -p "$(dirname "$cache")"
        url="https://github.com/dfinity/candid/releases/download/$release/didc-$platform"
        # Download to `.tmp` and atomically rename so a concurrent
        # script run never sees a half-written binary on its $cache check.
        if ! curl --location --fail --silent --show-error "$url" -o "$cache.tmp"; then
            rm -f "$cache.tmp"
            echo "Error: failed to download $url" >&2
            return 1
        fi
        chmod +x "$cache.tmp"
        mv "$cache.tmp" "$cache"
    fi

    PINNED_DIDC="$cache"
    export PINNED_DIDC
}
