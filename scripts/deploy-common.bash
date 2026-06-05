#!/usr/bin/env bash
# Shared helpers for deploy-pr-to-beta and deploy-local-to-beta.
#
# Responsibilities:
# - CLI arg parsing for staging selection (-sa/-sb/-sc/--staging custom),
#   end selection (-fe/-be/--end), dry-run, no-checks, and (for the local
#   script) the rebuild flags.
# - Reachability + consistency checks against the selected staging canisters.
# - Interactive prompting for the install-arg fields humans should review on
#   each upgrade:
#     * `backend_origin` and `related_origins` (FE + BE) — staging environments
#       often front custom domains in addition to the canister-default URLs,
#       and silently overwriting the on-chain lists has bitten us in the past.
#     * the behavior knobs `dev_csp` / `dummy_auth` / `analytics_config` (FE).
#   FE defaults are parsed from the canister's `/.config` Candid-text endpoint,
#   which faithfully exposes its required (non-opt) record. The BE's
#   `/.config.did.bin` only mirrors a `synchronized` subset of init args
#   (`openid_configs`, …) — `backend_origin` / `related_origins` aren't in
#   that subset — so the BE prompts default to `null` (= preserve under the
#   `opt`-field semantics of `InternetIdentityInit`'s `post_upgrade`) and the
#   operator types `opt "..."` to set explicit new values.
# - Install-arg builders that emit Candid text for each canister.
# - An icp install runner that honours --dry-run.
#
# Globals set by parse_common_args (and expected by later helpers):
#   STAGING_NAME       : "a" | "b" | "c" | "custom"
#   BE_ID, FE_ID       : principals (text form)
#   BE_URL, FE_URL     : https://... URLs
#   DEPLOY_FE          : true | false
#   DEPLOY_BE          : true | false
#   REBUILD_FE         : true | false  (deploy-local-to-beta only)
#   REBUILD_BE         : true | false  (deploy-local-to-beta only)
#   DRY_RUN            : true | false
#   NO_CHECKS          : true | false
#   REMAINING_ARGS[]   : leftover positional args (deploy-pr-to-beta reads PR#)

readonly PROXY_CANISTER_ID="cvthj-wyaaa-aaaad-aaaaq-cai"
readonly IC_NETWORK="ic"

# Default DoH allowlist for the email-recovery flow — single source
# of truth in `default-doh-domains.bash`, shared with
# `make-upgrade-proposal`. See that file for the audit notes.
# Override per-deploy via `--doh-domains <comma-list>`; empty list
# disables the DoH path entirely (DNSSEC-only).
# shellcheck source=default-doh-domains.bash
source "$(dirname "${BASH_SOURCE[0]}")/default-doh-domains.bash"

# Source the IANA fetcher so build_be_install_arg can reach it.
# shellcheck source=fetch-iana-root-anchors.bash
source "$(dirname "${BASH_SOURCE[0]}")/fetch-iana-root-anchors.bash"

# Pinned-didc helper (shared with make-upgrade-proposal). Exposes
# `ensure_pinned_didc` + `$PINNED_DIDC` so every script encodes /
# decodes Candid against the same dfinity/candid release listed in
# `.didc-release`, instead of whatever's on `PATH`.
# shellcheck source=didc-helpers.bash
source "$(dirname "${BASH_SOURCE[0]}")/didc-helpers.bash"

# -------------------------
# Staging environments
# -------------------------
staging_be_id() {
    case "$1" in
        a) echo "fgte5-ciaaa-aaaad-aaatq-cai" ;;
        b) echo "jqajs-xiaaa-aaaad-aab5q-cai" ;;
        c) echo "y2aaj-miaaa-aaaad-aacxq-cai" ;;
        *) return 1 ;;
    esac
}
staging_fe_id() {
    case "$1" in
        a) echo "gjxif-ryaaa-aaaad-ae4ka-cai" ;;
        b) echo "uhh2r-oyaaa-aaaad-agbva-cai" ;;
        c) echo "uag4f-daaaa-aaaad-agbvq-cai" ;;
        *) return 1 ;;
    esac
}

# Default public URL for any IC canister.
canister_default_url() {
    echo "https://$1.icp0.io"
}

# -------------------------
# Interactive / CI detection
# -------------------------
# True if a user terminal is attached (we can prompt), false in CI / piped.
#
# We check whether `/dev/tty` is openable rather than `[ -t 0/1 ]` because
# prompt_default is typically called inside `$(...)` to capture the answer;
# inside that subshell stdout is a pipe (not a tty) even when the user is
# sitting at a real terminal. `/dev/tty` stays accessible regardless, as
# long as the process has a controlling terminal — which is exactly the
# condition that makes prompting safe.
is_interactive() {
    [ -r /dev/tty ] && [ -w /dev/tty ]
}

# Emit an error that explains this script needs a human.
die_non_interactive() {
    local reason="$1"
    echo "Error: non-interactive invocation but $reason" >&2
    echo "       These scripts are intended to be run from a terminal. For CI-like" >&2
    echo "       use, run them locally and set --no-checks to skip reachability /" >&2
    echo "       consistency prompts; the FE install-arg prompts have no bypass." >&2
    exit 1
}

# Interactive y/N prompt. Returns 0 on yes, 1 on no. Errors out when stdin
# isn't a tty (so CI fails loudly instead of hanging).
confirm_yn() {
    local prompt="$1"
    if ! is_interactive; then
        die_non_interactive "a [y/N] confirmation is required: $prompt"
    fi
    local reply
    read -r -p "$prompt [y/N]: " reply </dev/tty
    [[ "$reply" =~ ^[Yy] ]]
}

# Prompt with a default; empty input keeps the default. Prints the chosen
# value on stdout.
prompt_default() {
    local label="$1"
    local default="$2"
    if ! is_interactive; then
        die_non_interactive "prompt '$label' requires input"
    fi
    local reply
    read -r -p "  $label [$default]: " reply </dev/tty >&2
    if [ -z "$reply" ]; then
        echo "$default"
    else
        echo "$reply"
    fi
}

# -------------------------
# CLI arg parsing shared between both deploy scripts.
#
# Arguments with no space (e.g. --end=front) are not supported — we expect
# the forms documented in print_common_usage below.
# -------------------------
print_common_usage() {
    cat <<'EOF'
Common options:
  --staging-a, -sa          Use Staging A
  --staging-b, -sb          Use Staging B
  --staging-c, -sc          Use Staging C
  --staging custom          Prompt for a custom (BE_ID, FE_ID, BE_URL, FE_URL) quad
  --end <front|back>        Which end(s) to deploy (can be repeated)
  -fe                       Shortcut for --end front
  -be                       Shortcut for --end back
  --dry-run                 Print icp canister install commands instead of running them
  --no-checks               Skip reachability and consistency checks
  --reconfigure             (deploy-pr-to-beta only) Args-only upgrade: no PR #,
                            script finds the CI artifact whose Wasm hash matches
                            what's currently on chain and re-uses it, so only the
                            install args change.
  --update-email-recovery-init
                            Fetch fresh IANA root anchors + set the curated
                            DoH allowlist on this upgrade. Without this flag
                            both fields stay opt null (i.e. preserve previous
                            on-chain values), which is what most upgrades
                            want once the canister has been initialized once.
  --doh-domains <a,b,...>   Override the default DoH allowlist (only used
                            with --update-email-recovery-init). Empty
                            disables the DoH path (DNSSEC-only).
  --openid-configs-file <path>
                            Set the backend openid_configs init arg from a
                            Candid-text file holding an opt vec OpenIdConfig
                            value (e.g. opt vec { record { ... } }, or
                            opt vec {} to clear all providers). Omit to leave
                            the current on-chain value untouched.
  -h, --help                Show this help
EOF
}

# Fills the globals listed in the file header. Any arguments not consumed
# (e.g. a PR number) are left in REMAINING_ARGS.
parse_common_args() {
    STAGING_NAME=""
    BE_ID=""
    FE_ID=""
    BE_URL=""
    FE_URL=""
    DEPLOY_FE=false
    DEPLOY_BE=false
    REBUILD_FE=false
    REBUILD_BE=false
    DRY_RUN=false
    NO_CHECKS=false
    IS_RECONFIGURE=false
    UPDATE_EMAIL_RECOVERY_INIT=false
    DOH_DOMAINS_ARG=""
    # Distinguishes "--doh-domains was not passed" (use the curated
    # default) from "--doh-domains '' was passed" (operator wants the
    # empty list to disable the DoH path entirely).
    DOH_DOMAINS_ARG_SET=false
    # Path to a Candid-text file holding the backend `openid_configs` value
    # to set on this upgrade. Empty = not passed = leave the field at `null`
    # (preserve the previously stored value).
    OPENID_CONFIGS_FILE=""
    REMAINING_ARGS=()

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                # Caller prints its own usage; re-raise.
                return 2
                ;;
            -sa|--staging-a)
                STAGING_NAME="a"
                shift
                ;;
            -sb|--staging-b)
                STAGING_NAME="b"
                shift
                ;;
            -sc|--staging-c)
                STAGING_NAME="c"
                shift
                ;;
            --staging)
                shift
                if [ $# -eq 0 ]; then
                    echo "Error: --staging requires an argument (a|b|c|custom)" >&2
                    return 1
                fi
                case "$1" in
                    a|b|c)   STAGING_NAME="$1" ;;
                    custom)  STAGING_NAME="custom" ;;
                    *)
                        echo "Error: --staging value must be a|b|c|custom, got '$1'" >&2
                        return 1
                        ;;
                esac
                shift
                ;;
            --end)
                shift
                if [ $# -eq 0 ]; then
                    echo "Error: --end requires an argument (front or back)" >&2
                    return 1
                fi
                case "$1" in
                    front) DEPLOY_FE=true ;;
                    back)  DEPLOY_BE=true ;;
                    *)
                        echo "Error: --end value must be 'front' or 'back', got '$1'" >&2
                        return 1
                        ;;
                esac
                shift
                ;;
            -fe)
                DEPLOY_FE=true
                shift
                ;;
            -be)
                DEPLOY_BE=true
                shift
                ;;
            -rfe)
                REBUILD_FE=true
                shift
                ;;
            -rbe)
                REBUILD_BE=true
                shift
                ;;
            --rebuild)
                shift
                # Consume one or more subsequent values in {fe, be, front, back}
                # until anything else.
                if [ $# -eq 0 ]; then
                    echo "Error: --rebuild requires at least one of {fe, be}" >&2
                    return 1
                fi
                while [[ $# -gt 0 ]]; do
                    case "$1" in
                        fe|front) REBUILD_FE=true; shift ;;
                        be|back)  REBUILD_BE=true; shift ;;
                        *) break ;;
                    esac
                done
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --no-checks)
                NO_CHECKS=true
                shift
                ;;
            --reconfigure)
                # Args-only mode (see deploy-pr-to-beta): no PR # required;
                # the script finds a CI artifact whose Wasm hash matches
                # whatever's currently on chain so the wasm bytes don't
                # change across the upgrade.
                IS_RECONFIGURE=true
                shift
                ;;
            --update-email-recovery-init)
                # Opt in to fetching IANA anchors + setting the DoH
                # allowlist on this upgrade. Without this flag both
                # fields stay `opt null` (preserve previous on-chain
                # value), which is what most upgrades want once the
                # canister has been initialized once.
                UPDATE_EMAIL_RECOVERY_INIT=true
                shift
                ;;
            --doh-domains)
                shift
                if [ $# -eq 0 ]; then
                    echo "Error: --doh-domains requires a comma-separated list (use '' to disable DoH)" >&2
                    return 1
                fi
                DOH_DOMAINS_ARG="$1"
                DOH_DOMAINS_ARG_SET=true
                shift
                ;;
            --openid-configs-file)
                shift
                if [ $# -eq 0 ]; then
                    echo "Error: --openid-configs-file requires a path to a Candid-text file" >&2
                    return 1
                fi
                if [ ! -f "$1" ]; then
                    echo "Error: --openid-configs-file: no such file: $1" >&2
                    return 1
                fi
                if [ ! -r "$1" ]; then
                    echo "Error: --openid-configs-file: file is not readable: $1" >&2
                    return 1
                fi
                # Reject empty / whitespace-only files now so the operator gets
                # a clear message here rather than an opaque didc parse error at
                # encode time.
                if ! grep -q '[^[:space:]]' "$1"; then
                    echo "Error: --openid-configs-file: file is empty or contains only whitespace: $1" >&2
                    return 1
                fi
                OPENID_CONFIGS_FILE="$1"
                shift
                ;;
            --)
                shift
                REMAINING_ARGS+=("$@")
                break
                ;;
            -*)
                echo "Unknown option: $1" >&2
                return 1
                ;;
            *)
                REMAINING_ARGS+=("$1")
                shift
                ;;
        esac
    done

    if [ -z "$STAGING_NAME" ]; then
        echo "Error: staging must be specified (use -sa/-sb/-sc or --staging custom)" >&2
        return 1
    fi
    if [ "$DEPLOY_FE" = false ] && [ "$DEPLOY_BE" = false ]; then
        echo "Error: --end must be specified (use -fe/-be or --end front/back)" >&2
        return 1
    fi
    return 0
}

# Populate BE_ID/FE_ID/BE_URL/FE_URL from STAGING_NAME. For known stagings we
# fill the canonical ids/URLs directly; for `custom` we prompt.
resolve_staging_config() {
    case "$STAGING_NAME" in
        a|b|c)
            BE_ID="$(staging_be_id "$STAGING_NAME")"
            FE_ID="$(staging_fe_id "$STAGING_NAME")"
            BE_URL="$(canister_default_url "$BE_ID")"
            FE_URL="$(canister_default_url "$FE_ID")"
            ;;
        custom)
            echo "Custom staging: please provide the four canister coordinates." >&2
            BE_ID="$(prompt_default "Backend canister id" "")"
            FE_ID="$(prompt_default "Frontend canister id" "")"
            BE_URL="$(prompt_default "Backend canister URL" "$(canister_default_url "$BE_ID")")"
            FE_URL="$(prompt_default "Frontend canister URL" "$(canister_default_url "$FE_ID")")"
            [ -n "$BE_ID" ] || { echo "Error: Backend canister id cannot be empty" >&2; exit 1; }
            [ -n "$FE_ID" ] || { echo "Error: Frontend canister id cannot be empty" >&2; exit 1; }
            [ -n "$BE_URL" ] || { echo "Error: Backend canister URL cannot be empty" >&2; exit 1; }
            [ -n "$FE_URL" ] || { echo "Error: Frontend canister URL cannot be empty" >&2; exit 1; }
            ;;
        *)
            echo "Error: unknown STAGING_NAME='$STAGING_NAME'" >&2
            exit 1
            ;;
    esac
}

# -------------------------
# Reachability checks
# -------------------------
# Verify the frontend URL serves II-frontend HTML. The HTML injects
# `data-canister-id` with the BACKEND canister id (configured on the FE via
# its `backend_canister_id` init arg) — we just check the attribute is
# present here; consistency between its value and our BE_ID is checked
# separately in `run_consistency_checks`.
check_fe_reachable() {
    local url="$1"
    echo "  Fetching $url ..." >&2
    local body
    if ! body=$(curl -fsSL --max-time 15 "$url" 2>/dev/null); then
        echo "  Error: could not fetch $url" >&2
        return 1
    fi
    if ! echo "$body" | grep -q 'data-canister-id="'; then
        echo "  Error: $url did not expose a data-canister-id attribute" >&2
        echo "         (likely not an II frontend canister at this URL)" >&2
        return 1
    fi
    local injected
    injected=$(echo "$body" | grep -oE 'data-canister-id="[^"]+"' \
        | head -n1 | sed -E 's/data-canister-id="([^"]+)"/\1/')
    echo "  OK — $url is an II frontend (advertises backend $injected)" >&2
    return 0
}

# Verify the backend URL serves `/.config.did.bin` with a non-empty body.
# Best-effort Candid-decode check uses the pinned didc when it can be
# fetched; falls back to a body-non-empty assertion if the download
# fails (e.g. offline).
#
# Uses explicit cleanup rather than `trap ... RETURN` — the RETURN trap isn't
# auto-restored, so a trap set inside a function keeps firing on every
# subsequent function return elsewhere in the script.
check_be_reachable() {
    local url="$1"
    local expected_id="$2"
    local config_url="${url}/.config.did.bin"
    echo "  Fetching $config_url ..." >&2
    local tmp
    tmp=$(mktemp)
    local status rc=0
    status=$(curl -sSL --max-time 15 -o "$tmp" -w '%{http_code}' "$config_url" 2>/dev/null || echo "000")
    if [ "$status" != "200" ]; then
        echo "  Error: $config_url returned HTTP $status" >&2
        rc=1
    elif [ ! -s "$tmp" ]; then
        echo "  Error: $config_url returned an empty body" >&2
        rc=1
    elif ensure_pinned_didc 2>/dev/null; then
        if ! "$PINNED_DIDC" decode "$(xxd -p "$tmp" | tr -d '\n')" >/dev/null 2>&1; then
            echo "  Error: $config_url did not decode as Candid" >&2
            rc=1
        else
            echo "  OK — $config_url returns valid Candid (canister $expected_id)" >&2
        fi
    else
        echo "  OK — $config_url returns HTTP 200 ($(wc -c <"$tmp" | tr -d ' ') bytes)." >&2
        echo "       (pinned didc download failed; skipping Candid validation)" >&2
    fi
    rm -f "$tmp"
    return $rc
}

run_reachability_checks() {
    if [ "$NO_CHECKS" = true ]; then
        echo "Skipping reachability checks (--no-checks)." >&2
        return 0
    fi
    echo "Running reachability checks..." >&2
    if [ "$DEPLOY_FE" = true ]; then
        check_fe_reachable "$FE_URL" || {
            echo "  Hint: pass --no-checks to bypass, or double-check FE_URL." >&2
            return 1
        }
    fi
    if [ "$DEPLOY_BE" = true ]; then
        check_be_reachable "$BE_URL" "$BE_ID" || {
            echo "  Hint: pass --no-checks to bypass, or double-check BE_URL/BE_ID." >&2
            return 1
        }
    fi
    return 0
}

# -------------------------
# Consistency check: FE's injected data-canister-id should equal our BE_ID
# (the FE canister points at the intended backend). This catches the
# "Staging C FE pointing at Staging B backend" misconfiguration we hit
# in the wild.
# -------------------------
run_consistency_checks() {
    if [ "$NO_CHECKS" = true ]; then
        echo "Skipping consistency checks (--no-checks)." >&2
        return 0
    fi
    # Only useful when deploying either end against a known staging.
    if [ "$STAGING_NAME" = "custom" ]; then
        echo "Consistency check skipped (custom staging has no expected baseline)." >&2
        return 0
    fi
    echo "Running consistency checks..." >&2
    local html
    if ! html=$(curl -fsSL --max-time 15 "$FE_URL" 2>/dev/null); then
        echo "  Warning: could not fetch $FE_URL to verify FE → BE wiring." >&2
        return 0
    fi
    local injected_be_id
    injected_be_id=$(echo "$html" | grep -oE 'data-canister-id="[^"]+"' \
        | head -n1 \
        | sed -E 's/data-canister-id="([^"]+)"/\1/') || true
    if [ -z "$injected_be_id" ]; then
        echo "  Warning: $FE_URL did not expose data-canister-id; skipping consistency check." >&2
        return 0
    fi
    if [ "$injected_be_id" = "$BE_ID" ]; then
        echo "  OK — FE $FE_ID is wired to BE $BE_ID." >&2
        return 0
    fi
    echo "" >&2
    echo "  INCONSISTENCY:" >&2
    echo "    $FE_URL currently points at backend $injected_be_id" >&2
    echo "    but this deploy would use $BE_ID for Staging $STAGING_NAME." >&2
    echo "    Deploying the FE (with its updated backend_canister_id) will realign them," >&2
    echo "    but anything calling the live FE until then will keep hitting the old BE." >&2
    echo "" >&2
    if ! confirm_yn "Continue anyway?"; then
        echo "Aborted by user." >&2
        exit 1
    fi
    return 0
}

# Extract a Candid record field's text value (including multi-line nested
# values) from the FE canister's /.config output. Returns empty if absent.
#
# Copied from scripts/frontend-arg-helpers.bash (where it's battle-tested) so
# we don't have to source the whole helper.
_parse_candid_field() {
    local field="$1" config="$2"
    awk -v field="$field" '
        BEGIN { found=0; depth=0; val="" }
        !found && $0 ~ field" = " {
            found=1
            sub(".*"field" = ", "")
        }
        found {
            val = (val == "" ? $0 : val "\n" $0)
            depth += gsub(/[{(]/, "&")
            depth -= gsub(/[})]/, "&")
            if (depth <= 0) {
                sub(/;[[:space:]]*$/, "", val)
                print val
                exit
            }
        }
    ' <<< "$config"
}

# -------------------------
# Per-FE interactive prompts for the install-arg fields that aren't tied to
# the staging quad: `backend_origin`, `related_origins`, `dev_csp`,
# `dummy_auth`, `analytics_config`. `backend_canister_id` is derived from
# BE_ID and `fetch_root_key` from the network, so neither is prompted.
#
# Fetches the FE canister's current `/.config` so each prompt offers the
# currently-stored value as its default. The FE init type is a required
# record (not opt), so every prompted field is set on each upgrade — there
# is no "absent = preserve" path — and using the current values as defaults
# is the only way hitting Enter through preserves behavior.
# -------------------------
prompt_fe_extra_args() {
    # Mainnet is the only target we currently support → fetch_root_key false.
    if [ "$IC_NETWORK" = "ic" ]; then
        FETCH_ROOT_KEY_ARG="opt false"
    else
        FETCH_ROOT_KEY_ARG="opt true"
    fi

    # Fetch the current FE config so prompt defaults reflect actual state.
    local config_url="${FE_URL}/.config"
    local raw_config=""
    echo "" >&2
    echo "Fetching current FE config from $config_url ..." >&2
    raw_config=$(curl --connect-timeout 10 --max-time 30 -sfL "$config_url" 2>/dev/null || true)

    # Fallback defaults derived from the staging quad. These match the
    # canister-default URLs and so will overwrite any custom-domain config
    # currently on chain — hence the prominent warning when the live
    # `/.config` couldn't be fetched.
    local backend_origin_default="\"$BE_URL\""
    local related_origins_default="opt vec { \"$FE_URL\" }"
    local dev_csp_default="null"
    local dummy_auth_default="null"
    local analytics_default="null"

    if [ -n "$raw_config" ]; then
        local parsed
        parsed=$(_parse_candid_field "backend_origin" "$raw_config");   [ -n "$parsed" ] && backend_origin_default="$parsed"
        parsed=$(_parse_candid_field "related_origins" "$raw_config");  [ -n "$parsed" ] && related_origins_default="$parsed"
        parsed=$(_parse_candid_field "dev_csp" "$raw_config");          [ -n "$parsed" ] && dev_csp_default="$parsed"
        parsed=$(_parse_candid_field "dummy_auth" "$raw_config");       [ -n "$parsed" ] && dummy_auth_default="$parsed"
        parsed=$(_parse_candid_field "analytics_config" "$raw_config"); [ -n "$parsed" ] && analytics_default="$parsed"
    else
        echo "  Warning: could not fetch current config; defaults are derived from the staging quad, NOT live values." >&2
        echo "           Hitting Enter through will OVERWRITE any custom-domain backend_origin / related_origins on chain." >&2
    fi

    echo "" >&2
    echo "Frontend install-arg prompts (hit Enter to keep current value):" >&2

    BACKEND_ORIGIN_ARG=$(prompt_default  "backend_origin (text)"                       "$backend_origin_default")
    RELATED_ORIGINS_ARG=$(prompt_default "related_origins (opt vec text)"              "$related_origins_default")
    DEV_CSP_ARG=$(prompt_default         "dev_csp (opt bool)"                          "$dev_csp_default")
    DUMMY_AUTH_ARG=$(prompt_default      "dummy_auth (opt opt DummyAuthConfig)"        "$dummy_auth_default")
    ANALYTICS_ARG=$(prompt_default       "analytics_config (opt opt AnalyticsConfig)"  "$analytics_default")
}

# -------------------------
# Per-BE interactive prompts for `backend_origin` and `related_origins`.
#
# The BE init type wraps every field in `opt`, and the canister's
# `post_upgrade` treats `null` as "leave the previously stored value
# untouched" — so emitting `null` for either field on upgrade is the
# safe preserve default, not destructive. Hitting Enter through these
# prompts therefore guarantees that any custom-domain config currently
# on chain is left in place; the operator only departs from `null` if
# they explicitly want to set new values.
#
# We deliberately don't try to read the live on-chain values back to
# fill the defaults: the BE's `/.config.did.bin` endpoint only mirrors
# the `synchronized` subset of init args (currently `openid_configs`
# and a couple of others), so `backend_origin` / `related_origins`
# decode as `null` there regardless of what was actually set at last
# upgrade. Using `null` defaults is honest about that — anything else
# would imply we know the on-chain values when we don't.
# -------------------------
prompt_be_extra_args() {
    echo "" >&2
    echo "Backend install-arg prompts (Enter = null = preserve previous on-chain value):" >&2

    # Defaults are `null` (preserve). To set a value, type the full
    # Candid expression at the prompt — e.g. `opt "https://backend.beta.id.ai"`
    # or `opt vec { "https://beta.id.ai"; "https://beta.identity.icp0.io" }`.
    BE_BACKEND_ORIGIN_ARG=$(prompt_default  "backend_origin (opt text)"       "null")
    BE_RELATED_ORIGINS_ARG=$(prompt_default "related_origins (opt vec text)"  "null")
}

# -------------------------
# Candid install-arg builders
# -------------------------

# Build the FE install arg. The FE init type is a REQUIRED record (not opt),
# so every required field is present; the optional ones we don't know are
# left as `null`.
#
# Output: Candid text on stdout.
build_fe_install_arg() {
    cat <<EOF
(
  record {
    backend_canister_id = principal "$BE_ID";
    backend_origin = $BACKEND_ORIGIN_ARG;
    related_origins = $RELATED_ORIGINS_ARG;
    fetch_root_key = $FETCH_ROOT_KEY_ARG;
    dev_csp = $DEV_CSP_ARG;
    dummy_auth = $DUMMY_AUTH_ARG;
    analytics_config = $ANALYTICS_ARG;
  }
)
EOF
}

# Build the BE install arg. The BE init type is `opt InternetIdentityInit`
# where every field is itself `opt` and "absent" = preserve previous value,
# so we only set the fields that follow from the shared quad (BE_ID, BE_URL,
# FE_URL), the prompted backend_origin/related_origins, the
# `--openid-configs-file` contents (when passed), and — when
# `UPDATE_EMAIL_RECOVERY_INIT` is true — the `dnssec_config`/`doh_config`
# knobs the email-recovery flow needs. Everything left unset stays untouched
# on upgrade.
#
# Output: Candid text on stdout.
build_be_install_arg() {
    local extra=""
    if [ "$UPDATE_EMAIL_RECOVERY_INIT" = true ]; then
        # IANA root anchors — fetched + reviewed live. Each call hits
        # data.iana.org, so we cache the result for the lifetime of the
        # script run by stashing it on the first call.
        if [ -z "${_BE_DNSSEC_CONFIG_CANDID:-}" ]; then
            local anchors_vec
            anchors_vec="$(fetch_and_review_iana_root_anchors)" || return 1
            _BE_DNSSEC_CONFIG_CANDID="opt opt record { root_anchors = $anchors_vec }"
        fi
        # DoH allowlist — operator config (the user can override via
        # --doh-domains; empty disables the DoH path entirely).
        local doh_record
        doh_record="$(_be_doh_config_candid)"
        extra=$(cat <<EXTRA
    dnssec_config = $_BE_DNSSEC_CONFIG_CANDID;
    doh_config = $doh_record;
EXTRA
)
    fi

    # `openid_configs` stays `null` (= preserve the previous on-chain value)
    # unless --openid-configs-file was passed; its Candid contents are then
    # emitted verbatim. parse_common_args already validated the path, but the
    # read is re-checked here (separately from the `local` declaration, whose
    # exit status would otherwise mask cat's) so a file that vanished or was
    # emptied after parsing fails loudly instead of emitting `... = ;`.
    local openid_configs_arg="null"
    if [ -n "$OPENID_CONFIGS_FILE" ]; then
        if ! openid_configs_arg="$(cat "$OPENID_CONFIGS_FILE")" \
                || [ -z "${openid_configs_arg//[[:space:]]/}" ]; then
            echo "Error: failed to read --openid-configs-file (or it is now empty): $OPENID_CONFIGS_FILE" >&2
            return 1
        fi
    fi

    cat <<EOF
(
  opt record {
    backend_canister_id = opt principal "$BE_ID";
    backend_origin = $BE_BACKEND_ORIGIN_ARG;
    related_origins = $BE_RELATED_ORIGINS_ARG;
    openid_configs = $openid_configs_arg;
$extra
  }
)
EOF
}

# Internal: render the DoH allowlist as Candid `opt opt DohConfig`.
# - `--doh-domains` not passed → use DEFAULT_DOH_ALLOWED_DOMAINS.
# - `--doh-domains foo,bar` → that list.
# - `--doh-domains ''` → empty list, which the canister treats as
#   "no domain may use the DoH path" (DNSSEC-only).
_be_doh_config_candid() {
    local -a domains
    if [ "$DOH_DOMAINS_ARG_SET" = true ]; then
        # Comma-separated list → array. Trim whitespace per element.
        # An empty string yields a single empty element, which we drop
        # in the rendering loop below — so `--doh-domains ''` emits
        # `allowed_domains = vec { };`.
        IFS=',' read -ra domains <<< "$DOH_DOMAINS_ARG"
        local i
        for i in "${!domains[@]}"; do
            domains[i]="${domains[i]## }"
            domains[i]="${domains[i]%% }"
        done
    else
        domains=("${DEFAULT_DOH_ALLOWED_DOMAINS[@]}")
    fi

    local list=""
    local d
    for d in "${domains[@]}"; do
        if [ -z "$d" ]; then continue; fi
        list+="\"$d\"; "
    done
    cat <<EOF
opt opt record {
      allowed_domains = vec { $list};
      max_cache_age_secs = opt (3600 : nat64);
    }
EOF
}

# -------------------------
# `--reconfigure` helpers
# -------------------------
# Args-only deploy support: find the CI artifact whose gzipped wasm
# sha256 matches whatever's currently on chain, so we can upgrade the
# install args without changing the on-chain wasm bytes. Verified
# empirically against Staging A: `module_hash` (from canister_status)
# equals `sha256(internet_identity_backend.wasm.gz)` — i.e. the IC
# stores the *gzipped* bytes and hashes them, rather than the
# decompressed module. So no `gunzip` step here.

# Read the on-chain module_hash via the public ic-api endpoint. No
# authentication, no canister call — just the dashboard's read-only
# REST surface. Stdout: lowercase hex sha256, or empty on failure.
fetch_onchain_module_hash() {
    local canister_id="$1"
    curl --connect-timeout 10 --max-time 30 -fsSL \
        "https://ic-api.internetcomputer.org/api/v3/canisters/$canister_id" 2>/dev/null \
        | jq -r '.module_hash // empty'
}

# Search recent CI artifacts for one whose .wasm.gz sha256 matches
# `$2`. Writes the matching wasm.gz to `$3` and prints the run id (for
# the per-end "=== Deploying ... ===" log line) to stdout. Returns
# non-zero with a diagnostic if no match is found in the search window.
#
# Args:
#   $1  artifact name (e.g. "internet_identity_backend.wasm.gz")
#   $2  expected sha256 (lowercase hex)
#   $3  destination path for the matching wasm.gz
#
# Requires: $REPO, $AUTH_HEADER set by the caller (deploy-pr-to-beta
# initialises both right after PR/auth resolution).
#
# Uses the per-repo artifacts endpoint with the `?name=...` filter,
# which returns up to 100 matching artifacts per page directly — much
# denser than walking workflow runs (most runs don't host the artifact
# we want, so per-run scans waste API budget). Searches at most
# `max_artifacts` artifacts before giving up; bump as needed if the
# on-chain wasm reliably exceeds the window.
find_artifact_by_hash() {
    local artifact_name="$1"
    local expected_hash="$2"
    local dest_path="$3"
    local max_artifacts=300

    echo "  Searching $REPO for $artifact_name with hash $expected_hash (up to $max_artifacts artifacts)..." >&2

    local tmp_zip tmp_dir
    tmp_zip=$(mktemp -t ii-reconfigure.XXXX.zip)
    tmp_dir=$(mktemp -d -t ii-reconfigure.XXXX)
    # shellcheck disable=SC2064 # we want $tmp_* expanded at trap setup
    trap "rm -rf '$tmp_zip' '$tmp_dir'" RETURN

    local page=1 checked=0 expired_skipped=0
    while [ "$checked" -lt "$max_artifacts" ]; do
        local artifacts_json
        artifacts_json=$(curl -sf -H "$AUTH_HEADER" \
            "https://api.github.com/repos/$REPO/actions/artifacts?name=$artifact_name&per_page=100&page=$page" \
            2>/dev/null || echo '{"artifacts":[]}')

        local count
        count=$(echo "$artifacts_json" | jq '.artifacts | length')
        if [ "$count" -eq 0 ]; then
            break
        fi

        local i
        for ((i=0; i<count && checked<max_artifacts; i++)); do
            local expired url run_id branch
            expired=$(echo "$artifacts_json" | jq -r ".artifacts[$i].expired")
            if [ "$expired" = "true" ]; then
                expired_skipped=$((expired_skipped + 1))
                continue
            fi
            url=$(echo "$artifacts_json" | jq -r ".artifacts[$i].archive_download_url")
            run_id=$(echo "$artifacts_json" | jq -r ".artifacts[$i].workflow_run.id")
            branch=$(echo "$artifacts_json" | jq -r ".artifacts[$i].workflow_run.head_branch")

            checked=$((checked + 1))
            if ! curl -sfL -H "$AUTH_HEADER" -o "$tmp_zip" "$url" 2>/dev/null; then
                continue
            fi
            rm -rf "$tmp_dir" && mkdir -p "$tmp_dir"
            if ! unzip -o -q "$tmp_zip" -d "$tmp_dir" 2>/dev/null; then
                continue
            fi
            if [ ! -f "$tmp_dir/$artifact_name" ]; then
                continue
            fi

            local hash
            hash=$(shasum -a 256 "$tmp_dir/$artifact_name" | awk '{print $1}')
            if [ "$hash" = "$expected_hash" ]; then
                mv "$tmp_dir/$artifact_name" "$dest_path"
                echo "  Match: run $run_id ($branch)" >&2
                echo "$run_id"
                return 0
            fi
        done

        page=$((page + 1))
        if [ "$count" -lt 100 ]; then
            break  # last page
        fi
    done

    echo "  Error: no artifact matching sha256 $expected_hash found in the most recent $checked artifacts" \
         "(of $artifact_name; $expired_skipped expired skipped)." >&2
    echo "         The on-chain wasm probably pre-dates GitHub's 90-day artifact retention window, or the" >&2
    echo "         deploy was performed by a path that didn't go through canister-tests.yml. Fall back to" >&2
    echo "         the regular PR-anchored flow: \`./scripts/deploy-pr-to-beta -s<env> -fe -be <PR>\`." >&2
    return 1
}

# -------------------------
# Render the frontend canister's `init_args.path` file from its template
# if it's missing. `icp canister install` loads the whole `icp.yaml`
# project before it processes our inline `--args`, and the project loader
# fails fast when any `init_args.path` reference is missing on disk.
#
# Normally `npm install`'s `postinstall` hook (see PR #3827) renders this
# file, but neither deploy script depends on a fresh `npm install` — they
# pull CI artifacts (`deploy-pr-to-beta`) or run `scripts/build` directly
# (`deploy-local-to-beta`), so a clone where `npm install` was never run
# or `.icp/` was cleaned will trip the loader.
#
# Idempotent: returns immediately if the file is already there. The
# render script itself falls back to anonymous-principal placeholders
# when no local network is up — which is exactly the deploy-to-mainnet
# case, where `--args` overrides at install time mean the placeholder
# content is never parsed.
bootstrap_init_args() {
    local scripts_dir
    scripts_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    local rendered="$scripts_dir/../.icp/cache/init-args/internet_identity_frontend.did"
    if [ -f "$rendered" ]; then
        return 0
    fi
    echo "Rendering frontend init_args stub (icp.yaml project load needs the file)..." >&2
    if ! "$scripts_dir/render-local-init-args" >&2; then
        echo "Error: failed to render $rendered. Cannot proceed with icp install." >&2
        return 1
    fi
}

# -------------------------
# Encode a Candid text install arg into a raw-binary file using the
# pinned didc, validated against the schema embedded *in the wasm being
# deployed* — not the working tree's `.did`. The local `.did` and the
# CI artifact's wasm can be from different commits (especially after a
# rebase or when running `--reconfigure` against an old on-chain wasm),
# so encoding against the local schema lets the type checker silently
# pass on a payload the deployed code can't parse. ic-wasm gives us the
# exact `candid:service` + `candid:args` sections the deployed module
# advertises, eliminating that drift.
#
# Args:
#   $1  Path to the wasm being installed (.wasm or .wasm.gz)
#   $2  Candid text payload (output of build_{fe,be}_install_arg)
#   $3  Output path for the raw-binary args file
#
# Output: writes raw Candid bytes to $3. Returns non-zero (and prints a
# targeted error) on download / extract / encode failure so the caller
# can bail before invoking `icp canister install` with a half-baked
# file.
encode_install_arg_bin() {
    local wasm_path="$1"
    local candid_text="$2"
    local out_file="$3"

    if [ ! -f "$wasm_path" ]; then
        echo "Error: wasm not found at $wasm_path" >&2
        return 1
    fi
    if ! command -v ic-wasm >/dev/null 2>&1; then
        echo "Error: \`ic-wasm\` not on PATH. Run \`./scripts/bootstrap\` (or" >&2
        echo "       \`cargo install ic-wasm --version 0.8.5\`) to install it." >&2
        return 1
    fi
    ensure_pinned_didc || return 1

    # ic-wasm needs a raw .wasm input. Decompress to a temp if we were
    # handed a .wasm.gz (CI artifact). Use a single temp dir for both
    # the decompressed wasm and the extracted .did so cleanup is easy.
    local tmp_dir
    tmp_dir=$(mktemp -d -t ii-encode-args.XXXX)
    local wasm_for_metadata
    if [[ "$wasm_path" == *.gz ]]; then
        wasm_for_metadata="$tmp_dir/wasm-input.wasm"
        if ! gunzip -c "$wasm_path" > "$wasm_for_metadata"; then
            echo "Error: failed to gunzip $wasm_path" >&2
            rm -rf "$tmp_dir"
            return 1
        fi
    else
        wasm_for_metadata="$wasm_path"
    fi

    # Extract the full Candid service definition (types + methods) and
    # the init args type from the wasm's metadata sections — both are
    # required to type-check the encode. `candid:args` is just the
    # parenthesised init type (e.g. `(opt InternetIdentityInit)`), so
    # the caller doesn't have to hard-code the name and can't drift.
    local did_file="$tmp_dir/wasm-input.did"
    if ! ic-wasm "$wasm_for_metadata" metadata candid:service > "$did_file" 2>/dev/null \
            || [ ! -s "$did_file" ]; then
        echo "Error: failed to extract candid:service metadata from $wasm_path" >&2
        echo "       (Is this an internet-identity backend / frontend wasm?)" >&2
        rm -rf "$tmp_dir"
        return 1
    fi
    local candid_type
    candid_type=$(ic-wasm "$wasm_for_metadata" metadata candid:args 2>/dev/null)
    if [ -z "$candid_type" ]; then
        echo "Error: failed to extract candid:args metadata from $wasm_path" >&2
        rm -rf "$tmp_dir"
        return 1
    fi

    local hex
    if ! hex=$("$PINNED_DIDC" encode -d "$did_file" -t "$candid_type" "$candid_text"); then
        echo "Error: didc encode failed for $wasm_path / $candid_type" >&2
        echo "       Candid input was:" >&2
        echo "$candid_text" | sed 's/^/         /' >&2
        rm -rf "$tmp_dir"
        return 1
    fi
    # `didc encode` emits hex; the icp-cli `--args-format bin` path
    # expects raw bytes, so we collapse hex → bytes with `xxd -r -p`.
    printf '%s' "$hex" | xxd -r -p > "$out_file"

    rm -rf "$tmp_dir"

    if [ ! -s "$out_file" ]; then
        echo "Error: encoded args file is empty after xxd conversion: $out_file" >&2
        return 1
    fi
}

# -------------------------
# Proxy-routed install runner (honours DRY_RUN)
# -------------------------
# Routes the upgrade through the proxy canister at PROXY_CANISTER_ID,
# which is the legacy staging wallet reinstalled with the icp-cli proxy
# WASM (see
# https://cli.internetcomputer.org/0.2/migration/from-dfx/#replacing-the-dfx-wallet-canister).
# The proxy keeps the wallet's canister ID — and therefore its
# controllership of the staging canisters — but now exposes the `proxy`
# method icp-cli's `--proxy` expects.
#
# Args: <canister_id> <wasm_path> <install_arg_bin_file>
#
# `install_arg_bin_file` is the raw-binary Candid arg produced by
# `encode_install_arg_bin` — same format `make-upgrade-proposal` ships
# in its NNS proposals, so the bytes on chain are bit-identical between
# both paths.
run_icp_install() {
    local canister_id="$1"
    local wasm_path="$2"
    local install_arg_bin="$3"

    if [ ! -f "$wasm_path" ]; then
        echo "Error: wasm not found at $wasm_path" >&2
        return 1
    fi
    if [ ! -f "$install_arg_bin" ]; then
        echo "Error: install-arg binary not found at $install_arg_bin" >&2
        return 1
    fi

    local cmd=(
        icp canister install "$canister_id"
            -e "$IC_NETWORK"
            --proxy "$PROXY_CANISTER_ID"
            --mode upgrade
            --wasm "$wasm_path"
            --args-file "$install_arg_bin"
            --args-format bin
    )

    if [ "$DRY_RUN" = true ]; then
        echo ""
        echo "[dry-run] Would run:"
        # Quote each arg so the printout is copy-pasteable.
        printf '  %q' "${cmd[@]}"; echo
        return 0
    fi

    # Bootstrap below the dry-run gate so `--dry-run` stays side-effect-free
    # (no file writes under `.icp/`, no implicit `icp canister create` calls
    # from `render-local-init-args`). On real runs we surface a render
    # failure here rather than letting the icp-cli project loader fail in a
    # less obvious place.
    bootstrap_init_args || return 1

    echo "Upgrading canister $canister_id ..."
    "${cmd[@]}"
    echo "Upgrade of $canister_id complete."
}

# -------------------------
# Banner / status summary
# -------------------------
print_deployment_summary() {
    echo ""
    echo "======== Deployment plan ========"
    echo "  Staging:    $STAGING_NAME"
    echo "  BE_ID:      $BE_ID"
    echo "  BE_URL:     $BE_URL"
    echo "  FE_ID:      $FE_ID"
    echo "  FE_URL:     $FE_URL"
    echo "  Deploy FE:  $DEPLOY_FE"
    echo "  Deploy BE:  $DEPLOY_BE"
    echo "  Dry run:    $DRY_RUN"
    echo "  No checks:  $NO_CHECKS"
    echo "================================="
}
