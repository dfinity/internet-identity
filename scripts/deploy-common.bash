#!/usr/bin/env bash
# Shared helpers for deploy-pr-to-beta and deploy-local-to-beta.
#
# Responsibilities:
# - CLI arg parsing for staging selection (-sa/-sb/-sc/--staging custom),
#   end selection (-fe/-be/--end), dry-run, no-checks, and (for the local
#   script) the rebuild flags.
# - Reachability + consistency checks against the selected staging canisters.
# - Interactive prompting for the small set of install-arg fields we actually
#   want humans to make decisions about (dev_csp / dummy_auth / analytics_config)
#   — everything else is derived from the four shared values (BE_ID, FE_ID,
#   BE_URL, FE_URL) or left opaque via opt null to preserve prior state.
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

readonly WALLET_CANISTER_ID="cvthj-wyaaa-aaaad-aaaaq-cai"
readonly IC_NETWORK="ic"

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
# If `didc` is available, additionally check the blob decodes as Candid.
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
    elif command -v didc >/dev/null 2>&1; then
        if ! didc decode "$(xxd -p "$tmp" | tr -d '\n')" >/dev/null 2>&1; then
            echo "  Error: $config_url did not decode as Candid" >&2
            rc=1
        else
            echo "  OK — $config_url returns valid Candid (canister $expected_id)" >&2
        fi
    else
        echo "  OK — $config_url returns HTTP 200 ($(wc -c <"$tmp" | tr -d ' ') bytes)." >&2
        echo "       (install didc to additionally verify the body decodes as Candid)" >&2
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
# Per-FE interactive prompts for the small set of fields that aren't derived
# from the shared quad (dev_csp, dummy_auth, analytics_config). fetch_root_key
# is auto-selected by network.
#
# Fetches the FE canister's current `/.config` so each prompt offers the
# currently-stored value as its default. Sending `null` on upgrade actively
# resets these fields on the canister — it is NOT a no-op — so using the
# current values as defaults preserves behavior when the user just hits
# Enter.
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

    local dev_csp_default="null"
    local dummy_auth_default="null"
    local analytics_default="null"

    if [ -n "$raw_config" ]; then
        local parsed
        parsed=$(_parse_candid_field "dev_csp" "$raw_config");          [ -n "$parsed" ] && dev_csp_default="$parsed"
        parsed=$(_parse_candid_field "dummy_auth" "$raw_config");       [ -n "$parsed" ] && dummy_auth_default="$parsed"
        parsed=$(_parse_candid_field "analytics_config" "$raw_config"); [ -n "$parsed" ] && analytics_default="$parsed"
    else
        echo "  Warning: could not fetch current config; defaulting all three fields to null." >&2
        echo "           (This will reset them on the canister if you hit Enter through.)" >&2
    fi

    echo "" >&2
    echo "Frontend-only install-arg prompts (hit Enter to keep current value):" >&2

    DEV_CSP_ARG=$(prompt_default "dev_csp (opt bool)" "$dev_csp_default")
    DUMMY_AUTH_ARG=$(prompt_default "dummy_auth (opt opt DummyAuthConfig)" "$dummy_auth_default")
    ANALYTICS_ARG=$(prompt_default "analytics_config (opt opt AnalyticsConfig)" "$analytics_default")
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
    backend_origin = "$BE_URL";
    related_origins = opt vec { "$FE_URL" };
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
# FE_URL). Everything else stays untouched on upgrade.
#
# Output: Candid text on stdout.
build_be_install_arg() {
    cat <<EOF
(
  opt record {
    backend_canister_id = opt principal "$BE_ID";
    backend_origin = opt "$BE_URL";
    related_origins = opt vec { "$FE_URL" };
  }
)
EOF
}

# -------------------------
# Wallet-proxied install runner (honours DRY_RUN)
# -------------------------
# Falls back to dfx for the actual upgrade because the staging canisters
# are controlled by a dfx wallet (`cvthj-wyaaa-aaaad-aaaaq-cai`) that
# routes via `wallet_call`. icp-cli's `--proxy` expects a custom proxy
# canister exposing a `proxy` method instead, so pointing it at the
# wallet trips `Canister has no update method 'proxy'`. See PR #3815
# where the same pattern was applied to `make-upgrade-proposal`.
#
# Args: <canister_id> <wasm_path> <install_arg_candid_text>
run_icp_install() {
    local canister_id="$1"
    local wasm_path="$2"
    local install_arg="$3"

    if [ ! -f "$wasm_path" ]; then
        echo "Error: wasm not found at $wasm_path" >&2
        return 1
    fi

    local cmd=(
        dfx canister
            --network "$IC_NETWORK"
            --wallet "$WALLET_CANISTER_ID"
            install "$canister_id"
            --mode upgrade
            --wasm "$wasm_path"
            --argument "$install_arg"
    )

    if [ "$DRY_RUN" = true ]; then
        echo ""
        echo "[dry-run] Would run:"
        # Quote each arg so the printout is copy-pasteable.
        printf '  %q' "${cmd[@]}"; echo
        return 0
    fi

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
