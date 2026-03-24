#!/usr/bin/env bash
# Shared helpers for building InternetIdentityFrontendInit arguments interactively.
# Sourced by make-upgrade-proposal and deploy-pr-to-beta.

# -------------------------
# Prompt for a single Candid field value.
# Usage: prompt_field <field_label> <current_value>
# Prints the (possibly updated) value to stdout.
# -------------------------
prompt_field() {
    local label="$1"
    local current="$2"
    echo "" >&2
    echo "  $label = $current" >&2
    read -r -p "  Keep current value? [Y/n]: " answer </dev/tty >&2
    if [[ "$answer" =~ ^[Nn] ]]; then
        read -r -p "  Enter new value for $label: " new_value </dev/tty >&2
        echo "$new_value"
    else
        echo "$current"
    fi
}

# -------------------------
# Build InternetIdentityFrontendInit Candid argument interactively.
# Fetches current config from the canister, then prompts field-by-field.
#
# Usage: build_frontend_install_arg <canister_id>
# Sets FRONTEND_INSTALL_ARG with the hex-encoded result.
# Sets FRONTEND_CANDID_ARG with the Candid text argument.
# -------------------------
build_frontend_install_arg() {
    local canister_id="$1"
    local primary_url="https://${canister_id}.icp0.io/.config"
    local fallback_url="https://${canister_id}.ic0.app/.config"

    echo ""
    local raw_config=""
    for config_url in "$primary_url" "$fallback_url"; do
        echo "Fetching current frontend config from $config_url ..."
        if raw_config=$(curl --connect-timeout 10 --max-time 30 -sfL "$config_url") && [ -n "$raw_config" ]; then
            break
        fi
        echo "  Failed, trying next domain..." >&2
        raw_config=""
    done
    if [ -z "$raw_config" ]; then
        echo "Error: Could not fetch config from either $primary_url or $fallback_url" >&2
        exit 1
    fi

    echo ""
    echo "Current frontend config:"
    echo "$raw_config"
    echo ""
    echo "Configure install arguments (press Enter to keep each current value):"

    # Parse individual fields from the Candid text output.
    # For multi-line nested values (analytics_config, related_origins), we extract
    # everything from "field = " to the matching closing brace/semicolon.
    parse_candid_field() {
        local field="$1" config="$2"
        # Try multi-line extraction: from "field = " to the line where braces balance
        awk -v field="$field" '
            BEGIN { found=0; depth=0; val="" }
            !found && $0 ~ field" = " {
                found=1
                sub(".*"field" = ", "")
                # Remove trailing semicolon if this is a single-line value
            }
            found {
                val = (val == "" ? $0 : val "\n" $0)
                depth += gsub(/[{(]/, "&")
                depth -= gsub(/[})]/, "&")
                if (depth <= 0) {
                    # Remove trailing semicolon at the top level
                    sub(/;[[:space:]]*$/, "", val)
                    print val
                    exit
                }
            }
        ' <<< "$config"
    }

    local current_backend_canister_id
    current_backend_canister_id=$(parse_candid_field "backend_canister_id" "$raw_config")
    local current_backend_origin
    current_backend_origin=$(parse_candid_field "backend_origin" "$raw_config")
    local current_related_origins
    current_related_origins=$(parse_candid_field "related_origins" "$raw_config")
    local current_fetch_root_key
    current_fetch_root_key=$(parse_candid_field "fetch_root_key" "$raw_config")
    local current_analytics_config
    current_analytics_config=$(parse_candid_field "analytics_config" "$raw_config")
    local current_dummy_auth
    current_dummy_auth=$(parse_candid_field "dummy_auth" "$raw_config")
    local current_dev_csp
    current_dev_csp=$(parse_candid_field "dev_csp" "$raw_config")

    local val_backend_canister_id
    val_backend_canister_id=$(prompt_field "backend_canister_id" "$current_backend_canister_id")
    local val_backend_origin
    val_backend_origin=$(prompt_field "backend_origin" "$current_backend_origin")
    local val_related_origins
    val_related_origins=$(prompt_field "related_origins" "$current_related_origins")
    local val_fetch_root_key
    val_fetch_root_key=$(prompt_field "fetch_root_key" "$current_fetch_root_key")
    local val_analytics_config
    val_analytics_config=$(prompt_field "analytics_config" "$current_analytics_config")
    local val_dummy_auth
    val_dummy_auth=$(prompt_field "dummy_auth" "$current_dummy_auth")
    local val_dev_csp
    val_dev_csp=$(prompt_field "dev_csp" "$current_dev_csp")

    FRONTEND_CANDID_ARG="(record { backend_canister_id = ${val_backend_canister_id}; backend_origin = ${val_backend_origin}; related_origins = ${val_related_origins}; fetch_root_key = ${val_fetch_root_key}; analytics_config = ${val_analytics_config}; dummy_auth = ${val_dummy_auth}; dev_csp = ${val_dev_csp} })"

    echo ""
    echo "Install argument (Candid):"
    echo "  $FRONTEND_CANDID_ARG"
    echo ""

    echo "Encoding argument with didc..."
    local encoded
    encoded=$(didc encode \
        -d ./src/internet_identity_frontend/internet_identity_frontend.did \
        -t '(InternetIdentityFrontendInit)' \
        "$FRONTEND_CANDID_ARG")
    if [ -z "$encoded" ]; then
        echo "Error: didc encode failed" >&2
        exit 1
    fi

    FRONTEND_INSTALL_ARG="$encoded"

    echo "Encoded argument: $FRONTEND_INSTALL_ARG"
    echo ""
    read -r -p "Proceed with this argument? [Y/n]: " confirm </dev/tty
    if [[ "$confirm" =~ ^[Nn] ]]; then
        echo "Aborted by user." >&2
        exit 1
    fi
}
