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
    local config_url="https://${canister_id}.icp0.io/.config"

    echo ""
    echo "Fetching current frontend config from $config_url ..."
    local raw_config
    raw_config=$(curl -sfL "$config_url")
    if [ -z "$raw_config" ]; then
        echo "Error: Could not fetch current config from $config_url" >&2
        exit 1
    fi

    echo ""
    echo "Current frontend config:"
    echo "$raw_config"
    echo ""
    echo "Configure install arguments (press Enter to keep each current value):"

    # Parse individual fields from the Candid text output.
    local current_backend_canister_id
    current_backend_canister_id=$(echo "$raw_config" | grep 'backend_canister_id' | sed 's/.*= *//;s/ *;$//')
    local current_backend_origin
    current_backend_origin=$(echo "$raw_config" | grep 'backend_origin' | sed 's/.*= *//;s/ *;$//')
    local current_related_origins
    current_related_origins=$(echo "$raw_config" | sed -n '/related_origins/,/}/p' | tr '\n' ' ' | sed 's/.*= *//;s/ *;[[:space:]]*$//')
    local current_fetch_root_key
    current_fetch_root_key=$(echo "$raw_config" | grep 'fetch_root_key' | sed 's/.*= *//;s/ *;$//')
    local current_analytics_config
    current_analytics_config=$(echo "$raw_config" | grep 'analytics_config' | sed 's/.*= *//;s/ *;$//')
    local current_dummy_auth
    current_dummy_auth=$(echo "$raw_config" | grep 'dummy_auth' | sed 's/.*= *//;s/ *;$//')
    local current_dev_csp
    current_dev_csp=$(echo "$raw_config" | grep 'dev_csp' | sed 's/.*= *//;s/ *;$//')

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
