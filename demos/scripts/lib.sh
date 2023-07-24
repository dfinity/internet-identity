repo_root() {
    git rev-parse --show-toplevel
}

print_red() {
    echo -e "\033[0;31m$*\033[0m" 1>&2
}

print_green() {
    echo -e "\033[0;32m$*\033[0m"
}

print_blue() {
    echo -e "\033[0;34m$*\033[0m"
}

info() {
    print_green "***** $*"
}

log() {
    echo "   $*"
}

log_stderr() {
    echo "   $*" >&2
}

debug_log() {
    if [ ! -z ${DEBUG_BASH+x} ]; then
        log_stderr "$*"
    fi
}

error() {
    print_red "ERROR: $1"
    exit 1
}

# Works from inside/outside of the repo to return the root of this repository
repo_root() {
    local SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
    # Execute in subshell to not change directory of caller
    (cd "$SCRIPT_DIR" && git rev-parse --show-toplevel)
}
