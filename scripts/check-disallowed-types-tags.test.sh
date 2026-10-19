#!/usr/bin/env bash
# Regression cases for check-disallowed-types-tags.sh: every evasive spelling of
# an untagged allow must fail, every tagged one must pass.
set -euo pipefail
cd "$(dirname "$0")/.."
checker=./scripts/check-disallowed-types-tags.sh
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

expect_fail() {
    local name=$1 body=$2
    mkdir -p "$tmp/$name" && printf '%s\n' "$body" > "$tmp/$name/lib.rs"
    if "$checker" "$tmp/$name" >/dev/null 2>&1; then
        echo "FAIL: '$name' should have been rejected" >&2
        exit 1
    fi
}
expect_pass() {
    local name=$1 body=$2
    mkdir -p "$tmp/$name" && printf '%s\n' "$body" > "$tmp/$name/lib.rs"
    if ! "$checker" "$tmp/$name" >/dev/null 2>&1; then
        echo "FAIL: '$name' should have been accepted" >&2
        exit 1
    fi
}

expect_fail single-line         $'#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail multi-line-allow    $'#[allow(\n    clippy::disallowed_types,\n)]\nfn f() {}'
expect_fail multi-line-expect   $'#[expect(\n    dead_code,\n    clippy::disallowed_types,\n)]\nfn f() {}'
expect_fail multi-line-group    $'#[allow(\n    clippy::all\n)]\nfn f() {}'
expect_fail style-group         $'#[allow(clippy::style)]\nfn f() {}'
expect_fail cfg-attr            $'#[cfg_attr(test, allow(clippy::disallowed_types))]\nfn f() {}'
expect_fail crate-level         $'#![allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail cfg-attr-second-list $'#[cfg_attr(test, allow(dead_code), allow(clippy::disallowed_types))]\nfn f() {}'
expect_fail cfg-attr-nested-expect $'#[cfg_attr(not(test), allow(dead_code), expect(clippy::all))]\nfn f() {}'
expect_fail spaced-path         $'#[allow(clippy :: disallowed_types)]\nfn f() {}'
expect_fail spaced-path-wrapped $'#[allow(\n    dead_code,\n    clippy\n        ::disallowed_types,\n)]\nfn f() {}'
expect_fail two-attrs-one-line  $'#[allow(dead_code)] #[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail tag-not-adjacent    $'// disallowed-types: removed by B2\n\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail tag-inside-attr     $'#[allow(\n    // disallowed-types: removed by B2\n    clippy::disallowed_types,\n)]\nfn f() {}'
expect_pass tagged-single       $'// disallowed-types: removed by B2\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_pass tagged-multi-line   $'// disallowed-types: removed by B2\n#[allow(\n    dead_code,\n    clippy::disallowed_types,\n)]\nfn f() {}'
expect_pass tagged-permanent    $'    // disallowed-types: permanent, Candid entry point\n    #[allow(clippy::disallowed_types)]\n    fn f() {}'
expect_pass tagged-cfg-attr     $'// disallowed-types: removed by A2.1\n#[cfg_attr(test, allow(clippy::disallowed_types))]\nfn f() {}'
expect_pass tagged-second-list  $'// disallowed-types: removed by A2.1\n#[cfg_attr(test, allow(dead_code), allow(clippy::disallowed_types))]\nfn f() {}'
expect_pass unrelated-two-lists $'#[cfg_attr(test, allow(dead_code), allow(unused_imports))]\nfn f() {}'
expect_pass lint-name-prefix    $'#[allow(clippy::allow_attributes)]\nfn f() {}'
expect_pass unrelated-allow     $'#[allow(\n    dead_code,\n    unused_imports,\n)]\nfn f() {}'
expect_pass derive-then-allow   $'#[derive(Debug)]\n// disallowed-types: removed by C1\n#[allow(clippy::disallowed_types)]\nstruct S;'
echo "check-disallowed-types-tags.test.sh: ok"
