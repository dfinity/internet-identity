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
expect_fail bracket-in-reason  $'#[allow(clippy::disallowed_types, reason = "legacy [B2]")]\nfn f() {}'
expect_fail paren-in-reason    $'#[allow(clippy::disallowed_types, reason = "see (B2")]\nfn f() {}'
expect_fail bracket-in-comment $'#[allow(\n    // see [B2]\n    clippy::disallowed_types,\n)]\nfn f() {}'
expect_fail bracket-in-block-comment $'#[allow(/* [ */ clippy::disallowed_types)]\nfn f() {}'
expect_fail after-byte-char-quote $'fn g() -> u8 { b\'"\' }\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail after-char-quote    $'fn g() -> char { \'"\' }\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail after-raw-string    $'const S: &str = r#"x " y"#;\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail after-hex-char-escape $'const CHARS: [char; 2] = [\'\\x41\', \'"\'];\n#[allow(clippy::disallowed_types)]\nfn f() {}\nconst S: &str = "later";'
expect_fail after-unicode-char-escape $'const CHARS: [char; 2] = [\'\\u{41}\', \'"\'];\n#[allow(clippy::disallowed_types)]\nfn f() {}\nconst S: &str = "later";'
expect_fail after-byte-hex-escape $'const B: [u8; 2] = [b\'\\x41\', b\'"\'];\n#[allow(clippy::disallowed_types)]\nfn f() {}\nconst S: &str = "later";'
expect_fail after-escaped-quote $'const S: &str = "a\\" b";\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail after-quote-in-comment $'/* " */\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail space-before-bracket $'# [allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail inner-space         $'#! [allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail inner-two-spaces    $'# ! [allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail comment-between     $'# /* note */ [allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail newline-between     $'#\n[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail raw-lint-ident      $'#[allow(clippy::r#disallowed_types)]\nfn f() {}'
expect_fail raw-tool-ident      $'#[allow(r#clippy::disallowed_types)]\nfn f() {}'
expect_fail raw-allow-ident     $'#[r#allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail old-lint-name       $'#[allow(clippy::disallowed_type)]\nfn f() {}'
expect_fail warnings-group      $'#[allow(warnings)]\nfn f() {}'
expect_fail warnings-wrapped    $'#[allow(\n    dead_code,\n    warnings,\n)]\nfn f() {}'
expect_fail expect-warnings     $'#[expect(warnings)]\nfn f() {}'
expect_fail raw-warnings        $'#[allow(r#warnings)]\nfn f() {}'
expect_fail string-tag-forgery  $'pub const NOTE: &str = "note:\n// disallowed-types: permanent, candid boundary type";\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail raw-string-tag-forgery $'pub const NOTE: &str = r#"note:\n// disallowed-types: permanent, candid boundary type"#;\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail block-comment-tag-forgery $'/* note:\n// disallowed-types: permanent, candid boundary type */\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail doc-attr-tag-forgery $'#[doc = "builds the map\n// disallowed-types: permanent, legacy boundary"]\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail block-comment-tag-same-line $'/*\n// disallowed-types: removed by B2\n*/ #[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail bare-lint-name      $'#[allow(renamed_and_removed_lints, disallowed_types)]\nfn f() {}'
expect_fail bare-old-name       $'#[allow(disallowed_type)]\nfn f() {}'
expect_fail bare-group-all      $'#[allow(all)]\nfn f() {}'
expect_fail bare-group-style    $'#![allow(renamed_and_removed_lints)]\n#[allow(style)]\nfn f() {}'
expect_fail bare-raw-lint-name  $'#[allow(r#disallowed_types)]\nfn f() {}'
expect_fail alias-clippy-style  $'#[allow(clippy_style)]\nfn f() {}'
expect_fail alias-clippy-all    $'#[expect(renamed_and_removed_lints, clippy_all)]\nfn f() {}'
expect_fail macro-metavariable  $'macro_rules! m {\n    ($l:ident) => {\n        #[allow(clippy::$l)]\n        fn f() {}\n    };\n}\nm!(disallowed_types);'
expect_fail macro-metavariable-spaced $'macro_rules! m {\n    ($l:ident) => {\n        #[allow(clippy::$ l)]\n        fn f() {}\n    };\n}\nm!(disallowed_types);'
expect_fail macro-metavariable-whole $'macro_rules! m {\n    ($l:path) => {\n        #[allow($l)]\n        fn f() {}\n    };\n}\nm!(clippy::disallowed_types);'
expect_fail unicode-ws-hash-bracket $'#\xe2\x80\x8e[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail unicode-ws-allow-paren $'#[allow\xc2\x85(clippy::disallowed_types)]\nfn f() {}'
expect_fail unicode-ws-lint-path $'#[allow(clippy\xe2\x80\xa8::disallowed_types)]\nfn f() {}'
expect_fail empty-permanent-reason $'// disallowed-types: permanent,  \n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail reason-not-a-tag    $'#[allow(clippy::disallowed_types, reason = "legacy")]\nfn f() {}'
expect_fail reason-tag-empty    $'#[allow(clippy::disallowed_types, reason = "disallowed-types: permanent, ")]\nfn f() {}'
expect_fail reason-tag-two-prs  $'#[allow(clippy::disallowed_types, reason = "disallowed-types: removed by B2 B3")]\nfn f() {}'
expect_fail reason-in-block-comment $'#[allow(clippy::disallowed_types /* reason = "disallowed-types: removed by B2" */)]\nfn f() {}'
expect_fail reason-in-line-comment $'#[allow(\n    clippy::disallowed_types, // reason = "disallowed-types: removed by B2"\n)]\nfn f() {}'
expect_fail reason-inside-raw-string $'#[allow(clippy::disallowed_types, reason = r#"see reason = "disallowed-types: removed by B2""#)]\nfn f() {}'
expect_fail reason-inside-other-string $'#[cfg_attr(all(), doc = "reason = \\"disallowed-types: removed by B2\\"", allow(clippy::disallowed_types))]\nfn f() {}'
expect_fail reason-in-neighbour-attr $'#[doc = "reason = \\"disallowed-types: removed by B2\\""]\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail tag-not-adjacent    $'// disallowed-types: removed by B2\n\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_fail tag-inside-attr     $'#[allow(\n    // disallowed-types: removed by B2\n    clippy::disallowed_types,\n)]\nfn f() {}'
expect_pass tagged-single       $'// disallowed-types: removed by B2\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_pass tagged-multi-line   $'// disallowed-types: removed by B2\n#[allow(\n    dead_code,\n    clippy::disallowed_types,\n)]\nfn f() {}'
expect_pass tagged-permanent    $'    // disallowed-types: permanent, Candid entry point\n    #[allow(clippy::disallowed_types)]\n    fn f() {}'
expect_pass tagged-cfg-attr     $'// disallowed-types: removed by A2.1\n#[cfg_attr(test, allow(clippy::disallowed_types))]\nfn f() {}'
expect_pass tagged-second-list  $'// disallowed-types: removed by A2.1\n#[cfg_attr(test, allow(dead_code), allow(clippy::disallowed_types))]\nfn f() {}'
expect_pass unrelated-two-lists $'#[cfg_attr(test, allow(dead_code), allow(unused_imports))]\nfn f() {}'
expect_pass lint-name-prefix    $'#[allow(clippy::allow_attributes)]\nfn f() {}'
expect_pass tagged-bracket-reason $'// disallowed-types: removed by B2\n#[allow(clippy::disallowed_types, reason = "legacy [B2]")]\nfn f() {}'
expect_pass lint-named-in-reason $'#[allow(dead_code, reason = "clippy::all is not suppressed here")]\nfn f() {}'
expect_pass attr-in-doc-comment $'/// Example: `#[allow(clippy::disallowed_types)]`\nfn f() {}'
expect_pass attr-in-string      $'const S: &str = "#[allow(clippy::disallowed_types)]";\nfn f() {}'
expect_pass tagged-space-before $'// disallowed-types: removed by B2\n# [allow(clippy::disallowed_types)]\nfn f() {}'
expect_pass tagged-newline-between $'// disallowed-types: removed by B2\n#\n[allow(clippy::disallowed_types)]\nfn f() {}'
expect_pass tagged-warnings     $'// disallowed-types: permanent, generated code\n#[allow(warnings)]\nfn f() {}'
expect_pass warn-not-allow      $'#[warn(clippy::disallowed_types)]\nfn f() {}'
expect_pass deny-warnings       $'#![deny(warnings)]\nfn f() {}'
expect_pass forbid-group        $'#![forbid(clippy::all)]\nfn f() {}'
expect_pass raw-unrelated       $'#[allow(clippy::r#needless_return)]\nfn f() {}'
expect_pass warnings-as-path    $'#[allow(my_tool::warnings)]\nfn f() {}'
expect_pass crlf-removed-by     $'// disallowed-types: removed by B2\r\n#[allow(clippy::disallowed_types)]\r\nfn f() {}\r'
expect_pass tag-trailing-space  $'// disallowed-types: removed by B2 \n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_pass tag-pr-number       $'// disallowed-types: removed by #4466\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_pass tag-pr-slug         $'// disallowed-types: removed by dfinity/internet-identity#4466\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_pass tag-pr-url          $'// disallowed-types: removed by https://github.com/dfinity/internet-identity/pull/4466\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_pass tag-no-space-after-slashes $'//disallowed-types: removed by B2\n#[allow(clippy::disallowed_types)]\nfn f() {}'
expect_pass reason-string-tag   $'#[allow(clippy::disallowed_types, reason = "disallowed-types: removed by B2")]\nfn f() {}'
expect_pass reason-string-permanent $'#[allow(clippy::disallowed_types, reason = "disallowed-types: permanent, Candid entry point")]\nfn f() {}'
expect_pass reason-raw-string   $'#[allow(clippy::disallowed_types, reason = r"disallowed-types: removed by B2")]\nfn f() {}'
expect_pass reason-after-comment $'#[allow(clippy::disallowed_types, reason = /* see the ladder */ "disallowed-types: removed by B2")]\nfn f() {}'
expect_pass reason-string-wrapped $'#[allow(\n    clippy::disallowed_types,\n    reason = "disallowed-types: removed by A2.1"\n)]\nfn f() {}'
expect_pass reason-string-after-use-reorder $'use std::cmp::Ordering;\n#[allow(clippy::disallowed_types, reason = "disallowed-types: removed by B2")]\nuse std::collections::HashMap;\nfn f() -> Ordering { let _: HashMap<u8, u8> = HashMap::new(); Ordering::Equal }'
expect_pass tag-above-block-comment-interior $'// disallowed-types: removed by B2\n#[allow(clippy::disallowed_types)]\nfn f() {}\n/* // disallowed-types: removed by B2 */'
expect_pass tagged-bare-name    $'// disallowed-types: removed by B2\n#[allow(renamed_and_removed_lints, disallowed_types)]\nfn f() {}'
expect_pass tagged-macro-metavariable $'macro_rules! m {\n    ($l:ident) => {\n        // disallowed-types: permanent, macro-assembled lint name\n        #[allow(clippy::$l)]\n        fn f() {}\n    };\n}\nm!(disallowed_types);'
expect_pass rustdoc-all         $'#[allow(rustdoc::all)]\nfn f() {}'
expect_pass rustdoc-spaced-all  $'#[allow(rustdoc :: all)]\nfn f() {}'
expect_pass rustdoc-raw-all     $'#[allow(rustdoc::r#all)]\nfn f() {}'
expect_pass other-tool-lint     $'#[allow(rustdoc::disallowed_types, unknown_lints)]\nfn f() {}'
expect_pass nonstandard-style   $'#![allow(nonstandard_style)]\nfn f() {}'
expect_pass ident-named-all     $'#[allow(unused, reason = "all")]\nfn all() {}'
expect_pass unrelated-allow     $'#[allow(\n    dead_code,\n    unused_imports,\n)]\nfn f() {}'
expect_pass derive-then-allow   $'#[derive(Debug)]\n// disallowed-types: removed by C1\n#[allow(clippy::disallowed_types)]\nstruct S;'
# Files reached through a symlink or with an upper-case extension are scanned.
mkdir -p "$tmp/symlink/src" "$tmp/symlink/elsewhere"
printf '%s\n' '#[allow(clippy::disallowed_types)]' 'fn f() {}' > "$tmp/symlink/elsewhere/real.rs"
ln -s ../elsewhere/real.rs "$tmp/symlink/src/linked.rs"
if "$checker" "$tmp/symlink/src" >/dev/null 2>&1; then
    echo "FAIL: 'symlink' should have been rejected" >&2
    exit 1
fi
mkdir -p "$tmp/upper"
printf '%s\n' '#[allow(clippy::disallowed_types)]' 'fn f() {}' > "$tmp/upper/Hidden.RS"
if "$checker" "$tmp/upper" >/dev/null 2>&1; then
    echo "FAIL: 'upper-case extension' should have been rejected" >&2
    exit 1
fi
echo "check-disallowed-types-tags.test.sh: ok"
