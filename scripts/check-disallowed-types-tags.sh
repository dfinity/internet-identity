#!/usr/bin/env bash
# Every `allow`/`expect` of `clippy::disallowed_types` under `src/` must carry,
# on the line directly above it, a tag that either names the type-ladder PR
# retiring that raw use of a Candid boundary type
#     // disallowed-types: removed by <PR>
# or declares the use permanent with a reason
#     // disallowed-types: permanent, <why>
# The banned types are listed in `src/internet_identity/clippy.toml`.
#
# The match covers every attribute that would silence the lint, so an
# untagged allow written in any of these forms is rejected too:
#     #[allow(dead_code, clippy::disallowed_types)]
#     #[allow(clippy::disallowed_types, dead_code)]
#     #[expect(clippy::disallowed_types)]
#     #[allow(clippy::style)]            (a lint group containing it)
#     #[allow(clippy::all)]              (likewise)
#     #[cfg_attr(test, allow(clippy::disallowed_types))]
set -euo pipefail
cd "$(dirname "$0")/.."

allow_pattern='(allow|expect)\([^)]*clippy::(disallowed_types|style|all)([[:space:],)]|$)'
tag_pattern='^[[:space:]]*// disallowed-types: (removed by [A-Za-z0-9.-]+|permanent, .+)$'
status=0
while IFS=: read -r file line _; do
    prev=""
    if [ "$line" -gt 1 ]; then
        prev=$(sed -n "$((line - 1))p" "$file")
    fi
    if ! printf '%s\n' "$prev" | grep -Eq "$tag_pattern"; then
        echo "$file:$line: allow/expect of clippy::disallowed_types (or a lint group containing it) without a '// disallowed-types: ...' tag on the line above" >&2
        status=1
    fi
done < <(grep -rnE --include='*.rs' "$allow_pattern" src)

if [ "$status" -eq 0 ]; then
    echo "disallowed-types allow tags: ok"
fi
exit "$status"
