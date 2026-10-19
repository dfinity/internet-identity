#!/usr/bin/env bash
# Every `allow`/`expect` of `clippy::disallowed_types` under `src/` must carry,
# on the line directly above the attribute's opening `#[` line, a tag that
# either names the type-ladder PR retiring that raw use of a Candid boundary
# type
#     // disallowed-types: removed by <PR>
# or declares the use permanent with a reason
#     // disallowed-types: permanent, <why>
# The banned types are listed in `src/internet_identity/clippy.toml`.
#
# Comments, string literals and char literals are blanked out before the scan
# (newlines and offsets kept), whole attributes are matched with balanced
# brackets, and every `allow`/`expect` list inside an attribute is inspected
# with balanced parentheses. So an untagged allow written in any of these
# forms is rejected too, including when rustfmt wraps it over several lines:
#     #[allow(dead_code, clippy::disallowed_types)]
#     #[allow(clippy::disallowed_types, dead_code)]
#     #[expect(clippy::disallowed_types)]
#     #[allow(clippy::style)]            (a lint group containing it)
#     #[allow(clippy::all)]              (likewise)
#     #[allow(clippy :: disallowed_types)]
#     #[allow(clippy::disallowed_types, reason = "legacy [B2]")]
#     #[cfg_attr(test, allow(clippy::disallowed_types))]
#     #[cfg_attr(test, allow(dead_code), allow(clippy::disallowed_types))]
#     #[allow(
#         // see [B2]
#         clippy::disallowed_types,
#     )]
# and an attribute quoted inside a comment or a string is not an attribute.
# Known limit: nested block comments (`/* /* */ */`) are not understood.
# `scripts/check-disallowed-types-tags.test.sh` pins these cases.
#
# Usage: check-disallowed-types-tags.sh [DIR]   (DIR defaults to src/)
set -euo pipefail
cd "$(dirname "$0")/.."
dir="${1:-src}"

status=0
while IFS= read -r -d '' file; do
    if ! perl -0777 -ne '
        my $bad = 0;
        my @lines = split /\n/, $_, -1;
        # Blank out comments, string literals and char literals, keeping
        # every newline and byte offset, so a delimiter inside them cannot
        # cut an attribute short and text inside them is never scanned.
        (my $code = $_) =~ s{
              //[^\n]*                                   # line comment
            | /\*(?:(?!\*/).)*\*/                        # block comment
            | b?r(\#*)"(?:(?!"\1).)*"\1                  # raw string
            | b?"(?:[^"\\]|\\.)*"                        # string
            | b?\x27(?:[^\x27\\\n]|\\.)\x27              # char literal
        }{ (my $blank = $&) =~ s/[^\n]/ /g; $blank }gsex;
        # Each attribute, outer or inner, as a whole (brackets balanced).
        while ($code =~ /#!?\[((?:[^\[\]]++|\[(?1)\])*)\]/gs) {
            my ($attr, $start) = ($1, $-[0]);
            my $hit = 0;
            # Every allow/expect list inside it (parentheses balanced).
            while ($attr =~ /\b(?:allow|expect)\s*(\((?:[^()]++|(?1))*\))/gs) {
                $hit = 1 if $1 =~ /\bclippy\s*::\s*(?:disallowed_types|style|all)\b/;
            }
            next unless $hit;
            my $line = 1 + (substr($code, 0, $start) =~ tr/\n//);
            my $prev = $line > 1 ? $lines[$line - 2] : "";
            next if $prev =~ m{^\s*// disallowed-types: (?:removed by [A-Za-z0-9.-]+|permanent, .+)$};
            print STDERR "$ARGV:$line: allow/expect of clippy::disallowed_types (or a lint group containing it) without a \x27// disallowed-types: ...\x27 tag on the line above\n";
            $bad = 1;
        }
        exit $bad;
    ' "$file"; then
        status=1
    fi
done < <(find "$dir" -name '*.rs' -type f -print0 | sort -z)

if [ "$status" -eq 0 ]; then
    echo "disallowed-types allow tags: ok"
fi
exit "$status"
