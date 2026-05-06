#!/usr/bin/env bash
# Fetch the IANA DNSSEC root trust anchors and emit them as Candid
# `vec DnssecRootAnchor` text suitable for the II install-arg
# `dnssec_config.root_anchors` field.
#
# Why we need a "trusted" anchor inside an init arg:
#
#   The canister-side DNSSEC verifier (PR #3838) takes a `DnsProofBundle`
#   from the FE and validates its `root_dnskey` against a configured
#   set of `DnssecRootAnchor` records — algo, digest_type, key_tag,
#   digest. Those four values *are* the IANA root KSK trust anchors
#   published at https://data.iana.org/root-anchors/root-anchors.xml.
#   We don't bundle them into the wasm because they roll over every
#   ~7 years; instead the deploy/upgrade arg carries them, so a
#   key rollover is a one-line change in the next upgrade.
#
# This script:
#
#   1. `curl`s the IANA XML.
#   2. Parses the `<KeyDigest>` entries with Node (we already depend
#      on Node for the FE build), filtering to those whose validity
#      window contains "now" — i.e. dropping retired KSKs (Kjqmt7v,
#      retired 2019) and warning if a not-yet-valid one is present.
#   3. Renders each entry as a one-line summary on stderr for human
#      review (`--review` mode), then prints the Candid record-list
#      to stdout.
#
# The Candid output shape matches `DnssecRootAnchor` in
# `src/internet_identity_interface/src/internet_identity/types/dnssec.rs`:
#
#   record {
#     key_tag : nat16;
#     algorithm : nat8;
#     digest_type : nat8;
#     digest : blob;
#   }
#
# Usage:
#   source scripts/fetch-iana-root-anchors.bash
#   anchors_candid=$(fetch_and_review_iana_root_anchors)
#   echo "$anchors_candid"   # → "vec { record { ... }; record { ... }; }"
#
# The function exits non-zero (and the caller should bail) if the
# user declines the on-screen review, the network fetch fails, or
# parsing turns up something unexpected.

readonly IANA_ROOT_ANCHORS_URL="https://data.iana.org/root-anchors/root-anchors.xml"

# Internal: fetch the XML to stdout, exit non-zero on transport failure.
_iana_fetch_xml() {
    curl --fail --silent --show-error --location \
         --retry 3 --connect-timeout 10 --max-time 30 \
         "$IANA_ROOT_ANCHORS_URL"
}

# Internal: parse the XML on stdin, emit JSON `[{...}, ...]` on stdout
# with one entry per *currently-valid* KeyDigest. Invalid (retired or
# not-yet-valid) entries are dropped silently — the script's job is
# to surface what the canister should trust right now.
_iana_parse_xml_to_json() {
    node --input-type=module -e '
        let xml = "";
        process.stdin.setEncoding("utf8");
        process.stdin.on("data", (chunk) => { xml += chunk; });
        process.stdin.on("end", () => {
            try {
                const now = new Date();
                const blocks = [...xml.matchAll(/<KeyDigest\b([^>]*)>([\s\S]*?)<\/KeyDigest>/g)];
                const out = [];
                for (const [, attrStr, body] of blocks) {
                    const attrs = {};
                    for (const m of attrStr.matchAll(/(\w+)="([^"]*)"/g)) {
                        attrs[m[1]] = m[2];
                    }
                    const validFrom = attrs.validFrom ? new Date(attrs.validFrom) : null;
                    const validUntil = attrs.validUntil ? new Date(attrs.validUntil) : null;
                    if (validFrom && validFrom > now) continue;
                    if (validUntil && validUntil < now) continue;

                    const child = (tag) => {
                        const m = body.match(new RegExp(`<${tag}>([\\s\\S]*?)</${tag}>`));
                        return m ? m[1].trim() : null;
                    };
                    const keyTag = child("KeyTag");
                    const algorithm = child("Algorithm");
                    const digestType = child("DigestType");
                    const digest = child("Digest");
                    if (!keyTag || !algorithm || !digestType || !digest) {
                        process.stderr.write(`KeyDigest ${attrs.id || "(no id)"} missing required fields, skipping\n`);
                        continue;
                    }
                    out.push({
                        id: attrs.id || "",
                        validFrom: attrs.validFrom || null,
                        validUntil: attrs.validUntil || null,
                        keyTag: Number(keyTag),
                        algorithm: Number(algorithm),
                        digestType: Number(digestType),
                        digest: digest.replace(/\s+/g, "").toUpperCase(),
                    });
                }
                if (out.length === 0) {
                    process.stderr.write("No currently-valid KeyDigest entries found in IANA XML.\n");
                    process.exit(1);
                }
                process.stdout.write(JSON.stringify(out));
            } catch (e) {
                process.stderr.write(`IANA XML parse failed: ${e.message}\n`);
                process.exit(1);
            }
        });
    '
}

# Internal: convert the JSON list to Candid `vec record { ... }` text.
# Reads JSON on stdin, writes Candid on stdout.
_iana_json_to_candid() {
    node --input-type=module -e '
        let json = "";
        process.stdin.setEncoding("utf8");
        process.stdin.on("data", (chunk) => { json += chunk; });
        process.stdin.on("end", () => {
            const list = JSON.parse(json);
            const escapeBlobBytes = (hex) => {
                // Candid blob literal: `blob "\HH\HH..."`, hex bytes
                // escaped one byte at a time.
                let out = "";
                for (let i = 0; i < hex.length; i += 2) {
                    out += "\\" + hex.slice(i, i + 2).toLowerCase();
                }
                return out;
            };
            const lines = ["vec {"];
            for (const a of list) {
                lines.push("  record {");
                lines.push(`    key_tag = ${a.keyTag} : nat16;`);
                lines.push(`    algorithm = ${a.algorithm} : nat8;`);
                lines.push(`    digest_type = ${a.digestType} : nat8;`);
                lines.push(`    digest = blob "${escapeBlobBytes(a.digest)}";`);
                lines.push("  };");
            }
            lines.push("}");
            process.stdout.write(lines.join("\n"));
        });
    '
}

# Internal: print one human-readable summary line per parsed entry to
# stderr. Used by the --review prompt.
_iana_pretty_print_to_stderr() {
    node --input-type=module -e '
        let json = "";
        process.stdin.setEncoding("utf8");
        process.stdin.on("data", (chunk) => { json += chunk; });
        process.stdin.on("end", () => {
            const list = JSON.parse(json);
            for (const a of list) {
                const valid =
                    "[" + (a.validFrom || "?") +
                    " — " + (a.validUntil || "no-expiry") + "]";
                const digestPreview =
                    a.digest.slice(0, 8) + "…" + a.digest.slice(-8);
                process.stderr.write(
                    `  ${a.id.padEnd(10)}  key_tag=${String(a.keyTag).padEnd(6)}` +
                    `  alg=${a.algorithm}  digest_type=${a.digestType}` +
                    `  digest=${digestPreview}  ${valid}\n`,
                );
            }
        });
    '
}

# Public: fetch + review + emit the Candid value on stdout.
#
# - On success: writes `vec { record { ... }; ... }` to stdout, returns 0.
# - On user decline / fetch failure / parse failure: writes an error
#   to stderr and returns non-zero. Caller should `exit 1`.
#
# Honours the existing `is_interactive` helper from deploy-common.bash
# when sourced from there; falls back to a static check on /dev/tty
# otherwise so this script stays usable on its own.
fetch_and_review_iana_root_anchors() {
    local interactive
    if declare -f is_interactive > /dev/null; then
        if is_interactive; then interactive=true; else interactive=false; fi
    elif [ -r /dev/tty ] && [ -w /dev/tty ]; then
        interactive=true
    else
        interactive=false
    fi

    echo "Fetching IANA DNSSEC root anchors from $IANA_ROOT_ANCHORS_URL …" >&2
    local xml json
    if ! xml="$(_iana_fetch_xml)"; then
        echo "Error: could not fetch IANA root anchors XML." >&2
        return 1
    fi
    if ! json="$(printf '%s' "$xml" | _iana_parse_xml_to_json)"; then
        return 1
    fi

    echo "" >&2
    echo "Currently-valid IANA root KSK trust anchors:" >&2
    printf '%s' "$json" | _iana_pretty_print_to_stderr
    echo "" >&2

    # Best-effort verify that /dev/tty actually opens — `[ -r/-w
    # /dev/tty ]` reports yes whenever the device file exists, even
    # when the process has no controlling terminal (sandboxes,
    # unit-tested CI runners). Try to acquire the tty as the prompt
    # would; if that fails, treat as non-interactive regardless of
    # what the upstream check decided.
    if [ "$interactive" = true ] && ! { exec 3>/dev/tty; } 2>/dev/null; then
        interactive=false
    fi
    if [ "$interactive" = true ]; then
        local reply=""
        printf '%s' "Use these anchors as dnssec_config.root_anchors? [Y/n] " >&3
        exec 3>&-
        IFS= read -r reply < /dev/tty || reply=""
        case "$reply" in
            ""|y|Y|yes|YES) ;;
            *)
                echo "Aborted by user." >&2
                return 1
                ;;
        esac
    else
        echo "Non-interactive run: using fetched anchors without prompt." >&2
    fi

    printf '%s' "$json" | _iana_json_to_candid
}
