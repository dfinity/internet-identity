#!/usr/bin/env bash
# Default DoH allowlist for the email-recovery flow.
#
# Single source of truth — sourced by both `deploy-common.bash`
# (used by `deploy-pr-to-beta` + `deploy-local-to-beta`) and by
# `make-upgrade-proposal` (production). Don't redefine this list
# anywhere else; if you need to override, use the deploy script's
# `--doh-domains` flag or the `II_DOH_DOMAINS` env var, both of
# which take a comma-separated list and accept the empty string
# as "disable DoH entirely (DNSSEC-only)".
#
# What's on the list: major consumer mailbox providers whose DKIM
# resolution can't be authenticated end-to-end via DNSSEC. The
# audit criterion is *not* "does the apex publish DS" — what
# matters is whether the DKIM CNAME chain stays in signed
# territory. A domain whose apex is signed but whose DKIM TXT
# lives behind a CNAME in an unsigned zone (live.com → unsigned
# protection.outlook.com) belongs on this list anyway, because
# the canister's DNSSEC verifier can't validate the resolution.
#
# Domains explicitly *excluded* (DNSSEC works end-to-end):
# - protonmail.com, pm.me — DKIM TXT lives in the same signed zone.
# - proton.me — DKIM CNAMEs into proton.ch (different signed zone).
# - tutanota.com / tuta.com / tuta.io — DKIM CNAMEs into tutanota.de
#   (different signed zone).
# The DNSSEC verifier handles the cross-zone CNAME case via
# multi-zone bundles (see docs/ongoing/email-recovery.md §7.2).
# Putting these on the DoH list would silently downgrade them.
#
# Audit procedure: resolve `<active-selector>._domainkey.<domain>`
# end-to-end via a DNSSEC-validating resolver and check the AD bit
# on the response. AD=1 → DNSSEC works; AD=0 → add to this list.
# Re-run the audit when adding entries.

readonly DEFAULT_DOH_ALLOWED_DOMAINS=(
    # Google
    gmail.com
    googlemail.com
    # Microsoft (live.com: apex is signed but DKIM CNAMEs into
    #  unsigned protection.outlook.com, so the DNSSEC chain breaks
    #  end-to-end — DoH path is the only workable verification).
    outlook.com
    hotmail.com
    msn.com
    live.com
    # Apple
    icloud.com
    me.com
    mac.com
    # Yahoo / Verizon Media
    yahoo.com
    ymail.com
    aol.com
    # Other Western providers
    zoho.com
    fastmail.com
    fastmail.fm
    hey.com
    # Yandex / Mail.ru (Russia)
    yandex.com
    yandex.ru
    mail.ru
    # Asia
    qq.com
    163.com
    126.com
    naver.com
    daum.net
)
