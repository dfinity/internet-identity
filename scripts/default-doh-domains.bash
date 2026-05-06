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
# What's on the list: major consumer mailbox providers that *don't*
# publish DNSSEC. Domains that DO publish DS records (Proton, GMX,
# Tutanota, mail.com, live.com, …) are deliberately excluded —
# the FE walks DNSSEC for them natively, and putting them on this
# list would let an unsigned-zone misconfiguration silently
# downgrade them to the canister's DoH path.
#
# Audited 2026-05 with `dig +short DS <domain>` cross-checked
# against both Google (8.8.8.8) and Cloudflare (1.1.1.1)
# resolvers. Re-run the audit when adding entries.

readonly DEFAULT_DOH_ALLOWED_DOMAINS=(
    # Google
    gmail.com
    googlemail.com
    # Microsoft (note: live.com IS DNSSEC-signed and is omitted here)
    outlook.com
    hotmail.com
    msn.com
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
