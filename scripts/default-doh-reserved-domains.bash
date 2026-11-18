#!/usr/bin/env bash
# Default `DohConfig.reserved_domains` for the email flows.
#
# Single source of truth — sourced by both `deploy-common.bash`
# (used by `deploy-pr-to-beta` + `deploy-local-to-beta`) and by
# `make-upgrade-proposal` (production). Don't redefine this list
# anywhere else; if you need to override, use the deploy script's
# `--doh-reserved-domains` flag or the `II_DOH_RESERVED_DOMAINS` env
# var, both of which take a comma-separated list and accept the empty
# string as "reserve no domain".
#
# The canister resolves DKIM/DMARC over DoH for every email domain.
# Lookups for the domains listed here draw from an outcall budget of
# their own, so a burst of lookups for arbitrary domains can't starve
# them. What's on the list: the major consumer mailbox providers,
# which carry most of the traffic.

readonly DEFAULT_DOH_RESERVED_DOMAINS=(
    # Google
    gmail.com
    googlemail.com
    # Microsoft
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
