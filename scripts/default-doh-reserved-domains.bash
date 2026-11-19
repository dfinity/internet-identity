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
# them. What's on the list: the domains the major consumer mailbox
# providers give new accounts, which carry most of the traffic.

readonly DEFAULT_DOH_RESERVED_DOMAINS=(
    # Google
    gmail.com
    # Microsoft
    outlook.com
    hotmail.com
    # Apple
    icloud.com
    # Yahoo
    yahoo.com
    # Yahoo Japan
    yahoo.co.jp
    # AOL
    aol.com
    # GMX
    gmx.de
    gmx.net
    gmx.com
    # WEB.DE
    web.de
    # mail.com
    mail.com
    # Proton
    proton.me
    # Zoho
    zohomail.com
    # Mail.ru
    mail.ru
    # Yandex
    yandex.ru
    yandex.com
    # NetEase
    163.com
    # Tencent
    qq.com
    # Naver
    naver.com
)
