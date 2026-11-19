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
    live.com
    msn.com
    passport.com
    windowslive.com
    livemail.com.br
    livemail.tw
    windowslive.es
    # Microsoft outlook.* country domains
    outlook.at
    outlook.be
    outlook.cl
    outlook.co.id
    outlook.co.il
    outlook.co.nz
    outlook.co.th
    outlook.com.ar
    outlook.com.au
    outlook.com.br
    outlook.com.gr
    outlook.com.pe
    outlook.com.tr
    outlook.com.vn
    outlook.cz
    outlook.de
    outlook.dk
    outlook.es
    outlook.fr
    outlook.hu
    outlook.ie
    outlook.in
    outlook.it
    outlook.jp
    outlook.kr
    outlook.lv
    outlook.my
    outlook.ph
    outlook.pt
    outlook.sa
    outlook.sg
    outlook.sk
    # Microsoft hotmail.* country domains
    hotmail.be
    hotmail.ca
    hotmail.ch
    hotmail.cl
    hotmail.co.id
    hotmail.co.il
    hotmail.co.in
    hotmail.co.jp
    hotmail.co.kr
    hotmail.co.nz
    hotmail.co.th
    hotmail.co.uk
    hotmail.co.za
    hotmail.com.ar
    hotmail.com.au
    hotmail.com.br
    hotmail.com.hk
    hotmail.com.tr
    hotmail.com.tw
    hotmail.com.vn
    hotmail.cz
    hotmail.de
    hotmail.dk
    hotmail.es
    hotmail.fi
    hotmail.fr
    hotmail.gr
    hotmail.hu
    hotmail.it
    hotmail.lt
    hotmail.lv
    hotmail.my
    hotmail.nl
    hotmail.no
    hotmail.ph
    hotmail.rs
    hotmail.se
    hotmail.sg
    hotmail.sk
    # Microsoft live.* country domains
    live.at
    live.be
    live.ca
    live.cl
    live.cn
    live.co.in
    live.co.kr
    live.co.uk
    live.co.za
    live.com.ar
    live.com.au
    live.com.co
    live.com.mx
    live.com.my
    live.com.pe
    live.com.ph
    live.com.pk
    live.com.pt
    live.com.sg
    live.com.ve
    live.de
    live.dk
    live.fi
    live.fr
    live.hk
    live.ie
    live.in
    live.it
    live.jp
    live.nl
    live.no
    live.ph
    live.ru
    live.se
    # Apple
    icloud.com
    me.com
    mac.com
    # Yahoo
    yahoo.com
    myyahoo.com
    ymail.com
    rocketmail.com
    # Yahoo country domains
    yahoo.co.uk
    yahoo.fr
    yahoo.it
    # Yahoo Japan
    yahoo.co.jp
    ymail.ne.jp
    yahoo.ne.jp
    # AOL
    aol.com
    aim.com
    love.com
    wow.com
    ygm.com
    # GMX
    gmx.de
    gmx.net
    gmx.com
    gmx.us
    gmx.co.uk
    gmx.ca
    gmx.eu
    gmx.org
    gmx.info
    gmx.biz
    gmx.tm
    mein.gmx
    mail.gmx
    email.gmx
    # WEB.DE
    web.de
    # mail.com
    mail.com
    email.com
    usa.com
    post.com
    consultant.com
    myself.com
    dr.com
    europe.com
    engineer.com
    asia.com
    # Mail.ru
    mail.ru
    inbox.ru
    list.ru
    bk.ru
    internet.ru
    # Yandex
    yandex.ru
    yandex.com
    ya.ru
    yandex.by
    yandex.kz
    # NetEase
    163.com
    126.com
    yeah.net
    188.com
    vip.163.com
    vip.126.com
    # Tencent
    qq.com
    foxmail.com
    vip.qq.com
    # Naver
    naver.com
    # Zoho
    zoho.com
    zohomail.com
    zohomail.in
    zohomail.eu
    zohomail.com.au
    # Proton
    proton.me
    protonmail.com
    protonmail.ch
    pm.me
)
