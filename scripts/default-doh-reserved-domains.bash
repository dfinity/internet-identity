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
    # Verizon (hosted by AOL)
    verizon.net
    # AT&T (hosted by Yahoo)
    att.net
    sbcglobal.net
    bellsouth.net
    pacbell.net
    swbell.net
    ameritech.net
    flash.net
    prodigy.net
    nvbell.net
    wans.net
    snet.net
    currently.com
    # Frontier (hosted by Yahoo)
    frontier.com
    frontiernet.net
    # Cox (hosted by Yahoo)
    cox.net
    # Comcast
    comcast.net
    # EarthLink
    earthlink.net
    mindspring.com
    # Optimum
    optonline.net
    optimum.net
    # Rogers / Shaw
    rogers.com
    shaw.ca
    # Bell
    bell.net
    sympatico.ca
    # TELUS
    telus.net
    # Telstra
    bigpond.com
    bigpond.net.au
    telstra.com
    # Optus
    optusnet.com.au
    # iiNet
    iinet.net.au
    # Spark
    xtra.co.nz
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
    # Deutsche Telekom
    t-online.de
    magenta.de
    # freenet
    freenet.de
    # Vodafone Germany
    vodafonemail.de
    arcor.de
    # BT
    btinternet.com
    btopenworld.com
    talk21.com
    # Sky
    sky.com
    # Virgin Media
    virginmedia.com
    ntlworld.com
    blueyonder.co.uk
    virgin.net
    # TalkTalk
    talktalk.net
    tiscali.co.uk
    # Orange
    orange.fr
    wanadoo.fr
    # Free
    free.fr
    # La Poste
    laposte.net
    # Bouygues Telecom
    bbox.fr
    # Italiaonline
    libero.it
    virgilio.it
    iol.it
    inwind.it
    blu.it
    # TIM
    tim.it
    alice.it
    tin.it
    # Tiscali Italy
    tiscali.it
    # Fastweb
    fastwebnet.it
    # Movistar
    telefonica.net
    movistar.es
    # Ziggo
    ziggo.nl
    home.nl
    # KPN
    kpnmail.nl
    planet.nl
    hetnet.nl
    xs4all.nl
    # Telenet
    telenet.be
    # Proximus
    proximus.be
    skynet.be
    # Swisscom
    bluewin.ch
    # Seznam
    seznam.cz
    email.cz
    post.cz
    # Centrum
    centrum.cz
    atlas.cz
    volny.cz
    # WP / o2
    wp.pl
    o2.pl
    tlen.pl
    # Interia
    interia.pl
    interia.eu
    # Onet
    onet.pl
    op.pl
    vp.pl
    poczta.onet.pl
    # Gazeta.pl
    gazeta.pl
    # Freemail (Hungary)
    freemail.hu
    # ABV (Bulgaria)
    abv.bg
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
    # Rambler
    rambler.ru
    lenta.ru
    myrambler.ru
    autorambler.ru
    ro.ru
    # ukr.net
    ukr.net
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
    # Sina
    sina.com
    sina.cn
    # Sohu
    sohu.com
    # Alibaba
    aliyun.com
    # China Mobile
    139.com
    # Naver
    naver.com
    # Kakao / Daum
    daum.net
    hanmail.net
    kakao.com
    # NTT docomo
    docomo.ne.jp
    # KDDI au
    au.com
    ezweb.ne.jp
    # SoftBank
    softbank.ne.jp
    i.softbank.jp
    # BIGLOBE
    biglobe.ne.jp
    # OCN
    ocn.ne.jp
    # Rediff
    rediffmail.com
    # UOL
    uol.com.br
    bol.com.br
    # Zoho
    zoho.com
    zohomail.com
    zohomail.in
    zohomail.eu
    zohomail.com.au
    # Fastmail
    fastmail.com
    fastmail.fm
    # HEY
    hey.com
    # Proton
    proton.me
    protonmail.com
    protonmail.ch
    pm.me
    # Tuta
    tuta.com
    tutanota.com
    tutanota.de
    tutamail.com
    tuta.io
    keemail.me
)
