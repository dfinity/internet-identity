# The concept's commits, by block

The branch's 175 commits up to `1d97302fd`, each under the blocks whose files it changed (see [README.md](README.md)), oldest first. A commit that changed several blocks is under each. From `1d97302fd` on every block has a folder of its own, so a later block's commits are

```sh
git log --oneline main.. -- "src/frontend/src/routes/(new-styling)/concept/features/apps"
```

## 1 · Shared product code (6)

Outside the concept: these change the product's own screens.

- `ac92bebaf` chore(concept): checkpoint the concept before the swipe depth effect
- `31337455c` feat(concept): show an identity's initials on colours of its own
- `1aab8f139` feat(concept): frost the avatar's sign-in badge
- `c8dcabcc8` feat(concept): centre one initial on an identity's avatar
- `b0bbf6b98` feat(concept): identity menu and Customize, search in Explore and Add app, recovery as one flow
- `a17d7ddc3` feat(concept): logos fade in once loaded, Explore and Connect wait

## 2 · Surfaces (19)

`foundation/surfaces`

- `ac92bebaf` chore(concept): checkpoint the concept before the swipe depth effect
- `7cffc8330` fix(concept): paint the overscroll area in the page's own surface
- `abc700f99` feat(concept): put Recent on a shelf of its own
- `76cf1a8a6` feat(concept): grey a muted app's unread count
- `2c966f5df` feat(concept): group the account's rows and part them like Settings
- `700000212` feat(concept): the panel's cards stand level with the canvas
- `1c8ac16b1` fix(concept): warning and error sit in a tint of their own color
- `5272dcf2f` feat(concept): a wide canvas shows the apps larger, not more of them
- `56b784a5e` fix(concept): a phone's sheets stay put, and the page under them too
- `16708e245` feat(concept): on desktop the apps and the panel share one surface
- `452be33a4` feat(concept): on desktop the apps are in a frame again, the panel beside it
- `a4ae490c8` feat(concept): the identity card's rows fade in as it opens
- `45fa89b04` feat(concept): surfaces only go up, in words and in a test
- `9b3f9a179` fix(concept): a sheet floats, and what it holds stands on it
- `40dca2fed` fix(concept): Edit's setting card stands on the canvas, not in it
- `04ca0dc07` fix(concept): an empty selection mark lies on the logo, not in it
- `8869bd523` fix(concept): a section's plain cue stands on its rows
- `afdb722c5` fix(concept): the identity stack's +N is a tint of its card
- `1d97302fd` chore(concept): the concept's files in folders by block

## 3 · Motion and loading (18)

`foundation/motion`

- `ac92bebaf` chore(concept): checkpoint the concept before the swipe depth effect
- `520daecb0` feat(concept): blur the pager's edges progressively
- `5a6448c82` feat(concept): keep the header in place and blur content into it
- `eed00fefd` feat(concept): run the header's fade on under the header
- `39c1f5435` feat(concept): float the header as pills instead of fading into it
- `12cb05ec8` feat(concept): hop the dock now and then while a step waits
- `b57674756` fix(concept): exits ease out, so the old view is gone before the new one lands
- `7cd3db15f` feat(concept): a panel card grows into its section, and back
- `8dda760c7` feat(concept): an open section keeps the identity above it
- `9975029e7` feat(concept): a tab switches pages with a short shift and a fade
- `356c85c09` feat(concept): one compact identity card heads every view of the panel
- `2523d3ae9` feat(concept): an open identity card lists each identity once
- `65ebd180a` fix(concept): the avatars' fallback fade takes no unused argument
- `e156a370b` feat(concept): on a large screen the controls and the panel grow
- `a4ae490c8` feat(concept): the identity card's rows fade in as it opens
- `cde64f5e9` feat(concept): the page loads like an app, skeletons where data goes
- `45fa89b04` feat(concept): surfaces only go up, in words and in a test
- `1d97302fd` chore(concept): the concept's files in folders by block

## 4 · Frame (27)

`foundation/frame`, and the frame's part of `+page.svelte`

- `ac92bebaf` chore(concept): checkpoint the concept before the swipe depth effect
- `1bf6f98ea` feat(concept): turn app tiles in depth as their page is swiped
- `5d27be993` Revert "feat(concept): turn app tiles in depth as their page is swiped"
- `e68034281` chore(concept): checkpoint the concept before the fixed header and Explore feed
- `048cae12e` feat(concept): turn each dock widget into the button for its step
- `8749566a2` feat(concept): give Manage apps a grid with a check
- `a31df4bf8` feat(concept): act on picked apps from the selection island
- `a8061cb2c` chore(concept): checkpoint the concept before the account panel rework
- `b4b897721` feat(concept): keep the account still as its panel opens
- `88b989600` feat(concept): switch identity inside the panel, not over it
- `51456a46c` feat(concept): mark the closed account when something needs you
- `7d2974245` feat(concept): stack the dock's widgets behind the step to take
- `280d3e619` feat(concept): head the panel with the identity, close it from the canvas
- `12cb05ec8` feat(concept): hop the dock now and then while a step waits
- `8c31f1747` feat(concept): hop the dock on a steady beat
- `740bcbf29` feat(concept): tell a risk as one incident at the head of the panel
- `b3ab1018f` feat(concept): menus and the app search on the surface ladder, search as a pill
- `fd6a0ce5c` fix(concept): lift the Add app popover to the menus' surface level
- `12b4c90a3` feat(concept): on a phone the panel is the head above the tabs
- `66a3a838b` feat(concept): the cards speak in Thomas's four styles and his states
- `4026aa356` feat(concept): on a phone, sections are sheets you swipe away
- `56b784a5e` fix(concept): a phone's sheets stay put, and the page under them too
- `2c6cb5159` feat(concept): beside the canvas, an app's options and managing apps are one card
- `d86511339` feat(concept): an open card's head closes it, Close at the icon's top
- `45fa89b04` feat(concept): surfaces only go up, in words and in a test
- `9b3f9a179` fix(concept): a sheet floats, and what it holds stands on it
- `1d97302fd` chore(concept): the concept's files in folders by block

## 5 · Identity (23)

`features/identity`

- `b4b897721` feat(concept): keep the account still as its panel opens
- `88b989600` feat(concept): switch identity inside the panel, not over it
- `1023cca3a` feat(concept): preview the identities to switch to as avatars
- `280d3e619` feat(concept): head the panel with the identity, close it from the canvas
- `b0bbf6b98` feat(concept): identity menu and Customize, search in Explore and Add app, recovery as one flow
- `b3ab1018f` feat(concept): menus and the app search on the surface ladder, search as a pill
- `618f74a3d` feat(concept): the greeting names the identity once; the card switches
- `4fb18e582` feat(concept): the identity's colour is picked inline from its avatar
- `12b4c90a3` feat(concept): on a phone the panel is the head above the tabs
- `2b4a5462e` chore(concept): format the new cards and identity pieces; statuses in status.ts
- `e4be7cdb8` feat(concept): on a phone the identity sits in the top corner
- `97be4d514` feat(concept): the identity card is one button, the avatar its colour
- `8a50756fb` feat(concept): on a phone, the identity's menu is a sheet too
- `23f4673c7` feat(concept): no line under the identity's name when it opens
- `356c85c09` feat(concept): one compact identity card heads every view of the panel
- `1a80137cc` feat(concept): the colour strip offers five colours, not ten
- `71bca5981` feat(concept): the identity card names how this browser signed in
- `59ddd603b` fix(concept): the identity's name stands on the cards' title line
- `2523d3ae9` feat(concept): an open identity card lists each identity once
- `a4ae490c8` feat(concept): the identity card's rows fade in as it opens
- `45fa89b04` feat(concept): surfaces only go up, in words and in a test
- `afdb722c5` fix(concept): the identity stack's +N is a tint of its card
- `1d97302fd` chore(concept): the concept's files in folders by block

## 6 · Security (44)

`features/security`

- `ac92bebaf` chore(concept): checkpoint the concept before the swipe depth effect
- `048cae12e` feat(concept): turn each dock widget into the button for its step
- `d926520b5` feat(concept): verify the recovery phrase with the product's wizard
- `8749566a2` feat(concept): give Manage apps a grid with a check
- `2c966f5df` feat(concept): group the account's rows and part them like Settings
- `5b00cfb65` feat(concept): put a row's step at the end of the row
- `740bcbf29` feat(concept): tell a risk as one incident at the head of the panel
- `b2d5721a7` feat(concept): meet a phone with the incident in a sheet over the apps
- `1384735dd` fix(concept): keep the devices row to its part of the incident
- `2e4b11285` feat(concept): end the incident banner in a chevron, on the logos' edges
- `e1d60c591` feat(concept): turn the Create page into Connect, for AI and CLI access
- `b0bbf6b98` feat(concept): identity menu and Customize, search in Explore and Add app, recovery as one flow
- `b733a479b` feat(concept): panel rows preview what's inside; Integrations leaves the panel
- `e51f04daa` feat(concept): recovery is one dialog with both ways back in and their status
- `7a1a648e4` feat(concept): Access, Recovery and Devices as three large cards
- `2b4a5462e` chore(concept): format the new cards and identity pieces; statuses in status.ts
- `700000212` feat(concept): the panel's cards stand level with the canvas
- `1c8ac16b1` fix(concept): warning and error sit in a tint of their own color
- `7cd3db15f` feat(concept): a panel card grows into its section, and back
- `888099a06` feat(concept): "Access methods" is just "Access"
- `66a3a838b` feat(concept): the cards speak in Thomas's four styles and his states
- `9a37d7b8b` feat(concept): a card says one thing, with the icons of what it's about
- `ae72e2a61` feat(concept): an open card says its message on the item it's about
- `aad74d7b5` feat(concept): a board with every card state, for Thomas
- `4026aa356` feat(concept): on a phone, sections are sheets you swipe away
- `0254e96a0` feat(concept): on a narrow phone, the head's cards are rows
- `682ccd355` feat(concept): a card's icons stand a step above it, with a small shadow
- `61f93dd94` feat(concept): a card's icons say what they are on hover
- `31eb1fa60` fix(concept): a card's tooltip hides instead of leaving
- `1267c6b8b` feat(concept): Devices lists one row a browser, named whole
- `7259118d7` feat(concept): a browser's Sign out is a tertiary button
- `c669c4cca` feat(concept): an unchecked phrase and a pending email are states again
- `405dbed60` feat(concept): signed in through recovery, by the phrase or the email
- `c193ac236` feat(concept): where the window is tall, the panel's cards take their room
- `90a8b6c85` feat(concept): an open card's rows lie on a layer of their own
- `2c6cb5159` feat(concept): beside the canvas, an app's options and managing apps are one card
- `d7e73db55` feat(concept): the dashed slots overlap as the coins do
- `d86511339` feat(concept): an open card's head closes it, Close at the icon's top
- `c68391a59` Revert "feat(concept): on a narrow phone, the head's cards are rows"
- `cde64f5e9` feat(concept): the page loads like an app, skeletons where data goes
- `45fa89b04` feat(concept): surfaces only go up, in words and in a test
- `9b3f9a179` fix(concept): a sheet floats, and what it holds stands on it
- `8869bd523` fix(concept): a section's plain cue stands on its rows
- `1d97302fd` chore(concept): the concept's files in folders by block

## 7 · My apps (31)

`features/apps`

- `ac92bebaf` chore(concept): checkpoint the concept before the swipe depth effect
- `e68034281` chore(concept): checkpoint the concept before the fixed header and Explore feed
- `eaaafff74` feat(concept): shrink the unread count onto the logo's corner
- `219ea6f9f` feat(concept): match the selection marks to the unread count
- `4ee0766ac` feat(concept): show an app's unread count once
- `abc700f99` feat(concept): put Recent on a shelf of its own
- `1e5ea5be5` feat(concept): show an arrow on a hovered tile where its count sits
- `ee3224db7` feat(concept): call the tile's way into managing "Select"
- `1b606d7af` feat(concept): mark an app whose notifications are turned off
- `76cf1a8a6` feat(concept): grey a muted app's unread count
- `a8061cb2c` chore(concept): checkpoint the concept before the account panel rework
- `ab6008241` feat(concept): one My apps grid with history, app details on click, confirm removal
- `b0bbf6b98` feat(concept): identity menu and Customize, search in Explore and Add app, recovery as one flow
- `29ea0fa58` feat(concept): the Edit switch decides whether an identity keeps app history
- `1e3202fe4` feat(concept): a new identity starts with three featured apps in My apps
- `70cfc1345` feat(concept): drop the Recently used row under the app grid
- `558e893ff` feat(concept): a tile opens its app; its corner's "⋯" opens the details
- `5b73c454f` fix(concept): a tile's count and its selection mark trade places both ways
- `86c2a776d` fix(concept): Add becomes the first free cell, so the grid keeps its rows
- `700000212` feat(concept): the panel's cards stand level with the canvas
- `5272dcf2f` feat(concept): a wide canvas shows the apps larger, not more of them
- `2e438006e` feat(concept): an app's tile has no plate on hover, its logo grows
- `aeb4d610a` feat(concept): an app's panel reads what it is, open it, then its settings
- `5ad5f2772` feat(concept): Manage apps shows what can be done while nothing is picked
- `2c6cb5159` feat(concept): beside the canvas, an app's options and managing apps are one card
- `d86511339` feat(concept): an open card's head closes it, Close at the icon's top
- `b743ffc6c` feat(concept): Remove app spans the panel, its line centred under it
- `a17d7ddc3` feat(concept): logos fade in once loaded, Explore and Connect wait
- `45fa89b04` feat(concept): surfaces only go up, in words and in a test
- `04ca0dc07` fix(concept): an empty selection mark lies on the logo, not in it
- `1d97302fd` chore(concept): the concept's files in folders by block

## 8 · Search (12)

`features/search`

- `b0bbf6b98` feat(concept): identity menu and Customize, search in Explore and Add app, recovery as one flow
- `b3ab1018f` feat(concept): menus and the app search on the surface ladder, search as a pill
- `1a1dddf69` feat(concept): on a phone everything stands on a 16px edge
- `b9f1a1d64` feat(concept): Explore and Connect keep to the logos' width
- `af61d1aa8` feat(concept): search lives in the header, on every page
- `5da0ddc93` feat(concept): search opens a palette over the page, offering before you type
- `d5d15c5af` fix(concept): a press on the search's blur closes it
- `d52a8dce2` feat(concept): the search no longer features an app before a letter is typed
- `e156a370b` feat(concept): on a large screen the controls and the panel grow
- `26475e4a7` feat(concept): Add app opens the page's one search, not a dropdown
- `45fa89b04` feat(concept): surfaces only go up, in words and in a test
- `1d97302fd` chore(concept): the concept's files in folders by block

## 9 · Explore and Connect (20)

`features/explore`, `features/connect`

- `d683e688d` feat(concept): lay Explore out like the App Store
- `edd7977ec` feat(concept): fill Create with a bento of ways to build on ICP
- `1f0125cfa` feat(concept): draw the Caffeine and Open Cloud banners in their own brands
- `332262977` Revert "feat(concept): draw the Caffeine and Open Cloud banners in their own brands"
- `109c85556` feat(concept): flatten the Explore and Create banners
- `4cd66fe44` feat(concept): lay the Caffeine banner in Create low and wide
- `26b538aed` feat(concept): paint the Caffeine banner a pale sky
- `3a27123eb` feat(concept): give the Caffeine banner Open Cloud's pale blue
- `b262f6f87` feat(concept): give Explore one banner for building an app of your own
- `e1d60c591` feat(concept): turn the Create page into Connect, for AI and CLI access
- `b0bbf6b98` feat(concept): identity menu and Customize, search in Explore and Add app, recovery as one flow
- `a11be749b` feat(concept): Connect is about AI; developers get a quiet line, then their own section
- `1a1dddf69` feat(concept): on a phone everything stands on a 16px edge
- `b9f1a1d64` feat(concept): Explore and Connect keep to the logos' width
- `af61d1aa8` feat(concept): search lives in the header, on every page
- `908dd8358` feat(concept): Connect opens on an intro panel
- `19529261a` feat(concept): Explore and Connect open with their title and a line
- `26475e4a7` feat(concept): Add app opens the page's one search, not a dropdown
- `45fa89b04` feat(concept): surfaces only go up, in words and in a test
- `1d97302fd` chore(concept): the concept's files in folders by block

## The screen (39)

Commits that changed only `+page.svelte` (and the mock data): the layout, the header, the tabs, the pager, the panel, the greeting.

- `18f23e9e5` feat(concept): even out the Recent shelf
- `f587f3a58` feat(concept): manage apps from All apps' heading, not the account
- `614ed1aea` fix(concept): open the identity switcher inside the window
- `250e00cea` feat(concept): close the panel with chevrons, not a cross
- `422826251` feat(concept): rename the "Your apps" tab to "My apps"
- `d79e7881f` feat(concept): raise the Recent shelf as a white card
- `aa1b87cf5` feat(concept): set the selected tab on a thumb
- `cfa5a178a` feat(concept): close the panel with an arrow to the edge
- `61526c765` feat(concept): open the desktop panel on arrival when it has news
- `cf8a698a4` feat(concept): let news leave the dock once the panel has shown it
- `d93f5af32` fix(concept): bleed the incident banner out with the shelf from @lg
- `d1211709b` fix(concept): keep the concept menu on a phone's screen
- `366fa3d05` feat(concept): hide the pages' scrollbar while they're swiped
- `a25528cbf` fix(concept): keep a tapped tab while the pager passes other pages
- `85cef2955` feat(concept): Edit on My apps holds the switch for adding apps on sign-in
- `e9144cd6c` feat(concept): greeting gets more room below and a tighter inset on wide screens
- `7ff129942` feat(concept): keep the identity at the top of the panel inside a section
- `903fc150d` feat(concept): put the account's rows in one untitled group
- `ac340c2e9` feat(concept): give each account row a card of its own
- `767ce4751` feat(concept): the side panel is the main layout and stays open
- `008468bc1` feat(concept): the greeting heads every page; the tabs sit over the apps
- `5caef3d6a` fix(concept): the privacy setting opens its room smoothly above the grid
- `37d90a00b` fix(concept): Edit and Done share one width, so the pill holds still
- `ffae0aba5` fix(concept): the panel's identity line arrives with its section, not over it
- `70fa2d56c` feat(concept): on a phone the tabs are a tab bar at the bottom
- `0aa42e629` fix(concept): the first tab's word stands on the greeting's line
- `b5a32bae8` fix(concept): the greeting and My apps step in past the logo's corner
- `f79c376f2` fix(concept): the greeting, the tabs' thumb and the logos share one edge
- `d1dae4276` feat(concept): on a phone, the greeting and cards head My apps only
- `4a890e66c` feat(concept): the panel's cards get more air between them
- `3597a3efc` feat(concept): the header's tabs are a segmented control
- `e7a611387` feat(concept): the greeting is a step larger and stands apart from the tabs
- `fcd1ffff1` feat(concept): the setting card spans the header row's controls
- `429959893` feat(concept): Explore and Connect start further under the tabs
- `70b799c07` feat(concept): the theme switch stands without its caption
- `4a3822a31` feat(concept): the tabs stand in the header, over the apps
- `321f7be7e` feat(concept): My apps' greeting stands a third of the way down
- `508daff96` feat(concept): what scrolls under the header leaves through a blur
- `87d342d5f` fix(concept): Edit's card opens downward, the greeting and Done stay put

## Concept only (27)

Mock data, the theme switch, the state board, the concept's tooling. Not for the product.

- `b0c2c9232` chore: start UI concept for II holistic redesign
- `ac92bebaf` chore(concept): checkpoint the concept before the swipe depth effect
- `d683e688d` feat(concept): lay Explore out like the App Store
- `edd7977ec` feat(concept): fill Create with a bento of ways to build on ICP
- `1b606d7af` feat(concept): mark an app whose notifications are turned off
- `1f0125cfa` feat(concept): draw the Caffeine and Open Cloud banners in their own brands
- `a8061cb2c` chore(concept): checkpoint the concept before the account panel rework
- `332262977` Revert "feat(concept): draw the Caffeine and Open Cloud banners in their own brands"
- `109c85556` feat(concept): flatten the Explore and Create banners
- `75eb6810d` feat(concept): shuffle the demo's All apps away from Recent's order
- `280d3e619` feat(concept): head the panel with the identity, close it from the canvas
- `26b538aed` feat(concept): paint the Caffeine banner a pale sky
- `3a27123eb` feat(concept): give the Caffeine banner Open Cloud's pale blue
- `69b0c0188` feat(concept): tint the Caffeine banner in Caffeine's sky hue
- `0da92f936` fix(concept): recognise the concept route under a base path
- `740bcbf29` feat(concept): tell a risk as one incident at the head of the panel
- `b262f6f87` feat(concept): give Explore one banner for building an app of your own
- `e1d60c591` feat(concept): turn the Create page into Connect, for AI and CLI access
- `382aa3ad4` fix(concept): apply a chosen theme to app.css once it loads
- `ab6008241` feat(concept): one My apps grid with history, app details on click, confirm removal
- `1e3202fe4` feat(concept): a new identity starts with three featured apps in My apps
- `66a3a838b` feat(concept): the cards speak in Thomas's four styles and his states
- `aad74d7b5` feat(concept): a board with every card state, for Thomas
- `61f93dd94` feat(concept): a card's icons say what they are on hover
- `c669c4cca` feat(concept): an unchecked phrase and a pending email are states again
- `405dbed60` feat(concept): signed in through recovery, by the phrase or the email
- `1d97302fd` chore(concept): the concept's files in folders by block
