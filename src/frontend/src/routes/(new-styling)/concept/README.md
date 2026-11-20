# Internet Identity dashboard: the concept

A redesign of the dashboard on mock data, for review and then to build in
the product. This page is the branch's description: what is here, in which
order to take it, what each part needs from the product, and what is the
concept's own.

## See it

```sh
npm run dev:concept
```

Then open http://127.0.0.1:5174/concept. The flask in a corner opens the
concept menu: the layout, a scenario, how fast the data arrives (at once,
in under a second, slowly, or the apps failing), and a new visit.
`?load=slow`, `?layout=island` and `?board=states` (every card state on one
board) work in the address too.

## The blocks

Take them in this order: each needs only the ones above it.

| #   | Block               | Where                                  | What it holds                                                                                                                                                                                                            | What it needs from the product                                                                                                                                                                                 |
| --- | ------------------- | -------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | Shared product code | `$lib`, `manage/…/AppLogo.svelte`      | The identity's avatar (`IdentityAvatar`: an initial on colours of its own, from `identityColors.ts`), the sign-in badge on it (`AuthMethodBadge`, frosted), an app's logo on a tile (`AppLogo`: `size="tile"`, `reveal`) | Nothing: review first, these already change the product's screens                                                                                                                                              |
| 2   | Surfaces            | `foundation/surfaces`                  | The surface ladder, `Elevated`, `SurfaceLevel`; the rule in [SURFACES.md](SURFACES.md), checked by `surfaces.test.ts`                                                                                                    | Nothing: tokens and components for `$lib`                                                                                                                                                                      |
| 3   | Motion and loading  | `foundation/motion`                    | `motion.ts` (every transition), `Load`, `Swap` and `loading.css` (skeletons, see below), the progressive blur                                                                                                            | Nothing                                                                                                                                                                                                        |
| 4   | Frame               | `foundation/frame`, `+page.svelte`     | The canvas, the header and its tabs, the pager, the panel beside the canvas (a phone's head and sheet), the sheet, popups, menus; the island and the dock of the two layouts compared                                    | The identity's name, from the device                                                                                                                                                                           |
| 5   | Identity            | `features/identity`                    | The identity's card, the list to switch, the phone's menu, the avatar stack, the colour strip                                                                                                                            | The identities on the device (`last-used-identities`, there today); where a chosen colour is kept is open                                                                                                      |
| 6   | Security            | `features/security`                    | Access, Recovery and Devices as cards and their sections, the phone's head cards, recovery's flow, an incident; `status.ts` works out each card's state and message (with its test)                                      | The identity's sign-in methods with when each was last used, its recovery (phrase, email, verified or not), and where it is signed in                                                                          |
| 7   | My apps             | `features/apps`                        | The grid of tiles, arranging it, an app's options, several at once, the hints                                                                                                                                            | The apps the identity signs in to (the applications page lists them today), with an order of the user's, unread notifications, notifications on or off, and whether to keep the history: the last four are new |
| 8   | Search              | `features/search`                      | Search over the header, ⌘K, adding an app by link                                                                                                                                                                        | The catalogue and the identity's apps                                                                                                                                                                          |
| 9   | Explore and Connect | `features/explore`, `features/connect` | Explore's feed (banners, shelves, building your own); Connect: AI and command-line access to the apps                                                                                                                    | The catalogue; Connect's integrations are new                                                                                                                                                                  |
|     | Concept only        | `mock`                                 | Mock data, the theme switch, the state board; and in `+page.svelte`, the concept menu                                                                                                                                    | Not for the product                                                                                                                                                                                            |

[COMMITS.md](COMMITS.md) lists the branch's commits under each block.
`+page.svelte` still holds the screen's assembly and much of the frame's
behaviour (the layout, the panel's states, the pager), about 2,900 lines:
split it when the frame moves.

## Loading

The concept loads the way the product will, after Fluid Functionalism's
[Reveal](https://www.fluidfunctionalism.com/docs/reveal):

- What the page knows at once is there from the first frame: the frame,
  the header, the greeting with the identity's name, each card's icon and
  title. Its blocks fade in 60ms apart (`arrive` in `motion.ts`).
- What waits for data stands in its place as a skeleton as large as it
  (`Swap`), in the same grid cell as the block that replaces it, so nothing
  moves when one gives way to the other. A skeleton shows only once the
  wait passes 160ms; a quick answer never flashes one.
- Nothing worked out from the data shows before it lands: a card's tone,
  a dot, an incident, the island, the dock, Edit.

Each `Load` in `+page.svelte` is a request the product will make:

| Request     | Shown in                                                                                       | While it loads                                              | If it fails                             |
| ----------- | ---------------------------------------------------------------------------------------------- | ----------------------------------------------------------- | --------------------------------------- |
| The account | The panel's cards, the phone's head cards, the header's dot, an incident, the island, the dock | Each card's icon and title, a skeleton line for its message | Not designed yet                        |
| The apps    | The grid, Edit, Explore, Connect                                                               | A skeleton tile for each app this device last saw           | "Your apps didn't load", with Try again |

A tile's logo shows once it has loaded and is measured, fading in over
200ms (`AppLogo`'s `reveal`).

## Not done here

For the product to decide, or to build there:

- **A first frame before the JavaScript.** The concept decides phone or
  desktop in script (`MediaQuery`), so its HTML can't be rendered ahead:
  until the script arrives the page is blank (about 4s at 1.6 Mbit/s, the
  script being 555KB). In the product, lay the phone and the desktop out by
  CSS and prerender the frame, as `/` and `about` already are; and load
  what isn't on the first screen (recovery's flow, search, Explore,
  Connect) only when it's opened.
- **Switching identity** shows the other identity's data at once; in the
  product it's fetched, with the same skeletons as on load.
- **A section opened while the account loads** shows its rows; in the
  product they'd wait, as skeleton rows.
- **Explore and Connect** show only their title until the apps land.
- **The account failing to load** has no design yet.
- **A layout shift** of 0.045 as the page first mounts, in the pager,
  from before the loading work.

## Working on it

[AGENTS.md](AGENTS.md) is what an agent (or anyone) should know before
changing the concept: surfaces, loading, motion, what is mock.
