# The landing playground (`/concept/landing`)

Internet Identity's first page, where you pick an identity or make one,
explored in six layouts on the concept's surfaces, type and components. A
design exploration on mock data: nothing here signs anyone in, and nothing
in the product's flows links to it.

```sh
npm run dev:concept
```

Then open http://127.0.0.1:5174/concept/landing. It opens on Profiles,
the layout we're taking forward, with two identities on the device and the
controls folded into a flask in its footer. Every choice is kept in the
address, so a link opens the same combination.

## Controls

The bar at the top; each control has a key.

| Control                  | Key   | What it does                                                                                                         |
| ------------------------ | ----- | -------------------------------------------------------------------------------------------------------------------- |
| Layout                   | ← →   | Six layouts, below                                                                                                   |
| Identities               | 1–5   | None (sign in or create), One, Two (the default: a passkey and SSO), Three, Many (7, to see where it breaks)         |
| Last used                | L     | The mark on the identity used last (shown where there's more than one)                                               |
| Texture                  | T     | None, the product's own dots (FlairCanvas), or one of six textures                                                   |
| Brand (Split, Immersive) | B, C  | What the brand side says (statement, quote, principles), and whether it takes the colours of the identity pointed at |
| View                     | W P O | This window, on a phone (a 390px frame), all layouts at once                                                         |
| Theme                    | D     | Light or dark                                                                                                        |
| Hide the bar             | H     | The bar starts folded into a flask, in Profiles' footer (elsewhere in the header); the flask (or H) opens it         |

## Layouts

| #   | Layout       | The idea                                                                                                     |
| --- | ------------ | ------------------------------------------------------------------------------------------------------------ |
| 1   | Profiles     | Identities as large tiles in the dashboard's frame, like My apps; Edit puts a remove mark on each            |
| 2   | Centered     | One card in the middle, as most sign-in pages choose an account                                              |
| 3   | Split        | Choosing on the left; a framed brand panel on the right with a texture and what Internet Identity stands for |
| 4   | Welcome back | The identity used last is the page; the others wait in the identity card's stack and open into its list      |
| 5   | Sentence     | "Continue as Thomas, Work or Testing.": the identities are the sentence's words                              |
| 6   | Immersive    | The texture is the page, the statement stands on it, and the identities float in a dock at the bottom        |

In every layout: a click on an identity shows its spinner and then a toast
(here the dashboard would open); Add identity opens the ways in; Edit
identities removes one from this device, with Undo. The ways in are
today's landing page's: a passkey, Google, Apple, Microsoft or SSO; an
identity from another device, by a link or QR code; recovery; and Create
new identity, which turns the same buttons to creating one.

Profiles puts each of today's actions in its own place: About at the top
right, Edit identities under the tiles, and the footer's language, Support
and Source code. On a phone its circles shrink so three fit a row.

## What it reuses, what is new

From the product and the concept, unchanged: the surface ladder
(`foundation/surfaces`, so `surfaces.test.ts` covers these files too),
`motion.ts` (`arrive`, `travel`), the theme switch, `Popup`,
`IdentityStack` and `IdentityList`, `IdentityAvatar` and its colours,
`Tooltip` (on the badge of how an identity signs in),
`AuthMethodBadge`, `Logo`, `Footer`, `Dialog`, `ProgressRing`, the
`btn` classes and the concept's identities (Thomas, Work, Testing).

New here, for review:

- **`parts/textures.ts`**: magnet, watch, twist, blinds, cells and swell,
  after [textures.dev](https://textures.dev)'s free set (MIT), redrawn on a
  canvas from their descriptions, so no dependency was added. Without a
  hover (a phone) they drift along a slow figure; under reduced motion they
  hold still.
- **Identity colour on the brand side**: the panel (Split) or the page
  (Immersive) takes the colours of the identity pointed at, the pair its
  avatar already wears; textures in Profiles, Welcome back and Sentence take
  its ground colour.
- **`parts/BigAvatar.svelte`**: the product's avatar at the sizes tiles and
  the hero need (the product's comes at 32 and 48px).
- **`parts/SignInMethods.svelte`**: the product's sign-in picker
  (`PickAuthenticationMethod`, signing in or creating) on the concept's
  surfaces, with the providers' marks drawn here (the product takes them
  from the canister).
- **`parts/LandingFooter.svelte`**: the product's `Footer` for a layout
  with About in its header, and the playground's flask at its start.
- **The copy** in `parts/Brand.svelte` is a draft to judge the layouts by.
  Each claim holds for Internet Identity today; the quote is Article 12 of
  the Universal Declaration of Human Rights.

`mock.ts` and `playground.svelte.ts` are the playground's own and don't go
to the product.
