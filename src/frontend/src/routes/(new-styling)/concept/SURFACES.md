# Surfaces

How the concept lays one thing on another. After
[Fluid Functionalism's Surfaces](https://www.fluidfunctionalism.com/docs/surfaces)
(MIT); the values are in
[`foundation/surfaces/surfaces.css`](foundation/surfaces/surfaces.css).

## The rule

**A surface only ever goes up from what it lies on.**

1. **A step, not a number.** A surface says how far above its place it
   stands: `<Elevated offset={1}>` for a card or a group, a popover two
   steps up. It reads the level it lies on from context and passes its own
   down, so whatever is nested keeps climbing.
2. **Up only.** Nothing nested is lower than what holds it. In dark every
   step is lighter, so a darker fill on a card reads as a hole cut into it,
   or a dent, never as a thing laid on it. In light the surfaces are white
   from level 3 and the shadow carries the height.
3. **Overlays don't change the level.** Hover, selected, a skeleton and a
   tone's tint are laid over whatever level they're on: `--ii-hover`,
   `--ii-selected`, `--ii-warning-tint`, `--ii-error-tint`, `.skeleton`.
4. **One named dent.** To group things without raising them, there is the
   shelf (`.ii-shelf`, `--ii-shelf`): the canvas, tinted. What sits on a
   shelf stands above it.
5. **Exceptions are written down.** Anything outside the ladder is listed
   with its reason in [`surfaces.test.ts`](surfaces.test.ts).

## The ladder

Eight levels, each a background and a shadow (`ii-surface-N`,
`ii-elevation-N`). In dark each is lighter than the last: `#171717` up to
`#484848`. In light they flatten to white from level 3 and only the shadow
grows.

Four steps don't depend on what lies under them, and have names:

| Name                         | Level | What stands there                                                                                                              |
| ---------------------------- | ----- | ------------------------------------------------------------------------------------------------------------------------------ |
| `--ii-page` · `PAGE`         | 1     | The window: under the canvas, and beside it on desktop                                                                         |
| `--ii-canvas` · `CANVAS`     | 3     | The canvas: the apps' frame on desktop, the whole screen on a phone                                                            |
| `--ii-floating` · `FLOATING` | 6     | What floats over the page: the header's pills once content runs under them, the tab bar, the island, the dock, search, a sheet |
| `--ii-top` · `TOP`           | 8     | The top: a thumb raised off its track, a mark over a logo                                                                      |

The CSS names are for classes (`bg-(--ii-floating)`); the constants, from
[`Elevated.svelte`](foundation/surfaces/Elevated.svelte), for code.

## How to

| You want                                                                     | Write                                                                            |
| ---------------------------------------------------------------------------- | -------------------------------------------------------------------------------- |
| A card or a group on whatever it lies on                                     | `<Elevated offset={1}>`                                                          |
| A popover or a menu                                                          | `<Popup>` (two steps up from where it opens)                                     |
| Content that counts as standing on a level it doesn't paint                  | `<SurfaceLevel level={…}>`, or `provideSurface(level)` in a component            |
| A colour one step up, where an `Elevated` can't go (a style, a ring, a fill) | `surfaceColor(useSurface() + 1)`                                                 |
| Something floating                                                           | `bg-(--ii-floating) ii-elevation-6`                                              |
| A ring that cuts a badge out of its surface                                  | the surface's own colour: `ring-(color:--ii-tile-backdrop)`, `--ii-dot-backdrop` |
| Hover, selected, a tint                                                      | the overlays above, never a darker fill                                          |

Never: the product's page backgrounds (`bg-bg-primary`, `bg-bg-secondary`
and their kin sit below the ladder in dark), a fixed white, black or grey,
or a level by number (`ii-surface-4`). The test names the line and the
reason if one slips in.

## Checked

```sh
npx vitest run "src/frontend/src/routes/(new-styling)/concept/surfaces.test.ts"
```

It checks that the ladder itself only gets lighter in dark and never
darker in light, and that no component paints a background outside it.

## Into the product

The ladder becomes the product's tokens: the eight levels and their
shadows next to the theme's colours in `app.css`, `Elevated`,
`SurfaceLevel` and the surface context in `$lib`. Then the product's own
screens move off `bg-bg-primary` and its kin the same way the concept
did, and the test's check runs over `$lib` and the routes.
