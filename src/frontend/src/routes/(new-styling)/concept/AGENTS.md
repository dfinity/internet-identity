# The II concept (`/concept`)

A redesign of Internet Identity's dashboard, on mock data, for review. Run
it with `npm run dev:concept` and open http://127.0.0.1:5174/concept; the
flask in a corner opens the concept menu (layout, scenario, how fast data
arrives, a new visit).

Before you change anything here:

1. **Surfaces only go up.** Read [SURFACES.md](SURFACES.md). A surface is
   an `Elevated` one step above what it lies on, or a named step
   (`--ii-canvas`, `--ii-floating`, `--ii-top`); hover, selected and tints
   are overlays. Never `bg-bg-primary` and its kin, a fixed white, black or
   grey, or a level by number. `surfaces.test.ts` fails on any of them.
2. **Data arrives after the page.** What shows the identity's data waits
   for its request (`Load` in `loading.svelte.ts`): a skeleton as large as
   the block stands in its place (`Swap`, `loading.css`), and nothing
   worked out from the data (a tone, a dot, a count) shows before it
   lands. What the page knows at once (the frame, titles, the identity's
   name) is there from the first frame.
3. **Motion comes from `motion.ts`.** Ease out, exits quicker than
   entrances, no bounce on a click, and under reduced motion only opacity.
   Blocks arriving on load use `arrive`, never a CSS animation that would
   replay as the DOM changes.
4. **Mock is mock.** `mockData.ts`, the load speeds in
   `loading.svelte.ts`, `StateBoard` and the concept menu are the concept's
   own and don't go to the product.

Commits: `feat(concept): …` or `fix(concept): …`, one change each, so any
can be reverted on its own.
