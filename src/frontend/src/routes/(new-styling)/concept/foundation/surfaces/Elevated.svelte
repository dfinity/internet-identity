<script lang="ts" module>
  import { getContext, setContext } from "svelte";

  const SURFACE = Symbol("surface");

  /** The level of the surface this component sits on; the page is 1. */
  export const useSurface = (): number => getContext<number>(SURFACE) ?? 1;

  /** Sets the level for everything rendered inside, e.g. a sheet's content. */
  export const provideSurface = (level: number): void => {
    setContext(SURFACE, level);
  };

  // The steps that don't depend on what lies under them (SURFACES.md); in
  // CSS they're --ii-page, --ii-canvas, --ii-floating and --ii-top.
  /** The window, under the canvas and beside it on desktop. */
  export const PAGE = 1;
  /** The canvas: the apps' frame on desktop, the whole screen on a phone. */
  export const CANVAS = 3;
  /** What floats over the page: the header's pills once content runs under
   *  them, the tab bar, the island, the dock, search, a sheet. */
  export const FLOATING = 6;
  /** The top: a thumb raised off its track, a mark over a logo. */
  export const TOP = 8;

  /** A level's colour, for a style, a ring or a fill that has to match it;
   *  `surfaceColor(useSurface() + 1)` is one step up from here. */
  export const surfaceColor = (level: number): string =>
    `var(--ii-surface-${Math.min(Math.max(Math.round(level), 1), 8)})`;
</script>

<script lang="ts">
  import type { Snippet } from "svelte";
  import type { ClassValue, HTMLAttributes } from "svelte/elements";

  type Props = Omit<HTMLAttributes<HTMLElement>, "class"> & {
    /** Steps above the surface underneath: 1 for a group, 2 for a card, 5 for
     *  something floating over the page. */
    offset: number;
    /** Keeps a constant shadow weight wherever the surface is nested. */
    shadowLevel?: number;
    as?: "div" | "section" | "ul" | "aside" | "header" | "main";
    class?: ClassValue;
    children?: Snippet;
  };

  const {
    offset,
    shadowLevel,
    as = "div",
    class: className,
    children,
    ...rest
  }: Props = $props();

  // Levels are fixed for a component's life, like the place it renders in.
  // svelte-ignore state_referenced_locally
  const level = Math.min(useSurface() + offset, 8);
  provideSurface(level);
</script>

<svelte:element
  this={as}
  class={[
    `ii-surface-${level}`,
    `ii-elevation-${Math.min(shadowLevel ?? level, 8)}`,
    className,
  ]}
  {...rest}
>
  {@render children?.()}
</svelte:element>
