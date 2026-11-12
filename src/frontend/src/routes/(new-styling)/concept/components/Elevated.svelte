<script lang="ts" module>
  import { getContext, setContext } from "svelte";

  const SURFACE = Symbol("surface");

  /** The level of the surface this component sits on; the page is 1. */
  export const useSurface = (): number => getContext<number>(SURFACE) ?? 1;

  /** Sets the level for everything rendered inside, e.g. a sheet's content. */
  export const provideSurface = (level: number): void => {
    setContext(SURFACE, level);
  };
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
