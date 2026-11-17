<!--
  A menu or popover in the concept: the product's Popover places it, and the
  surface ladder paints it, the way Fluid Functionalism's dropdown is painted
  (https://www.fluidfunctionalism.com, MIT). It stands two steps above the
  surface it opens over, so it never melts into a card or a sheet, and keeps
  one shadow weight wherever it opens. The shadow's ring is its edge, in
  place of the product popover's border, and in dark mode a lighter step
  does what the shadow can't.

  Rows inside sit on a 4px inset with 8px corners, inside the popup's 12px.
-->
<script lang="ts">
  import type { ComponentProps } from "svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { provideSurface, useSurface } from "./Elevated.svelte";

  type Props = ComponentProps<typeof Popover> & {
    /** The level it opens over, for a popup rendered outside that surface,
     *  e.g. at the page's root but over the canvas. */
    over?: number;
  };

  const { over, class: className, style, children, ...rest }: Props = $props();

  // The level is fixed for the popup's life, like where it opens.
  // svelte-ignore state_referenced_locally
  const level = Math.min((over ?? useSurface()) + 2, 8);
  provideSurface(level);
</script>

<!-- The level travels as a variable on the popover itself, which its card
     inherits. Below 480px a responsive Popover is a sheet instead, which gets
     the class but not the variable, so the background falls back to a level
     of its own there. -->
<Popover
  {...rest}
  style={`--ii-popup-surface: var(--ii-surface-${level});${style ?? ""}`}
  class={[
    "!border-transparent !bg-[var(--ii-popup-surface,var(--ii-surface-3))] !shadow-(--ii-shadow-3)",
    className,
  ]}
>
  {@render children?.()}
</Popover>
