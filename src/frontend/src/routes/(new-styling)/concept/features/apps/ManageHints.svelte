<!--
  What managing apps lets you do, while nothing is picked yet: a sketch of the
  grid in its edit mode, then a row for each thing to try. One step above the
  panel, as its other cards are. The sketch is drawn from the grid's own
  parts at a smaller size: a tile with its empty mark, a picked one on its
  plate, and one lifted off its cell the way a drag carries it.
-->
<script lang="ts">
  import {
    CheckIcon,
    CopyCheckIcon,
    HandGrabIcon,
    MousePointerClickIcon,
  } from "@lucide/svelte";
  import type { Component } from "svelte";
  import Elevated from "../../foundation/surfaces/Elevated.svelte";

  interface Props {
    /** `card`: a layer inside the Manage apps card, 8px in from its edges,
     *  its rows stepping in to keep their icons on the card's 16px line.
     *  `page`: a card of its own, as in a phone's sheet. */
    variant?: "page" | "card";
  }

  const { variant = "page" }: Props = $props();

  const HINTS: { icon: Component; title: string; detail: string }[] = [
    {
      icon: MousePointerClickIcon,
      title: "Select an app",
      detail: "Turn its notifications off or forget it.",
    },
    {
      icon: CopyCheckIcon,
      title: "Select several",
      detail: "Change them all at once.",
    },
    {
      icon: HandGrabIcon,
      title: "Drag to rearrange",
      detail: "Pick an app up and drop it where you want it.",
    },
  ];

  // A tile's logo, three quarters of the grid's: the same rounding to size.
  // On the shelf, at the floating step, which stands clear of the shelf (the
  // canvas tinted) in both themes.
  const TILE = "size-12 rounded-xl";
  const LOGO = `${TILE} bg-(--ii-floating) shadow-(--ii-shadow-2)`;
  // Where a tile's name would be.
  const NAME = "h-1.5 w-8 rounded-full bg-(--ii-selected)";
  // The grid's selection mark, its place over the logo's corner kept.
  const MARK =
    "absolute -start-1 -top-1 flex size-4 items-center justify-center rounded-full border-[1.5px]";
  // A tile's cell, with or without the plate a picked one sits on.
  const CELL = "flex flex-col items-center gap-2 rounded-xl px-2 pt-2.5 pb-2";
</script>

<Elevated
  offset={1}
  shadowLevel={variant === "card" ? 3 : undefined}
  as="section"
  class={[
    "overflow-hidden",
    variant === "card" ? "mx-2 mt-4 mb-2 rounded-lg" : "rounded-2xl",
  ]}
>
  <h3 class="sr-only">How to manage apps</h3>

  <div
    aria-hidden="true"
    class="ii-shelf flex items-center justify-center gap-4 pt-7 pb-6"
  >
    <span class={CELL}>
      <span class="relative">
        <span class={["block", LOGO]}></span>
        <span class={[MARK, "border-border-primary bg-(--ii-top)"]}></span>
      </span>
      <span class={NAME}></span>
    </span>

    <span class={[CELL, "bg-(--ii-selected)"]}>
      <span class="relative">
        <span class={["block", LOGO]}></span>
        <span
          class={[
            MARK,
            "border-bg-brand-solid bg-bg-brand-solid text-text-primary-inversed",
          ]}
        >
          <CheckIcon class="size-2.5" strokeWidth={3.5} />
        </span>
      </span>
      <span class={NAME}></span>
    </span>

    <!-- The free cell a drag shows, and the tile carried toward it: larger,
         as a lifted tile is, with a stronger shadow and the hand on it. -->
    <span class={CELL}>
      <span class="relative">
        <span
          class={[
            "border-border-primary block border-[1.5px] border-dashed",
            TILE,
          ]}
        ></span>
        <span
          class="absolute inset-0 -translate-x-4 -translate-y-3.5 scale-108 rtl:translate-x-4"
        >
          <span
            class={["block bg-(--ii-floating) shadow-(--ii-shadow-5)", TILE]}
          ></span>
          <HandGrabIcon
            class="text-fg-primary absolute -end-2.5 -bottom-2.5 size-6 fill-(--ii-top)"
            strokeWidth={1.75}
          />
        </span>
      </span>
      <span class={[NAME, "invisible"]}></span>
    </span>
  </div>

  <ul
    class={[
      "ii-rows",
      variant === "card"
        ? "[--ii-rows-inset:3.5rem]"
        : "[--ii-rows-inset:4rem]",
    ]}
  >
    {#each HINTS as hint (hint.title)}
      <li
        class={[
          "flex items-center gap-3 py-3.5",
          variant === "card" ? "px-2" : "px-4",
        ]}
      >
        <span
          aria-hidden="true"
          class="text-fg-secondary flex size-9 shrink-0 items-center justify-center rounded-lg bg-(--ii-selected)"
        >
          <hint.icon class="size-4.5" />
        </span>
        <span class="flex min-w-0 flex-col gap-0.5">
          <span class="text-text-primary text-sm font-medium">
            {hint.title}
          </span>
          <span class="text-text-tertiary text-[13px] leading-[18px]">
            {hint.detail}
          </span>
        </span>
      </li>
    {/each}
  </ul>
</Elevated>
