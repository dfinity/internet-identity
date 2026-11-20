<script lang="ts" module>
  import type { Component } from "svelte";

  /** One thing a card's message is about: a glyph, or a browser's logo. */
  export interface Glyph {
    id: string;
    icon?: Component;
    src?: string;
    /** What it is, said on hover: "Recovery phrase", "Firefox on Windows". */
    label: string;
    /** Where it stands, under the label: "Not set up", "New sign-in". */
    note?: string;
    /** Not there yet (a way back in not set up, a way in to add): a dashed
     *  outline, as an empty row in the section has it. */
    empty?: boolean;
    /** The item the message is about: it carries the style's dot, the same
     *  cue its row carries once the card is open. */
    marked?: boolean;
  }
</script>

<script lang="ts">
  import type { Tone } from "../status";
  import { surfaceColor, useSurface } from "./Elevated.svelte";

  interface Props {
    glyphs: Glyph[];
    tone: Tone;
  }

  const { glyphs, tone }: Props = $props();

  /** The dots the account's avatar and a section's icon carry already: red
   *  for a risk, amber for a warning, the text's own colour for info. */
  const DOT: Record<Tone, string> = {
    risk: "bg-bg-error-solid",
    warning: "bg-bg-warning-solid",
    info: "bg-fg-primary",
    neutral: "",
  };

  // One step above the card they lie on, as anything raised off a surface
  // is in the ladder (surfaces.css): lighter in dark, the same white in
  // light, where the shadow alone lifts them. The shadow stays a small one
  // whatever the level, for a 28px coin.
  const level = Math.min(useSurface() + 1, 8);

  // A dashed slot has no fill to hide what lies under it, so the one before
  // it is cut where it lies instead: a hole the size of the slot and 2px
  // more around its centre, 8px in (the overlap) and 14px past the edge
  // (its radius), the gap the shadow's ring leaves between two coins. Only
  // a slot's outline is cut, so its icon stays whole; a coin is cut whole.
  // A cut rather than the card's colour: the card's tint on hover would
  // show a filled slot as a disc.
  const UNDER_EMPTY =
    "radial-gradient(circle at calc(100% + 6px) 50%, transparent 15.5px, #000 16px)";
</script>

<!-- Overlapping coins, each a step above the card (StatusCard), with a small
     shadow, so they read as things that are there and that overlap; the
     shadow's ring parts each from the one under it. What isn't there yet
     lies flat instead, a dashed slot in the card itself, so raised means set
     up and dashed means missing; the slots overlap all the same, each over
     the one before it. Decorative: the message beside them says what they
     show, and each one's label (`data-glyph`) shows on hover (StatusCard). -->
<span
  aria-hidden="true"
  style:--ii-glyph-surface={surfaceColor(level)}
  class="flex shrink-0 items-center"
>
  {#each glyphs as glyph, index (glyph.id)}
    {@const Icon = glyph.icon}
    {@const cut = glyphs[index + 1]?.empty === true ? UNDER_EMPTY : undefined}
    <span
      data-glyph={glyph.id}
      style:mask-image={glyph.empty === true ? undefined : cut}
      class={[
        "relative grid size-7 place-items-center rounded-full",
        glyph.empty === true
          ? "text-fg-quaternary"
          : "text-fg-tertiary bg-(--ii-glyph-surface) shadow-(--ii-shadow-3)",
        index > 0 && "-ms-2",
      ]}
    >
      {#if glyph.empty === true}
        <span
          style:mask-image={cut}
          class="border-border-secondary absolute inset-0 rounded-full border border-dashed"
        ></span>
      {/if}
      {#if glyph.src !== undefined}
        <img src={glyph.src} alt="" class="size-4" />
      {:else if Icon !== undefined}
        <Icon class="size-3.5" />
      {/if}
      {#if glyph.marked === true && tone !== "neutral"}
        <span
          class={[
            "absolute -end-0.5 -top-0.5 size-2 rounded-full ring-2 ring-(color:--ii-glyph-surface)",
            DOT[tone],
          ]}
        ></span>
      {/if}
    </span>
  {/each}
</span>
