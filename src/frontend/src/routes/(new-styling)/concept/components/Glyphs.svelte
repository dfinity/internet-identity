<script lang="ts" module>
  import type { Component } from "svelte";

  /** One thing a card's message is about: a glyph, or a browser's logo. */
  export interface Glyph {
    id: string;
    icon?: Component;
    src?: string;
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
</script>

<!-- Overlapping circles, each cut out of the card it stands on
     (--ii-card-surface, StatusCard's own surface); bg-secondary keeps them
     apart from it in light, where a raised card is white. Decorative: the
     message beside them says what they show. -->
<span aria-hidden="true" class="flex shrink-0 items-center">
  {#each glyphs as glyph, index (glyph.id)}
    {@const Icon = glyph.icon}
    <span
      class={[
        "relative grid size-7 place-items-center rounded-full ring-2 ring-(color:--ii-card-surface)",
        glyph.empty === true
          ? "border-border-secondary text-fg-quaternary border border-dashed bg-(--ii-card-surface)"
          : "bg-bg-secondary text-fg-tertiary",
        index > 0 && "-ms-2",
      ]}
    >
      {#if glyph.src !== undefined}
        <img src={glyph.src} alt="" class="size-4" />
      {:else if Icon !== undefined}
        <Icon class="size-3.5" />
      {/if}
      {#if glyph.marked === true && tone !== "neutral"}
        <span
          class={[
            "absolute -end-0.5 -top-0.5 size-2 rounded-full ring-2 ring-(color:--ii-card-surface)",
            DOT[tone],
          ]}
        ></span>
      {/if}
    </span>
  {/each}
</span>
