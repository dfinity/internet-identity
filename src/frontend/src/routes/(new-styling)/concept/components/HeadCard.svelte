<script lang="ts">
  import type { Component } from "svelte";
  import { ChevronRightIcon } from "@lucide/svelte";
  import type { CardStatus } from "../status";
  import Elevated from "./Elevated.svelte";

  interface Props {
    card: CardStatus;
    title: string;
    icon: Component;
    onOpen?: () => void;
  }

  const { card, title, icon: Icon, onOpen }: Props = $props();
</script>

<!-- Phone: one of the head's three cards, side by side, so how you get in,
     how you'd get back in and where you're signed in are there as the page
     opens. Its one message, in its style: the tinted icon for a warning or
     a risk, red words for a risk, quiet words while it's neutral. On a
     narrow phone (a canvas under 384px, as an iPhone SE's) a third of it is
     too tight for a sentence, so the three stand as rows instead (Thomas):
     the icon, the title, the message beside it and a chevron. -->
<Elevated offset={1} class="overflow-hidden rounded-2xl">
  <button
    type="button"
    onclick={onOpen}
    aria-label={`${title}: ${card.sentence}`}
    class="ii-hover focus-visible:ring-focus-ring flex h-full w-full flex-col items-start gap-2 p-2.5 text-start outline-none focus-visible:ring-2 focus-visible:ring-inset @max-[24rem]/canvas:flex-row @max-[24rem]/canvas:items-center @max-[24rem]/canvas:gap-3"
  >
    <span
      aria-hidden="true"
      class={[
        "grid size-7 shrink-0 place-items-center rounded-lg",
        {
          risk: "text-fg-error-primary bg-(--ii-error-tint)",
          warning: "text-fg-warning-primary bg-(--ii-warning-tint)",
        }[card.tone as "risk" | "warning"] ??
          "text-fg-secondary bg-(--ii-selected)",
      ]}
    >
      <Icon class="size-4" />
    </span>
    <span
      class="flex flex-col gap-0.5 @max-[24rem]/canvas:min-w-0 @max-[24rem]/canvas:flex-1 @max-[24rem]/canvas:flex-row @max-[24rem]/canvas:items-baseline @max-[24rem]/canvas:gap-2"
    >
      <span
        class="text-text-primary text-[13px] leading-4 font-semibold @max-[24rem]/canvas:w-16 @max-[24rem]/canvas:shrink-0"
      >
        {title}
      </span>
      <span
        class={[
          "line-clamp-3 text-xs leading-4 text-pretty @max-[24rem]/canvas:line-clamp-2 @max-[24rem]/canvas:min-w-0 @max-[24rem]/canvas:flex-1",
          card.tone === "risk"
            ? "text-text-error-primary"
            : card.tone === "neutral"
              ? "text-text-tertiary"
              : "text-text-primary",
        ]}
      >
        {card.sentence}
      </span>
    </span>
    <ChevronRightIcon
      aria-hidden="true"
      class="text-fg-quaternary hidden size-4 shrink-0 @max-[24rem]/canvas:block rtl:-scale-x-100"
    />
  </button>
</Elevated>
