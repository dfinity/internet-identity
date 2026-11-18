<script lang="ts">
  import type { Component } from "svelte";
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
     a risk, red words for a risk, quiet words while it's neutral. -->
<Elevated offset={1} class="overflow-hidden rounded-2xl">
  <button
    type="button"
    onclick={onOpen}
    aria-label={`${title}: ${card.sentence}`}
    class="ii-hover focus-visible:ring-focus-ring flex h-full w-full flex-col items-start gap-2 p-2.5 text-start outline-none focus-visible:ring-2 focus-visible:ring-inset"
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
    <span class="flex flex-col gap-0.5">
      <span class="text-text-primary text-[13px] leading-4 font-semibold">
        {title}
      </span>
      <span
        class={[
          "line-clamp-3 text-xs leading-4 text-pretty",
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
  </button>
</Elevated>
