<script lang="ts">
  import type { Snippet } from "svelte";
  import {
    BotIcon,
    BriefcaseMedicalIcon,
    CheckIcon,
    ChevronRightIcon,
    Grid2x2CheckIcon,
    KeyRoundIcon,
    MonitorSmartphoneIcon,
  } from "@lucide/svelte";
  import { prefersReducedMotion } from "svelte/motion";
  import type { CardKind, Tone } from "../status";
  import { swapIn, swapOut } from "../motion";
  import { useSurface } from "./Elevated.svelte";

  interface Props {
    kind: CardKind;
    title: string;
    sentence: string;
    tone: Tone;
    /** Just resolved: the icon turns into a check for a moment. */
    resolved?: boolean;
    onOpen: () => void;
    /** What's inside (the methods, the two ways back in, the browsers), so
     *  the card says more than its section's name. */
    preview?: Snippet;
  }

  const {
    kind,
    title,
    sentence,
    tone,
    resolved = false,
    onOpen,
    preview,
  }: Props = $props();

  // Same glyphs as the current sidebar, so each section keeps its icon, but
  // for apps: a bare grid said nothing of managing, so it carries the check
  // an app gets when picked, as the tile's "Select" does.
  const Icon = $derived(
    {
      access: KeyRoundIcon,
      devices: MonitorSmartphoneIcon,
      recovery: BriefcaseMedicalIcon,
      apps: Grid2x2CheckIcon,
      integrations: BotIcon,
    }[kind],
  );

  // The card's own surface, which it sits in (an Elevated), so the preview's
  // overlapping circles cut themselves out of it at whatever level it is.
  const cardSurface = `var(--ii-surface-${useSurface()})`;
</script>

<!-- One large target per section: the whole card leads to its page, where
     every step for it lives, Verify included. The tinted icon and the
     sentence carry the tone; nothing inside the card is a button of its own,
     so it's never unclear how to reach the rest of the section. -->
<button
  type="button"
  onclick={onOpen}
  aria-label={`${title}: ${sentence}`}
  style:--ii-card-surface={cardSurface}
  class="ii-hover focus-visible:ring-focus-ring flex w-full flex-col gap-2.5 rounded-[inherit] px-4 pt-3.5 pb-4 text-start outline-none focus-visible:ring-2 focus-visible:ring-inset"
>
  <span class="flex w-full items-center gap-3">
    <span
      aria-hidden="true"
      class={[
        "grid size-9 shrink-0 place-items-center rounded-[0.625rem] transition-colors duration-200 *:col-start-1 *:row-start-1",
        {
          risk: "text-fg-error-primary bg-(--ii-error-tint)",
          action: "text-fg-warning-primary bg-(--ii-warning-tint)",
        }[tone as "risk" | "action"] ?? "text-fg-secondary bg-(--ii-selected)",
      ]}
    >
      {#if resolved}
        <span
          class="text-fg-success-primary"
          in:swapIn={{ reduced: prefersReducedMotion.current }}
          out:swapOut={{ reduced: prefersReducedMotion.current }}
        >
          <CheckIcon class="size-4.5" strokeWidth={2.5} />
        </span>
      {:else}
        <span
          in:swapIn={{ reduced: prefersReducedMotion.current }}
          out:swapOut={{ reduced: prefersReducedMotion.current }}
        >
          <Icon class="size-4.5" />
        </span>
      {/if}
    </span>
    <span class="text-text-primary min-w-0 flex-1 text-sm font-semibold">
      {title}
    </span>
    <ChevronRightIcon
      aria-hidden="true"
      class="text-fg-quaternary size-4 shrink-0 rtl:-scale-x-100"
    />
  </span>
  <!-- One size and weight for every sentence: the tone shows in its color
       and the tinted icon, not in heavier text. -->
  <span
    class={[
      "flex min-w-0 items-center gap-1.5 text-[13px] leading-[18px] text-pretty",
      {
        risk: "text-text-error-primary",
        action: "text-text-primary",
        queued: "text-text-primary",
        now: "text-text-primary",
        recent: "text-text-primary",
        pending: "text-text-secondary",
        tip: "text-text-tertiary",
        calm: "text-text-tertiary",
      }[tone],
    ]}
  >
    {#if tone === "now" || tone === "recent"}
      <span
        aria-hidden="true"
        class="bg-fg-primary size-1.5 shrink-0 rounded-full"
      ></span>
    {/if}
    <span class="min-w-0">{sentence}</span>
  </span>
  {#if preview !== undefined}
    <span aria-hidden="true" class="flex min-w-0 flex-wrap items-center gap-2">
      {@render preview()}
    </span>
  {/if}
</button>
