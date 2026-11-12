<script lang="ts">
  import {
    BotIcon,
    BriefcaseMedicalIcon,
    CheckIcon,
    ChevronRightIcon,
    KeyRoundIcon,
    LayoutGridIcon,
    MonitorSmartphoneIcon,
  } from "@lucide/svelte";
  import { slide } from "svelte/transition";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import type { CardKind, LeadAction, Tone } from "../status";
  import { swapIn, swapOut } from "../motion";

  interface Props {
    kind: CardKind;
    title: string;
    sentence: string;
    tone: Tone;
    /** Only the loud row offers its step inline. */
    action?: LeadAction;
    /** Just resolved: the icon turns into a check for a moment. */
    resolved?: boolean;
    onOpen: () => void;
    onAction?: () => void;
  }

  const {
    kind,
    title,
    sentence,
    tone,
    action,
    resolved = false,
    onOpen,
    onAction,
  }: Props = $props();

  // Same glyphs as the current sidebar, so each section keeps its icon.
  const Icon = $derived(
    {
      access: KeyRoundIcon,
      devices: MonitorSmartphoneIcon,
      recovery: BriefcaseMedicalIcon,
      apps: LayoutGridIcon,
      integrations: BotIcon,
    }[kind],
  );

  const loud = $derived(tone === "risk" || tone === "action");
</script>

<!-- The row itself stays plain: the tinted icon carries the tone and the
     button carries the ask, so the card reads as one list, not two halves. -->
<li class="flex flex-col">
  <button
    type="button"
    onclick={onOpen}
    aria-label={`${title}: ${sentence}`}
    class="ii-hover focus-visible:ring-focus-ring flex w-full items-center gap-3 px-4 py-3 text-start outline-none focus-visible:ring-2 focus-visible:ring-inset"
  >
    <span
      aria-hidden="true"
      class={[
        "grid size-9 shrink-0 place-items-center rounded-[0.625rem] transition-colors duration-200 *:col-start-1 *:row-start-1",
        {
          risk: "bg-bg-error-secondary text-fg-error-primary",
          action: "bg-bg-warning-secondary text-fg-warning-primary",
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
    <span class="flex min-w-0 flex-1 flex-col gap-0.5">
      <span class="text-text-primary text-sm font-medium">{title}</span>
      <!-- One size and weight for every sentence: the tone shows in its
           color, the tinted icon and the button, not in heavier text. -->
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
    </span>
    <ChevronRightIcon
      aria-hidden="true"
      class="text-fg-quaternary size-4 shrink-0 rtl:-scale-x-100"
    />
  </button>
  {#if loud && action !== undefined}
    <div
      transition:slide={{
        duration: prefersReducedMotion.current ? 0 : 240,
        easing: quintOut,
      }}
    >
      <div class="ps-16 pe-4 pb-4">
        <button
          type="button"
          onclick={onAction}
          class="btn btn-primary btn-sm rounded-full font-medium transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100"
        >
          {action.label}
        </button>
      </div>
    </div>
  {/if}
</li>
