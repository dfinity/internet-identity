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
  import { cubicIn, quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { fade } from "svelte/transition";
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
    /** A glimpse of what's inside (the methods, the browsers), so the row
     *  says more than its group's name. */
    preview?: Snippet;
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

  /** The step this row offers: only a loud row does. */
  const offered = $derived(
    tone === "risk" || tone === "action" ? action : undefined,
  );
</script>

<!-- The row itself stays plain: the tinted icon carries the tone and the
     button carries the ask, so the card reads as one list, not two halves.
     The ask stands at the row's end, where a quiet row has its chevron, so
     every row keeps one height and the step is the last thing on its line. -->
<li class="relative flex">
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
    {#if offered !== undefined}
      <!-- Keeps the step's room in the row: the step is a button of its own,
           and a button can't sit inside this one, so it lies over this. -->
      <span
        aria-hidden="true"
        class="btn btn-sm invisible shrink-0 font-medium"
      >
        {offered.label}
      </span>
    {:else}
      {#if preview !== undefined}
        <span aria-hidden="true" class="flex shrink-0 items-center">
          {@render preview()}
        </span>
      {/if}
      <ChevronRightIcon
        aria-hidden="true"
        class="text-fg-quaternary size-4 shrink-0 rtl:-scale-x-100"
      />
    {/if}
  </button>
  {#if offered !== undefined}
    <button
      type="button"
      onclick={onAction}
      aria-label={`${title}: ${offered.label}`}
      in:fade={{ duration: 200, easing: quintOut }}
      out:fade={{
        duration: prefersReducedMotion.current ? 0 : 100,
        easing: cubicIn,
      }}
      class="btn btn-primary btn-sm absolute end-4 top-1/2 -translate-y-1/2 rounded-full font-medium transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100"
    >
      {offered.label}
    </button>
  {/if}
</li>
