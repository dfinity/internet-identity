<script lang="ts" module>
  import type { CardKind, LeadAction, Tone } from "../status";

  export interface DockItem {
    kind: CardKind;
    title: string;
    sentence: string;
    tone: Tone;
    action: LeadAction;
    /** The one step the user should take; the rest offer a quieter button. */
    primary: boolean;
  }
</script>

<script lang="ts">
  import {
    BotIcon,
    BriefcaseMedicalIcon,
    KeyRoundIcon,
    LayoutGridIcon,
    MonitorSmartphoneIcon,
  } from "@lucide/svelte";
  import { flip } from "svelte/animate";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { shellIn, shellOut, widgetIn, widgetOut } from "../motion";

  interface Props {
    items: DockItem[];
    onOpen: (kind: CardKind) => void;
    onAction: (kind: CardKind) => void;
  }

  const { items, onOpen, onAction }: Props = $props();

  const reduced = $derived(prefersReducedMotion.current);

  // The same glyphs as the panel's rows, so a widget reads as that row.
  const ICON = {
    access: KeyRoundIcon,
    devices: MonitorSmartphoneIcon,
    recovery: BriefcaseMedicalIcon,
    apps: LayoutGridIcon,
    integrations: BotIcon,
  };

  const TILE: Partial<Record<Tone, string>> = {
    risk: "bg-bg-error-secondary text-fg-error-primary",
    action: "bg-bg-warning-secondary text-fg-warning-primary",
    queued: "bg-bg-warning-secondary text-fg-warning-primary",
  };

  const PRESS =
    "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
</script>

<!-- The page with everything closed: what needs the user waits here, one
     widget per step, and goes away once the step is done. -->
<div
  in:shellIn={{ reduced }}
  out:shellOut={{ reduced }}
  class="pointer-events-none fixed inset-x-0 bottom-[calc(0.75rem+env(safe-area-inset-bottom))] z-20 flex justify-center"
>
  <ul
    aria-label="Needs your attention"
    class="pointer-events-auto relative flex max-w-full snap-x snap-mandatory scroll-px-3 [scrollbar-width:none] gap-2.5 overflow-x-auto px-3 pt-2 pb-3 [&::-webkit-scrollbar]:hidden"
  >
    {#each items as item, index (item.kind)}
      {@const Icon = ICON[item.kind]}
      <li
        animate:flip={{ duration: reduced ? 0 : 320, easing: quintOut }}
        in:widgetIn={{ reduced, index }}
        out:widgetOut={{ reduced }}
        class="ii-surface-6 ii-elevation-6 flex w-[min(20rem,calc(100vw-3rem))] shrink-0 snap-start items-center gap-1 rounded-[1.375rem] p-1.5"
      >
        <button
          type="button"
          onclick={() => onOpen(item.kind)}
          aria-label={`${item.title}: ${item.sentence}. Open`}
          class="ii-hover focus-visible:ring-focus-ring flex min-w-0 flex-1 items-center gap-3 rounded-[1rem] p-1.5 text-start outline-none focus-visible:ring-2"
        >
          <span
            aria-hidden="true"
            class={[
              "grid size-9 shrink-0 place-items-center rounded-[0.625rem]",
              TILE[item.tone] ?? "text-fg-secondary bg-(--ii-selected)",
            ]}
          >
            <Icon class="size-4.5" />
          </span>
          <span class="flex min-w-0 flex-col">
            <span class="text-text-tertiary text-xs">{item.title}</span>
            <span
              class={[
                "line-clamp-2 text-[13px] leading-[18px] font-medium text-pretty",
                item.tone === "risk"
                  ? "text-text-error-primary"
                  : "text-text-primary",
              ]}
            >
              {item.sentence}
            </span>
          </span>
        </button>
        <button
          type="button"
          onclick={() => onAction(item.kind)}
          class={[
            "btn btn-sm shrink-0 rounded-full",
            item.primary ? "btn-primary" : "btn-secondary",
            PRESS,
          ]}
        >
          {item.action.label}
        </button>
      </li>
    {/each}
  </ul>
</div>
