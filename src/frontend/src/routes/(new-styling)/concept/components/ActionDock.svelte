<script lang="ts" module>
  import type { CardKind, Tone } from "../status";

  export interface DockItem {
    kind: CardKind;
    tone: Tone;
    /** The step, as a verb: the widget is the button that takes it. */
    label: string;
    /** The one step the user should take shows dark; the rest stay light. */
    primary: boolean;
  }
</script>

<script lang="ts">
  import {
    BotIcon,
    BriefcaseMedicalIcon,
    Grid2x2CheckIcon,
    KeyRoundIcon,
    MonitorSmartphoneIcon,
  } from "@lucide/svelte";
  import { flip } from "svelte/animate";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { shellIn, shellOut, widgetIn, widgetOut } from "../motion";

  interface Props {
    items: DockItem[];
    onAction: (kind: CardKind) => void;
  }

  const { items, onAction }: Props = $props();

  const reduced = $derived(prefersReducedMotion.current);

  // The same glyphs as the panel's rows, so a widget reads as that row.
  const ICON = {
    access: KeyRoundIcon,
    devices: MonitorSmartphoneIcon,
    recovery: BriefcaseMedicalIcon,
    apps: Grid2x2CheckIcon,
    integrations: BotIcon,
  };

  // The glyph carries the tone. The dark button is inverted, so its glyph
  // takes the other theme's shade to stay readable on it.
  const GLYPH: Partial<Record<Tone, string>> = {
    risk: "text-fg-error-primary",
    action: "text-fg-warning-primary",
    queued: "text-fg-warning-primary",
  };
  const GLYPH_ON_PRIMARY: Partial<Record<Tone, string>> = {
    risk: "text-error-400 dark:text-error-600",
    action: "text-warning-400 dark:text-warning-600",
  };
</script>

<!-- The page with everything closed: what needs the user waits here, one
     button per step, and goes away once the step is done. -->
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
        class="flex shrink-0 snap-start"
      >
        <button
          type="button"
          onclick={() => onAction(item.kind)}
          class={[
            "ii-elevation-6 flex h-11 max-w-[calc(100vw-3rem)] items-center gap-2 rounded-full ps-3.75 pe-4.5 text-sm font-medium whitespace-nowrap outline-none",
            "focus-visible:ring-focus-ring focus-visible:ring-offset-bg-primary focus-visible:ring-2 focus-visible:ring-offset-2",
            "transition-[scale,background-color] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100",
            // A utility, not `ii-surface-6`: the reset for buttons outranks
            // the surface classes, and the widget must not show the page through.
            item.primary
              ? "btn-primary"
              : "ii-hover text-text-primary bg-(--ii-surface-6)",
          ]}
        >
          <Icon
            aria-hidden="true"
            class={[
              "size-4 shrink-0",
              (item.primary ? GLYPH_ON_PRIMARY : GLYPH)[item.tone] ??
                "text-fg-tertiary",
            ]}
          />
          <span class="min-w-0 truncate">{item.label}</span>
        </button>
      </li>
    {/each}
  </ul>
</div>
