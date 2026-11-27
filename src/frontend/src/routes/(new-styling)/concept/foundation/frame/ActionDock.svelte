<script lang="ts" module>
  import type { CardKind, Tone } from "../../features/security/status";

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
  import { prefersReducedMotion } from "svelte/motion";
  import {
    hop,
    shellIn,
    shellOut,
    widgetIn,
    widgetOut,
  } from "../motion/motion";

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
  // takes the other theme's shade to stay readable on it. A risk's button is
  // red, not dark, so it doesn't pass for one more step: its glyph is white.
  const GLYPH: Partial<Record<Tone, string>> = {
    risk: "text-fg-error-primary",
    warning: "text-fg-warning-primary",
  };
  const GLYPH_ON_PRIMARY: Partial<Record<Tone, string>> = {
    risk: "text-white",
    warning: "text-warning-400 dark:text-warning-600",
  };

  // The widgets lie in a stack: the most urgent in front, the rest tucked
  // under it with only their top edges showing. Hovering, tabbing in or
  // tapping an edge spreads them into a column, each at its own width.
  /** A widget's height (h-11), and the room between spread widgets. */
  const ROW = 44;
  const GAP = 8;
  /** How far each tucked widget shows above the one in front of it. */
  const PEEK = 9;
  /** A widget's inline padding (ps-3.75 + pe-4.5) around its glyph and label. */
  const PADDING = 33;

  let stack = $state<HTMLElement>();
  const inStack = (target: EventTarget | null) =>
    target instanceof Node && stack?.contains(target) === true;
  let hovered = $state(false);
  let focused = $state(false);
  /** A tap on a tucked edge spreads the stack until a tap lands outside it. */
  let tapped = $state(false);
  /** The user is on the dock: pointing at it, tabbed into it or tapped it. */
  const engaged = $derived(hovered || focused || tapped);
  const spread = $derived(engaged && items.length > 1);

  /** Each widget's glyph and label, measured, so a widget can take its own
   *  width when spread and the front one's when tucked under it. */
  const contentWidths = $state<Partial<Record<CardKind, number>>>({});
  const widthOf = (kind: CardKind) => (contentWidths[kind] ?? 0) + PADDING;
  const measured = $derived(
    items.every(({ kind }) => (contentWidths[kind] ?? 0) > 0),
  );
  const frontWidth = $derived(items.length > 0 ? widthOf(items[0].kind) : 0);
  const stackWidth = $derived(
    spread ? Math.max(...items.map(({ kind }) => widthOf(kind))) : frontWidth,
  );
  const stackHeight = $derived(
    spread
      ? items.length * ROW + (items.length - 1) * GAP
      : ROW + (items.length - 1) * PEEK,
  );

  /** Sizes only move once the widgets are measured, so the stack doesn't
   *  grow out of nothing as it first appears. */
  let settled = $state(false);
  $effect(() => {
    if (!measured || settled) {
      return;
    }
    const frame = requestAnimationFrame(() => (settled = true));
    return () => cancelAnimationFrame(frame);
  });

  // A step to take asks for the user now and then, as a Dock icon does: the
  // stack hops soon after it appears, then again on a steady beat for as long
  // as it's left waiting. Not while the user is on it, not in a hidden tab,
  // and not under reduced motion. Something only on its way never asks.
  /** The first hop, once the widgets have risen in and had a moment. */
  const NUDGE_FIRST = 1500;
  /** The beat after that. */
  const NUDGE_EVERY = 8000;
  const asking = $derived(items[0]?.primary === true);
  const front = $derived(items[0]?.kind);
  /** Whether the front step has hopped yet; a new step asks afresh. */
  let nudged = false;
  let nudgedFor: CardKind | undefined;
  $effect(() => {
    const node = stack;
    if (front !== nudgedFor) {
      nudgedFor = front;
      nudged = false;
    }
    if (engaged) {
      // The user has seen it: the next hop waits a full beat.
      nudged = true;
      return;
    }
    if (reduced || !asking || node === undefined) {
      return;
    }
    let timer: ReturnType<typeof setTimeout>;
    const schedule = () => {
      timer = setTimeout(
        () => {
          if (!document.hidden) {
            hop(node);
            nudged = true;
          }
          schedule();
        },
        nudged ? NUDGE_EVERY : NUDGE_FIRST,
      );
    };
    schedule();
    return () => clearTimeout(timer);
  });
</script>

<svelte:document
  onpointerdown={(event) => {
    if (!inStack(event.target)) {
      tapped = false;
    }
  }}
/>

<!-- The page with everything closed: what needs the user waits here, one
     button per step, and goes away once the step is done. -->
<div
  in:shellIn={{ reduced }}
  out:shellOut={{ reduced }}
  class="pointer-events-none fixed inset-x-0 bottom-[calc(1.5rem+env(safe-area-inset-bottom))] z-20 flex justify-center"
>
  <ul
    bind:this={stack}
    aria-label="Needs your attention"
    onpointerenter={(event) => {
      if (event.pointerType === "mouse") {
        hovered = true;
      }
    }}
    onpointerleave={(event) => {
      if (event.pointerType === "mouse") {
        hovered = false;
      }
    }}
    onfocusin={(event) => {
      if (
        event.target instanceof Element &&
        event.target.matches(":focus-visible")
      ) {
        focused = true;
      }
    }}
    onfocusout={(event) => {
      if (!inStack(event.relatedTarget)) {
        focused = false;
      }
    }}
    class={[
      "pointer-events-auto relative",
      settled &&
        "transition-[width,height] duration-450 ease-(--ii-ease-out) motion-reduce:transition-none",
    ]}
    style:width={`${stackWidth}px`}
    style:height={`${stackHeight}px`}
  >
    {#each items as item, index (item.kind)}
      {@const Icon = ICON[item.kind]}
      {@const tucked = !spread && index > 0}
      <li
        class={[
          "absolute bottom-0 left-1/2 flex",
          settled &&
            "transition-[transform,width] duration-450 ease-(--ii-ease-out) motion-reduce:transition-none",
        ]}
        style:width={`${spread ? widthOf(item.kind) : frontWidth}px`}
        style:transform={`translateX(-50%) translateY(${-index * (spread ? ROW + GAP : PEEK)}px) scale(${spread ? 1 : 1 - index * 0.05})`}
        style:z-index={items.length - index}
      >
        <button
          type="button"
          in:widgetIn={{ reduced, index }}
          out:widgetOut={{ reduced }}
          tabindex={tucked ? -1 : undefined}
          onclick={(event) => {
            // A tap on a tucked edge spreads the stack first. A click with no
            // pointer (a screen reader's) takes the step straight away.
            if (tucked && event.detail > 0) {
              tapped = true;
              return;
            }
            onAction(item.kind);
          }}
          class={[
            "flex h-11 w-full items-center justify-center overflow-hidden rounded-full ps-3.75 pe-4.5 text-sm font-medium whitespace-nowrap outline-none",
            "focus-visible:ring-focus-ring focus-visible:ring-offset-bg-primary focus-visible:ring-2 focus-visible:ring-offset-2",
            "transition-[scale,background-color,box-shadow] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100",
            // A tucked widget is only an edge, so it casts a shorter shadow.
            tucked ? "ii-elevation-2" : "ii-elevation-6",
            // The floating step as a utility, not a class: the reset for
            // buttons outranks the surface classes, and the widget must not
            // show the page through.
            item.primary
              ? item.tone === "risk"
                ? "btn-primary btn-danger"
                : "btn-primary"
              : "ii-hover text-text-primary bg-(--ii-floating)",
          ]}
        >
          <span
            bind:offsetWidth={contentWidths[item.kind]}
            class={[
              "flex shrink-0 items-center gap-2 transition-opacity duration-200 ease-out",
              tucked && "opacity-0",
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
            <span class="max-w-[calc(100vw-7rem)] truncate">{item.label}</span>
          </span>
        </button>
      </li>
    {/each}
  </ul>
</div>
