<script lang="ts">
  import { untrack, type Snippet } from "svelte";
  import {
    BotIcon,
    BriefcaseMedicalIcon,
    CheckIcon,
    ChevronRightIcon,
    Grid2x2CheckIcon,
    KeyRoundIcon,
    MonitorSmartphoneIcon,
    XIcon,
  } from "@lucide/svelte";
  import { prefersReducedMotion } from "svelte/motion";
  import { MediaQuery } from "svelte/reactivity";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { TALL_PANEL } from "../panel";
  import type { CardKind, Tone } from "../status";
  import { swapIn, swapOut } from "../motion";
  import Glyphs, { type Glyph } from "./Glyphs.svelte";

  interface Props {
    kind: CardKind;
    title: string;
    sentence: string;
    tone: Tone;
    /** Just resolved: the icon turns into a check for a moment. */
    resolved?: boolean;
    /** Grown into its whole section: the section takes the preview's place
     *  and Close the chevron's. */
    open?: boolean;
    /** While the card grows or shrinks back, both the preview and the
     *  section are there, the leaving one out of the flow, so the panel can
     *  cross them (see `morph` in motion.ts). */
    morphing?: boolean;
    onOpen: () => void;
    onClose?: () => void;
    /** What the sentence is about, beside it: what's inside (the ways in,
     *  the ways back in, the browsers) while the card is neutral, and the
     *  one item it's about otherwise. */
    glyphs?: Glyph[];
    /** The section itself, shown while the card is open. */
    children?: Snippet;
  }

  const {
    kind,
    title,
    sentence,
    tone,
    resolved = false,
    open = false,
    morphing = false,
    onOpen,
    onClose,
    glyphs = [],
    children,
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

  const reduced = $derived(prefersReducedMotion.current);

  /** Where there's the height for it, a card takes its room (Thomas): a
   *  larger icon, the title under it, a larger sentence, more air. In a
   *  lower window it keeps to the compact row, title beside the icon. */
  const tall = new MediaQuery(TALL_PANEL);
  const large = $derived(tall.current);
  const contentId = $props.id();

  // The control pressed leaves as the card grows or shrinks, so focus moves
  // to the one that takes its place: Close, then the card again.
  let openButton = $state<HTMLButtonElement>();
  let closeButton = $state<HTMLButtonElement>();
  let wasOpen = untrack(() => open);
  $effect(() => {
    const now = open;
    if (now === wasOpen) return;
    wasOpen = now;
    // Close is still there while it fades out, holding focus.
    const leaving = now ? openButton : closeButton;
    const active = document.activeElement;
    if (active === null || active === document.body || active === leaving) {
      (now ? closeButton : openButton)?.focus({ preventScroll: true });
    }
  });

  // What each glyph is, on hover. The glyphs lie over the card's one button
  // and let the pointer through to it, so the whole card stays one target;
  // the button finds which of them the pointer is on, the topmost where two
  // overlap, and the product's tooltip stands over that one. Only for a
  // mouse: a tap opens the card, and the open card names every item.
  // The tooltip is always there and only hides, keeping the last glyph, so
  // it fades out still saying it, and nothing reads a glyph that's gone.
  let summary = $state<HTMLElement>();
  let hovered = $state<{ glyph: Glyph; anchor: HTMLElement }>();
  let hovering = $state(false);
  const hover = (event: PointerEvent) => {
    if (event.pointerType !== "mouse" || summary === undefined) {
      hovering = false;
      return;
    }
    const chips = Array.from(
      summary.querySelectorAll<HTMLElement>("[data-glyph]"),
    ).reverse();
    const anchor = chips.find((chip) => {
      const box = chip.getBoundingClientRect();
      const radius = box.width / 2;
      return (
        Math.hypot(
          event.clientX - (box.left + radius),
          event.clientY - (box.top + radius),
        ) <= radius
      );
    });
    const glyph = glyphs.find(({ id }) => id === anchor?.dataset.glyph);
    hovering = anchor !== undefined && glyph !== undefined;
    if (
      anchor !== undefined &&
      glyph !== undefined &&
      (anchor !== hovered?.anchor || glyph !== hovered.glyph)
    ) {
      hovered = { glyph, anchor };
    }
  };
</script>

<!-- One large target per section: the whole card leads to its section,
     which it grows into in place, so it's always the same card. Closed,
     nothing inside it is a button of its own: one button lies under the
     whole card. Open, the section's own buttons are there, and Close stands
     where the chevron was. The tinted icon and the sentence carry the tone. -->
<div class="relative flex flex-col rounded-[inherit]">
  {#if !open}
    <button
      bind:this={openButton}
      type="button"
      onclick={() => {
        hovering = false;
        onOpen();
      }}
      onpointermove={hover}
      onpointerleave={() => (hovering = false)}
      aria-label={`${title}: ${sentence}`}
      aria-expanded="false"
      aria-controls={contentId}
      class="ii-hover focus-visible:ring-focus-ring absolute inset-0 rounded-[inherit] outline-none focus-visible:ring-2 focus-visible:ring-inset"
    ></button>
  {/if}
  <!-- Large: the icon and the chevron (or Close) on one line, the title on
       the next. Compact: all three on one line. The 16px sides stay either
       way, the identity's and the rows' own edge. -->
  <div
    aria-hidden={!open}
    class={[
      "pointer-events-none relative px-4",
      large
        ? "grid grid-cols-[auto_minmax(0,1fr)_auto] items-center pt-5"
        : "flex items-center gap-3 pt-3.5",
    ]}
  >
    <span
      aria-hidden="true"
      class={[
        "grid shrink-0 place-items-center transition-colors duration-200 *:col-start-1 *:row-start-1",
        large ? "size-11 rounded-xl" : "size-9 rounded-[0.625rem]",
        {
          risk: "text-fg-error-primary bg-(--ii-error-tint)",
          warning: "text-fg-warning-primary bg-(--ii-warning-tint)",
        }[tone as "risk" | "warning"] ?? "text-fg-secondary bg-(--ii-selected)",
      ]}
    >
      {#if resolved}
        <span
          class="text-fg-success-primary"
          in:swapIn={{ reduced }}
          out:swapOut={{ reduced }}
        >
          <CheckIcon
            class={large ? "size-5.5" : "size-4.5"}
            strokeWidth={2.5}
          />
        </span>
      {:else}
        <span in:swapIn={{ reduced }} out:swapOut={{ reduced }}>
          <Icon class={large ? "size-5.5" : "size-4.5"} />
        </span>
      {/if}
    </span>
    <h3
      class={[
        "text-text-primary min-w-0 font-semibold",
        large
          ? "col-span-full row-start-2 mt-3.5 text-[1.0625rem] leading-6 tracking-[-0.01em]"
          : "flex-1 text-sm",
      ]}
    >
      {title}
    </h3>
    <!-- The chevron and Close share one cell at the card's end, so the
         chevron turns into Close where it stood. -->
    <span
      class={[
        "-me-2.5 grid size-9 shrink-0 place-items-center *:col-start-1 *:row-start-1",
        large && "col-start-3 row-start-1",
      ]}
    >
      {#if open}
        <button
          bind:this={closeButton}
          type="button"
          onclick={onClose}
          aria-expanded="true"
          aria-controls={contentId}
          in:swapIn={{ reduced, delay: reduced ? 0 : 60 }}
          out:swapOut={{ reduced }}
          class="btn btn-tertiary btn-icon btn-sm pointer-events-auto rounded-full"
        >
          <XIcon class="size-4.5" aria-hidden="true" />
          <span>Close {title}</span>
        </button>
      {:else}
        <span in:swapIn={{ reduced }} out:swapOut={{ reduced }}>
          <ChevronRightIcon
            aria-hidden="true"
            class="text-fg-quaternary size-4 rtl:-scale-x-100"
          />
        </span>
      {/if}
    </span>
  </div>
  <!-- The message and the section start at one line under the title, so
       the section's lead takes the message's place as they cross. -->
  <div class={["relative", !open && "pointer-events-none"]}>
    {#if !open || morphing}
      <div
        bind:this={summary}
        data-layer="summary"
        aria-hidden="true"
        class={[
          "pointer-events-none flex min-h-7 items-center gap-2.5 px-4",
          large ? "pt-2 pb-5" : "pt-2.5 pb-4",
          open && "absolute inset-x-0 top-0",
        ]}
      >
        <!-- One message, Thomas's way: the icons of what it's about and the
             sentence, never a second line saying the same in other words.
             One size and weight for every sentence: the style shows in its
             color, its dot and the tinted icon, not in heavier text. Neutral
             says where the card stands, quietly; info, something to know;
             warning tints the icon amber; risk turns the icon and the
             sentence red. The dot sits on the item the message is about; an
             info about no item has it before the sentence instead. -->
        {#if glyphs.length > 0}
          <Glyphs {glyphs} {tone} />
          <Tooltip
            label={hovered?.glyph.label ?? ""}
            description={hovered?.glyph.note}
            anchor={hovered?.anchor}
            hidden={!hovering || open}
            manual
          />
        {/if}
        <span
          class={[
            "flex min-w-0 items-center gap-1.5 text-pretty",
            large ? "text-sm" : "text-[13px] leading-[18px]",
            {
              risk: "text-text-error-primary",
              warning: "text-text-primary",
              info: "text-text-primary",
              neutral: "text-text-tertiary",
            }[tone],
          ]}
        >
          {#if tone === "info" && !glyphs.some(({ marked }) => marked === true)}
            <span class="bg-fg-primary size-1.5 shrink-0 rounded-full"></span>
          {/if}
          <span class="min-w-0">{sentence}</span>
        </span>
      </div>
    {/if}
    {#if open || morphing}
      <div
        id={contentId}
        data-layer="content"
        inert={!open}
        class={[!open && "absolute inset-x-0 top-0"]}
      >
        {@render children?.()}
      </div>
    {/if}
  </div>
</div>
