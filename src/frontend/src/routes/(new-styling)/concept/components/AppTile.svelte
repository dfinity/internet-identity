<script lang="ts">
  import {
    BellIcon,
    BellOffIcon,
    CheckCheckIcon,
    CheckIcon,
    CircleCheckIcon,
    EllipsisIcon,
    ExternalLinkIcon,
    LinkIcon,
    Trash2Icon,
  } from "@lucide/svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { scale } from "svelte/transition";
  import { toaster } from "$lib/components/utils/toaster";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { MockNotification } from "../mockData";
  import ContextMenu, { type ContextMenuEntry } from "./ContextMenu.svelte";

  interface Props {
    name: string;
    logo: string;
    url: string;
    unread?: MockNotification[];
    /** Whether the tile shows its unread count. Off where another tile on the
     *  page already shows the same app's, so one count is told once. */
    showCount?: boolean;
    /** `open` opens the app in one click; `select` picks it for managing; `inert` is
     *  shown but can't be picked (an app the identity has never used). */
    mode: "open" | "select" | "inert";
    selected?: boolean;
    /** Place in the grid, so the selection marks arrive one after another. */
    index?: number;
    onToggle?: () => void;
    /** The corner's "⋯": the app's details beside the grid (a sheet on a
     *  phone), with its notifications and settings. A click on the tile
     *  itself opens the app. */
    onDetails?: () => void;
    /** The app whose details are showing: its logo is ringed. */
    current?: boolean;
    /** Opening the app reads everything it sent. */
    onOpen?: () => void;
    onClearAll?: () => void;
    /** Whether the identity lets the app notify it. */
    notificationsAllowed?: boolean;
    /** The user turned the app's notifications off: it notified before and
     *  no longer may. Shown on the logo, since the grid can't tell otherwise. */
    muted?: boolean;
    /** From the tile's right-click menu: the app's settings, managing apps
     *  starting from this one, or removing it. */
    onSetNotifications?: (allowed: boolean) => void;
    onManage?: () => void;
    onRemove?: () => void;
  }

  const {
    name,
    logo,
    url,
    unread = [],
    showCount = true,
    mode,
    selected = false,
    index = 0,
    onToggle,
    onDetails,
    current = false,
    onOpen,
    onClearAll,
    notificationsAllowed = false,
    muted = false,
    onSetNotifications,
    onManage,
    onRemove,
  }: Props = $props();

  const badge = $derived(unread.length > 99 ? "99+" : `${unread.length}`);
  const hasUnread = $derived(mode === "open" && unread.length > 0);
  /** The corner shows a count. Not tied to the mode: while apps are picked the
   *  corner fades away with its count still in it, and comes back with it. */
  const counted = $derived(unread.length > 0 && showCount);

  const copyLink = () => {
    navigator.clipboard.writeText(url).then(
      () => toaster.success({ title: "Link copied", duration: 2500 }),
      () => toaster.create({ title: "Couldn't copy the link", duration: 3000 }),
    );
  };

  // What a browser's menu would offer for the link, then everything the app's
  // details panel offers, so none of it takes a trip to the panel. A group
  // shows only what the tile was given a handler for: a featured app the
  // identity hasn't used has nothing to read, set, manage or remove.
  const menuItems: ContextMenuEntry[] = $derived.by(() => {
    const groups: ContextMenuEntry[][] = [
      [
        {
          label: "Open in new tab",
          icon: ExternalLinkIcon,
          href: url,
          onSelect: onOpen,
        },
        { label: "Copy link", icon: LinkIcon, onSelect: copyLink },
      ],
      [
        ...(hasUnread && onClearAll !== undefined
          ? [
              {
                label: "Mark all as read",
                icon: CheckCheckIcon,
                onSelect: onClearAll,
              },
            ]
          : []),
        ...(onSetNotifications === undefined
          ? []
          : [
              notificationsAllowed
                ? {
                    label: "Turn off notifications",
                    icon: BellOffIcon,
                    onSelect: () => onSetNotifications(false),
                  }
                : {
                    label: "Turn on notifications",
                    icon: BellIcon,
                    onSelect: () => onSetNotifications(true),
                  },
            ]),
      ],
      [
        // Says what happens: the grid goes into selecting with this app
        // already checked, under the very mark the icon draws.
        ...(onManage === undefined
          ? []
          : [{ label: "Select", icon: CircleCheckIcon, onSelect: onManage }]),
        ...(onRemove === undefined
          ? []
          : [
              {
                label: "Remove app",
                icon: Trash2Icon,
                tone: "destructive" as const,
                onSelect: onRemove,
              },
            ]),
      ],
    ];
    return groups
      .filter((group) => group.length > 0)
      .flatMap((group, index) =>
        index === 0 ? group : [{ kind: "separator" as const }, ...group],
      );
  });

  // Shared by both faces: a 0.97 press, 160ms ease-out, off under reduced motion.
  const PRESS =
    "transition-[scale] duration-160 ease-out active:scale-[0.97] motion-reduce:active:scale-100";
</script>

{#snippet face()}
  <!-- While managing, the logo rocks the way icons do in a home screen's edit
       mode, each on its own beat, so the grid reads as one you can rearrange. -->
  <span
    class={[
      "relative",
      mode === "open" && "lift",
      mode === "select" && "jiggle",
    ]}
    style:animation-delay={mode === "select"
      ? `-${(index * 97) % 260}ms`
      : undefined}
    style:animation-direction={mode === "select" && index % 2 === 1
      ? "reverse"
      : undefined}
  >
    <!-- The logo takes the canvas's size for it (--ii-logo), which grows on
         a wide canvas, over the 64px AppLogo draws a tile at. -->
    <span
      class={[
        "block rounded-2xl outline-offset-[3px] transition-[outline-color] duration-150 *:size-(--ii-logo)!",
        current ? "outline-fg-primary outline-2" : "outline-transparent",
      ]}
    >
      <AppLogo {logo} size="tile" reveal />
    </span>
    {#if muted}
      <!-- Notifications turned off: the count's circle, in grey and on the
           logo's lower corner, where iOS puts a status on an avatar. It stays
           while managing, so muted apps can be told apart there too. -->
      <span
        aria-hidden="true"
        transition:scale={{
          start: 0.5,
          duration: prefersReducedMotion.current ? 0 : 220,
          easing: quintOut,
        }}
        class="absolute -end-[5px] -bottom-[5px] grid size-4.5 place-items-center rounded-full bg-(--ii-muted) text-white ring-2 ring-(color:--ii-tile-backdrop)"
      >
        <BellOffIcon class="size-3" strokeWidth={2.5} />
      </span>
    {/if}
  </span>
  <span
    class="text-text-secondary line-clamp-2 w-full text-center text-(length:--ii-tile-label) leading-tight text-balance break-words"
  >
    {name}
  </span>
{/snippet}

<div class="tile relative">
  {#if mode === "open"}
    <!-- Held on, the tile goes on into managing apps, as an iPhone icon goes
         on into editing the home screen. -->
    <ContextMenu
      items={menuItems}
      title={new URL(url).host}
      label={name}
      onHold={onManage}
    >
      {#snippet trigger(menu)}
        <a
          {...menu}
          data-tile-link
          href={url}
          target="_blank"
          rel="noopener noreferrer"
          onclick={onOpen}
          aria-current={current ? "true" : undefined}
          aria-label={[
            `Open ${name}`,
            hasUnread && `${unread.length} unread`,
            muted && "notifications off",
          ]
            .filter(Boolean)
            .join(", ")}
          class={[
            // No plate behind it: hovered, the logo grows instead (`.lift`).
            "focus-visible:ring-focus-ring flex w-full flex-col items-center gap-2.5 rounded-2xl px-1.5 pt-4 pb-3 outline-none focus-visible:ring-2",
            PRESS,
          ]}
        >
          {@render face()}
        </a>
      {/snippet}
    </ContextMenu>
  {:else}
    <button
      type="button"
      onclick={onToggle}
      disabled={mode === "inert"}
      aria-pressed={mode === "select" ? selected : undefined}
      aria-label={mode === "inert"
        ? `${name}, not used yet`
        : `${selected ? "Deselect" : "Select"} ${name}${muted ? ", notifications off" : ""}`}
      class={[
        "focus-visible:ring-focus-ring flex w-full flex-col items-center gap-2.5 rounded-2xl px-1.5 pt-4 pb-3 outline-none focus-visible:ring-2",
        mode === "inert" && "opacity-40",
        mode === "select" && [selected ? "ii-selected" : "ii-hover", PRESS],
      ]}
    >
      {@render face()}
    </button>
  {/if}

  {#if onToggle !== undefined}
    <!-- The selection mark lies over the logo, not inside the tile's link or
         button, which are swapped when picking starts and ends. So it stays
         put and can come and go by itself: the marks arrive one after
         another, and leave together, quicker. The layer is the logo's size
         and place and rocks with it, on the same beat. -->
    <span
      aria-hidden="true"
      class={[
        "pointer-events-none absolute start-[calc(50%-var(--ii-logo)/2)] top-4 size-(--ii-logo)",
        mode === "select" && "jiggle",
      ]}
      style:animation-delay={mode === "select"
        ? `-${(index * 97) % 260}ms`
        : undefined}
      style:animation-direction={mode === "select" && index % 2 === 1
        ? "reverse"
        : undefined}
    >
      <span
        data-shown={mode === "select" ? "" : undefined}
        style:--mark-delay={`${Math.min(index, 12) * 30}ms`}
        class={[
          // The unread count's size and place, mirrored to the start corner:
          // 18px, 5px over the logo's edges. The 1.5px ring matches the
          // check's own stroke (3 on lucide's 24-unit grid, drawn at 12px).
          "mark absolute -start-[5px] -top-[5px] flex size-4.5 items-center justify-center rounded-full border-[1.5px]",
          selected
            ? "border-bg-brand-solid bg-bg-brand-solid text-text-primary-inversed"
            : "border-border-primary bg-bg-primary",
        ]}
      >
        {#if selected}
          <CheckIcon class="size-3" strokeWidth={3} />
        {/if}
      </span>
    </span>
  {/if}

  {#if onDetails !== undefined}
    <!-- The tile opens the app, the way Okta's does; its corner holds the
         rest: the app's notifications and settings, behind "⋯". At rest the
         corner shows the unread count, and turns into "⋯" while the tile is
         hovered; with nothing unread it is only "⋯", there on hover. Touch
         has no hover, so there the corner always takes a tap: the count, or
         a quiet "⋯" where there's none. While apps are picked the corner
         steps away for the selection mark, and comes back after. -->
    <button
      type="button"
      onclick={onDetails}
      inert={mode !== "open"}
      data-away={mode !== "open" ? "" : undefined}
      aria-label={[`${name} details`, counted && `${unread.length} unread`]
        .filter(Boolean)
        .join(", ")}
      aria-expanded={current}
      class={[
        "corner focus-visible:ring-focus-ring absolute end-[calc(50%-var(--ii-logo)/2-0.3125rem)] top-[11px] flex h-4.5 min-w-4.5 items-center justify-center rounded-full px-[5px] text-[11px] leading-none font-medium tabular-nums ring-2 ring-(color:--ii-tile-backdrop) outline-none before:absolute before:-inset-[13px] focus-visible:ring-4",
        muted
          ? "bg-(--ii-muted) text-white"
          : "bg-bg-brand-solid text-text-primary-inversed",
        !counted && "corner-bare",
      ]}
    >
      {#if counted}
        <span class="cue-figure">{badge}</span>
      {/if}
      <span
        aria-hidden="true"
        class="cue-more absolute inset-0 grid place-items-center"
      >
        <EllipsisIcon class="size-3.5" strokeWidth={2.75} />
      </span>
    </button>
  {/if}
</div>

<style>
  .jiggle {
    animation: jiggle 260ms ease-in-out infinite;
  }

  @keyframes jiggle {
    0%,
    100% {
      rotate: -1.5deg;
    }
    50% {
      rotate: 1.5deg;
    }
  }

  /* The tile being dragged holds still, lifted off the page, and loses the
     plate its press would draw, which would read as picked. */
  :global([data-dragging]) .jiggle {
    animation: none;
    filter: drop-shadow(0 8px 14px rgb(0 0 0 / 0.18));
  }
  :global([data-dragging]) .ii-hover {
    background-image: none;
  }

  @media (prefers-reduced-motion: reduce) {
    .jiggle {
      animation: none;
    }
  }

  /* Hovering the tile shows the way to its details: the count's circle
     turns into "⋯", or the same circle appears where a count would sit.
     --cue is 1 meanwhile. With no count, "⋯" takes clicks only while it
     shows. */
  @media (hover: hover) {
    .tile:hover {
      --cue: 1;
      --lift: 1;
    }
  }
  .tile:has(:focus-visible) {
    --cue: 1;
    --lift: 1;
  }
  /* The tile has no plate: hovered or focused, its logo grows by 6%, and it
     stays grown while the tile's menu is open, so it's clear whose it is.
     --lift is 1 meanwhile. The corner rides on the logo's corner, which
     moves 3% of the logo out each way. */
  .tile:has([data-tile-link][aria-expanded="true"]) {
    --lift: 1;
  }
  .lift {
    scale: calc(1 + 0.06 * var(--lift, 0));
    transition: scale 220ms var(--ii-ease-out);
  }
  .corner {
    translate: calc(var(--ii-logo) * 0.03 * var(--lift, 0))
      calc(var(--ii-logo) * -0.03 * var(--lift, 0));
    transition:
      opacity 150ms ease-out,
      color 200ms ease-out,
      background-color 200ms ease-out,
      scale 220ms var(--ii-ease-out),
      translate 220ms var(--ii-ease-out);
  }
  .corner:dir(rtl) {
    translate: calc(var(--ii-logo) * -0.03 * var(--lift, 0))
      calc(var(--ii-logo) * -0.03 * var(--lift, 0));
  }
  .corner-bare {
    opacity: var(--cue, 0);
    pointer-events: none;
  }

  /* Picking apps, the count and the selection mark trade places. What leaves
     goes in 120ms, all together; what arrives grows from half its size over
     220ms, the marks one after another, 30ms apart. */
  .corner[data-away] {
    opacity: 0;
    scale: 0.5;
    pointer-events: none;
    transition-duration: 120ms;
    transition-timing-function: ease-out;
  }
  .mark {
    opacity: 0;
    scale: 0.5;
    transition:
      opacity 120ms ease-out,
      scale 120ms ease-out,
      background-color 150ms ease-out,
      border-color 150ms ease-out,
      color 150ms ease-out;
  }
  .mark[data-shown] {
    opacity: 1;
    scale: 1;
    transition-duration: 220ms, 220ms, 150ms, 150ms, 150ms;
    transition-timing-function:
      var(--ii-ease-out), var(--ii-ease-out), ease-out, ease-out, ease-out;
    transition-delay:
      var(--mark-delay, 0ms), var(--mark-delay, 0ms), 0ms, 0ms, 0ms;
  }
  /* Reduced motion: they only fade, all at once; the logo and the corner on
     it take their hovered size and place at once, without moving there. */
  @media (prefers-reduced-motion: reduce) {
    .corner,
    .corner[data-away],
    .mark,
    .mark[data-shown] {
      scale: 1;
      transition-delay: 0ms;
    }
    .lift {
      transition: none;
    }
    .corner {
      transition-property: opacity, color, background-color;
    }
  }
  @media (hover: hover) {
    .tile:hover .corner-bare {
      pointer-events: auto;
    }
  }
  /* Touch: the count stays the count, and a tile without one keeps a quiet
     "⋯" on the canvas's own grey, so every tile's details are a tap away. */
  @media (hover: none) {
    .corner-bare {
      opacity: 1;
      pointer-events: auto;
      background-color: var(--ii-shelf);
      color: var(--color-fg-tertiary);
    }
    .corner-bare .cue-more {
      --cue: 1;
    }
  }
  .cue-more,
  .cue-figure {
    transition: opacity 150ms ease-out;
  }
  .cue-more {
    opacity: var(--cue, 0);
  }
  .cue-figure {
    opacity: calc(1 - var(--cue, 0));
  }
  /* The contextual icon cross-fade: scale 0.25, blur 4px, 300ms. */
  @media (prefers-reduced-motion: no-preference) {
    .cue-more,
    .cue-figure {
      transition-property: opacity, scale, filter;
      transition-duration: 300ms;
      transition-timing-function: cubic-bezier(0.2, 0, 0, 1);
    }
    .cue-more {
      scale: calc(0.25 + 0.75 * var(--cue, 0));
      filter: blur(calc(4px * (1 - var(--cue, 0))));
    }
    .cue-figure {
      scale: calc(1 - 0.75 * var(--cue, 0));
      filter: blur(calc(4px * var(--cue, 0)));
    }
  }
</style>
