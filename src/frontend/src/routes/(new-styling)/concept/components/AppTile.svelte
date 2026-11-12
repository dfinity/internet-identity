<script lang="ts">
  import {
    BellIcon,
    BellOffIcon,
    CheckCheckIcon,
    CheckIcon,
    ExternalLinkIcon,
    LayoutGridIcon,
    LinkIcon,
    Trash2Icon,
    XIcon,
  } from "@lucide/svelte";
  import { quintOut } from "svelte/easing";
  import { prefersReducedMotion } from "svelte/motion";
  import { scale } from "svelte/transition";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { formatRelative } from "$lib/stores/locale.store";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { MockNotification } from "../mockData";
  import ContextMenu, { type ContextMenuEntry } from "./ContextMenu.svelte";

  interface Props {
    name: string;
    logo: string;
    url: string;
    unread?: MockNotification[];
    /** `open` opens the app in one click; `select` picks it for managing; `inert` is
     *  shown but can't be picked (an app the identity has never used). */
    mode: "open" | "select" | "inert";
    selected?: boolean;
    /** Place in the grid, so the selection marks arrive one after another. */
    index?: number;
    onToggle?: () => void;
    /** Opening the app reads everything it sent. */
    onOpen?: () => void;
    onRead?: (id: string) => void;
    onClearAll?: () => void;
    /** Whether the identity lets the app notify it. */
    notificationsAllowed?: boolean;
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
    mode,
    selected = false,
    index = 0,
    onToggle,
    onOpen,
    onRead,
    onClearAll,
    notificationsAllowed = false,
    onSetNotifications,
    onManage,
    onRemove,
  }: Props = $props();

  const badge = $derived(unread.length > 99 ? "99+" : `${unread.length}`);
  const hasUnread = $derived(mode === "open" && unread.length > 0);

  let badgeRef = $state<HTMLElement>();
  let isUnreadOpen = $state(false);

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
        ...(onManage === undefined
          ? []
          : [{ label: "Manage", icon: LayoutGridIcon, onSelect: onManage }]),
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

<svelte:window
  onkeydown={(event) => {
    if (isUnreadOpen && event.key === "Escape") {
      event.preventDefault();
      isUnreadOpen = false;
      badgeRef?.focus();
    }
  }}
/>

{#snippet face()}
  <!-- While managing, the logo rocks the way icons do in a home screen's edit
       mode, each on its own beat, so the grid reads as one you can rearrange. -->
  <span
    class={["relative", mode === "select" && "jiggle"]}
    style:animation-delay={mode === "select"
      ? `-${(index * 97) % 260}ms`
      : undefined}
    style:animation-direction={mode === "select" && index % 2 === 1
      ? "reverse"
      : undefined}
  >
    <AppLogo {logo} size="tile" />
    {#if mode === "select"}
      <span
        aria-hidden="true"
        in:scale|global={{
          start: 0.5,
          duration: prefersReducedMotion.current ? 0 : 220,
          delay: prefersReducedMotion.current ? 0 : Math.min(index, 12) * 30,
          easing: quintOut,
        }}
        class={[
          "absolute -start-2 -top-2 flex size-6 items-center justify-center rounded-full border-2 transition-colors duration-150",
          selected
            ? "border-bg-brand-solid bg-bg-brand-solid text-text-primary-inversed"
            : "border-border-primary bg-bg-primary",
        ]}
      >
        {#if selected}
          <CheckIcon class="size-3.5" strokeWidth={3} />
        {/if}
      </span>
    {/if}
  </span>
  <span
    class="text-text-secondary line-clamp-2 w-full text-center text-xs leading-tight text-balance break-words"
  >
    {name}
  </span>
{/snippet}

<div class="relative">
  {#if mode === "open"}
    <ContextMenu items={menuItems} title={new URL(url).host} label={name}>
      {#snippet trigger(menu, menuOpen)}
        <a
          {...menu}
          href={url}
          target="_blank"
          rel="noopener noreferrer"
          onclick={onOpen}
          aria-label={hasUnread
            ? `Open ${name}, ${unread.length} unread`
            : `Open ${name}`}
          class={[
            "focus-visible:ring-focus-ring flex w-full flex-col items-center gap-2.5 rounded-2xl px-1.5 pt-4 pb-3 outline-none focus-visible:ring-2",
            // The tile stays lit while its menu is open, so it's clear whose it is.
            menuOpen === true ? "ii-selected" : "ii-hover",
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
        : selected
          ? `Deselect ${name}`
          : `Select ${name}`}
      class={[
        "focus-visible:ring-focus-ring flex w-full flex-col items-center gap-2.5 rounded-2xl px-1.5 pt-4 pb-3 outline-none focus-visible:ring-2",
        mode === "inert" && "opacity-40",
        mode === "select" && [selected ? "ii-selected" : "ii-hover", PRESS],
      ]}
    >
      {@render face()}
    </button>
  {/if}

  {#if hasUnread}
    <!-- The count opens what arrived, so it can be read on touch too. The hit area
         reaches past the badge without growing it, to 44px.
         It sits on the logo's rounded corner rather than hanging off it: 5px
         over the 4rem logo's top and end edges (top: the tile's 1rem padding
         less 5px; end: half the logo, 2rem, plus 5px from the tile's middle). -->
    <button
      bind:this={badgeRef}
      type="button"
      onclick={() => (isUnreadOpen = true)}
      aria-haspopup="dialog"
      aria-expanded={isUnreadOpen}
      aria-label={`${unread.length} unread from ${name}`}
      class="bg-bg-brand-solid text-text-primary-inversed focus-visible:ring-focus-ring absolute end-[calc(50%-2.3125rem)] top-[11px] flex h-4.5 min-w-4.5 items-center justify-center rounded-full px-[5px] text-[11px] leading-none font-medium tabular-nums ring-2 ring-(color:--ii-surface-3) outline-none before:absolute before:-inset-[13px] focus-visible:ring-4"
    >
      {badge}
    </button>
  {/if}
</div>

{#if isUnreadOpen && hasUnread}
  <Popover
    anchor={badgeRef}
    onClose={() => (isUnreadOpen = false)}
    direction="down"
    align="start"
    distance="0.5rem"
  >
    <div class="flex flex-col p-1.5">
      <div class="flex items-baseline gap-2 px-3 pt-2 pb-1.5 max-[480px]:pe-12">
        <span class="text-text-primary text-sm font-semibold">{name}</span>
        <span class="text-text-tertiary text-xs tabular-nums">
          {unread.length} unread
        </span>
      </div>
      <ul class="flex max-h-80 flex-col overflow-y-auto overscroll-contain">
        {#each unread as notification (notification.id)}
          <li class="group/row relative">
            <a
              href={url}
              target="_blank"
              rel="noopener noreferrer"
              onclick={() => onRead?.(notification.id)}
              class="ii-hover focus-visible:ring-focus-ring grid grid-cols-[0.375rem_minmax(0,1fr)] items-start gap-x-2.5 rounded-lg px-3 py-2.5 outline-none focus-visible:ring-2 focus-visible:ring-inset"
            >
              <span
                aria-hidden="true"
                class="bg-fg-primary mt-1.5 size-1.5 rounded-full"
              ></span>
              <span class="flex min-w-0 flex-col gap-0.5 pe-7">
                <span class="flex items-baseline gap-2">
                  <span
                    class="text-text-primary min-w-0 flex-1 truncate text-sm font-semibold"
                  >
                    {notification.title}
                  </span>
                  <span class="text-text-tertiary shrink-0 text-xs">
                    {$formatRelative(new Date(notification.sentMillis), {
                      style: "short",
                    })}
                  </span>
                </span>
                <span class="text-text-tertiary line-clamp-2 text-xs">
                  {notification.body}
                </span>
              </span>
            </a>
            <!-- Always there on touch; on a mouse it appears with the row. -->
            <button
              type="button"
              onclick={() => onRead?.(notification.id)}
              aria-label={`Dismiss: ${notification.title}`}
              class="dismiss btn btn-tertiary btn-icon absolute end-1.5 top-1.5 size-7 rounded-md"
            >
              <XIcon class="size-3.5" />
            </button>
          </li>
        {/each}
      </ul>
      <div class="mt-1 flex gap-1">
        {#if unread.length > 1}
          <button
            type="button"
            onclick={onClearAll}
            class="btn btn-tertiary btn-sm text-text-tertiary flex-1"
          >
            Clear all
          </button>
        {/if}
        <a
          href={url}
          target="_blank"
          rel="noopener noreferrer"
          onclick={onOpen}
          class="btn btn-secondary btn-sm flex-1"
        >
          Open {name}
          <ExternalLinkIcon class="size-3.5" />
        </a>
      </div>
    </div>
  </Popover>
{/if}

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

  @media (hover: hover) {
    .dismiss {
      opacity: 0;
    }
    li:hover .dismiss,
    li:focus-within .dismiss {
      opacity: 1;
    }
  }
</style>
