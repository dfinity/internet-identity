<script lang="ts">
  import {
    BellIcon,
    BellOffIcon,
    CheckCheckIcon,
    CheckIcon,
    ExternalLinkIcon,
    InfoIcon,
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
    /** From the tile's right-click menu: the app's settings, its details, or
     *  removing it. */
    onSetNotifications?: (allowed: boolean) => void;
    onDetails?: () => void;
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
    onDetails,
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
  // details panel offers, so none of it takes a trip to the panel.
  const menuItems: ContextMenuEntry[] = $derived([
    {
      label: "Open in new tab",
      icon: ExternalLinkIcon,
      href: url,
      onSelect: onOpen,
    },
    { label: "Copy link", icon: LinkIcon, onSelect: copyLink },
    { kind: "separator" },
    ...(hasUnread
      ? [
          {
            label: "Mark all as read",
            icon: CheckCheckIcon,
            onSelect: onClearAll,
          },
        ]
      : []),
    notificationsAllowed
      ? {
          label: "Turn off notifications",
          icon: BellOffIcon,
          onSelect: () => onSetNotifications?.(false),
        }
      : {
          label: "Turn on notifications",
          icon: BellIcon,
          onSelect: () => onSetNotifications?.(true),
        },
    { kind: "separator" },
    { label: "App details", icon: InfoIcon, onSelect: onDetails },
    {
      label: "Remove app",
      icon: Trash2Icon,
      tone: "destructive",
      onSelect: onRemove,
    },
  ]);

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
  <span class="relative">
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
         reaches past the badge without growing it. -->
    <button
      bind:this={badgeRef}
      type="button"
      onclick={() => (isUnreadOpen = true)}
      aria-haspopup="dialog"
      aria-expanded={isUnreadOpen}
      aria-label={`${unread.length} unread from ${name}`}
      class="bg-bg-brand-solid text-text-primary-inversed focus-visible:ring-focus-ring absolute end-[calc(50%-2.5rem)] top-2 flex h-6 min-w-6 items-center justify-center rounded-full px-1.5 text-xs font-semibold tabular-nums ring-2 ring-(color:--ii-surface-3) outline-none before:absolute before:-inset-2.5 focus-visible:ring-4"
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
