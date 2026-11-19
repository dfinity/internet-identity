<script lang="ts">
  import {
    ArrowLeftIcon,
    ArrowUpRightIcon,
    BellIcon,
    BotIcon,
    CheckIcon,
    CopyIcon,
    MonitorSmartphoneIcon,
  } from "@lucide/svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { formatRelative } from "$lib/stores/locale.store";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { ConceptApp } from "../mockData";
  import Elevated from "./Elevated.svelte";

  interface Props {
    app: ConceptApp;
    /** Where Back goes: the identity's overview, or Done while managing. */
    backLabel?: string;
    onExit: () => void;
    onToggleNotifications: (allowed: boolean) => void;
    onRemove: () => void;
    /** Opening the app, or one of its notifications, reads them. */
    onOpen?: () => void;
    onRead?: (id: string) => void;
    onReadAll?: () => void;
    /** Off when the identity keeps no record of where it signs in, so there's
     *  no last sign-in to tell. */
    history?: boolean;
  }

  const {
    app,
    backLabel = "Back",
    onExit,
    onToggleNotifications,
    onRemove,
    onOpen,
    onRead,
    onReadAll,
    history = true,
  }: Props = $props();

  const host = $derived(new URL(app.url).host);
  // The domain (its last two labels) never shortens; what's before it may.
  const hostEnd = $derived(
    host.split(".").length > 2 ? `.${host.split(".").slice(-2).join(".")}` : "",
  );
  const hostStart = $derived(
    hostEnd === "" ? host : host.slice(0, -hostEnd.length),
  );
  const nameId = $props.id();
  const switchId = `${nameId}-notifications`;

  /** The link was just copied, so its button says so for a moment. */
  let copied = $state(false);
  let copiedTimer: ReturnType<typeof setTimeout> | undefined;

  const copyLink = async () => {
    try {
      await navigator.clipboard.writeText(app.url);
    } catch {
      toaster.create({ title: "Couldn't copy the link", duration: 3000 });
      return;
    }
    copied = true;
    clearTimeout(copiedTimer);
    copiedTimer = setTimeout(() => (copied = false), 1600);
  };

  const ROW = "flex items-center gap-3 px-4 py-3.5";
  const ICON =
    "text-fg-secondary flex size-9 shrink-0 items-center justify-center rounded-lg bg-(--ii-selected)";
</script>

<!-- Read top to bottom: what the app is, the way into it, then its settings.
     A phone's sheet keeps the same order. -->
<div class="flex flex-col gap-4">
  <!-- The identity's line above already says whose app this is. -->
  <button
    type="button"
    onclick={onExit}
    class="btn btn-tertiary btn-sm text-text-secondary hover:text-text-primary -ms-2 self-start px-2 font-medium"
  >
    <ArrowLeftIcon class="size-4 rtl:-scale-x-100" aria-hidden="true" />
    {backLabel}
  </button>

  <!-- What the app is, and the one way into it. -->
  <Elevated
    offset={2}
    as="section"
    aria-labelledby={nameId}
    class="flex flex-col gap-4 rounded-2xl p-4"
  >
    <div class="flex items-center gap-3.5">
      <!-- The same plate as its tile in the grid. -->
      <AppLogo logo={app.logo} size="tile" />
      <div class="flex min-w-0 flex-1 flex-col gap-0.5">
        <h2
          id={nameId}
          class="text-text-primary truncate text-lg leading-6 font-semibold tracking-tight"
        >
          {app.name}
        </h2>
        <!-- A canister's address is long: where it doesn't fit, its middle
             gives way, so the domain and the copy button stay in view. -->
        <p
          class="text-text-tertiary flex min-w-0 items-center text-[13px] leading-[18px]"
          title={host}
        >
          <span class="truncate">{hostStart}</span>
          <span class="shrink-0">{hostEnd}</span>
          <button
            type="button"
            onclick={copyLink}
            aria-label="Copy link"
            class="text-fg-tertiary hover:text-fg-secondary focus-visible:outline-focus-ring -my-1.5 ms-0.5 flex size-7 shrink-0 items-center justify-center rounded-md transition-colors duration-150 outline-none hover:bg-(--ii-hover) focus-visible:outline-2 active:bg-(--ii-selected)"
          >
            {#if copied}
              <CheckIcon
                class="text-fg-success-primary size-3.5"
                aria-hidden="true"
              />
            {:else}
              <CopyIcon class="size-3.5" aria-hidden="true" />
            {/if}
          </button>
        </p>
      </div>
    </div>
    {#if app.oneLiner !== undefined}
      <p class="text-text-secondary line-clamp-3 text-sm text-pretty">
        {app.oneLiner}
      </p>
    {/if}
    <a
      href={app.url}
      target="_blank"
      rel="noopener noreferrer"
      onclick={onOpen}
      class="btn btn-primary w-full font-medium"
    >
      Open {app.name}
      <ArrowUpRightIcon class="size-4" aria-hidden="true" />
    </a>
  </Elevated>

  <!-- What the app sent, only while there's something to read: each one
       goes to its own place in the app. -->
  {#if app.unread.length > 0}
    <section class="flex flex-col gap-2">
      <div class="flex items-baseline justify-between ps-4 pe-1">
        <h3 class="text-text-tertiary text-[13px] leading-[18px] font-medium">
          Latest
        </h3>
        <button
          type="button"
          onclick={onReadAll}
          class="text-text-tertiary hover:text-text-secondary rounded-md px-1.5 text-[13px] leading-[18px] font-medium transition-colors duration-150"
        >
          Mark all read
        </button>
      </div>
      <Elevated
        offset={1}
        as="ul"
        class="ii-rows flex flex-col overflow-hidden rounded-2xl"
      >
        {#each app.unread as notification (notification.id)}
          <li>
            <a
              href={app.url}
              target="_blank"
              rel="noopener noreferrer"
              onclick={() => onRead?.(notification.id)}
              class="ii-hover group/n focus-visible:ring-focus-ring grid grid-cols-[0.375rem_minmax(0,1fr)_1rem] items-start gap-x-2.5 px-4 py-3 outline-none focus-visible:ring-2 focus-visible:ring-inset"
            >
              <span
                aria-hidden="true"
                class="bg-fg-primary mt-1.5 size-1.5 rounded-full"
              ></span>
              <span class="flex min-w-0 flex-col gap-0.5">
                <span class="flex items-baseline gap-2">
                  <span
                    class="text-text-primary min-w-0 flex-1 truncate text-sm font-medium"
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
              <ArrowUpRightIcon
                aria-hidden="true"
                class="text-fg-quaternary mt-0.5 size-4 opacity-0 transition-opacity duration-150 group-hover/n:opacity-100 group-focus-visible/n:opacity-100"
              />
            </a>
          </li>
        {/each}
      </Elevated>
    </section>
  {/if}

  <!-- Its settings. The switch says on or off itself. -->
  <Elevated
    offset={1}
    as="ul"
    class="divide-border-tertiary flex flex-col divide-y overflow-hidden rounded-2xl"
  >
    <li class={ROW}>
      <span aria-hidden="true" class={ICON}>
        <BellIcon class="size-4.5" />
      </span>
      <span
        id={switchId}
        class="text-text-primary min-w-0 flex-1 text-sm font-medium"
      >
        Notifications
      </span>
      <Toggle
        checked={app.notificationsAllowed}
        onchange={(event) => onToggleNotifications(event.currentTarget.checked)}
        aria-labelledby={switchId}
      />
    </li>
    {#if history}
      <li class={ROW}>
        <span aria-hidden="true" class={ICON}>
          <MonitorSmartphoneIcon class="size-4.5" />
        </span>
        <span class="flex min-w-0 flex-col gap-0.5">
          <span class="text-text-primary text-sm font-medium">
            Last signed in
          </span>
          <span class="text-text-tertiary text-[13px] leading-[18px]">
            {app.lastUsedMillis > 0
              ? `${app.lastBrowser} · ${$formatRelative(
                  new Date(app.lastUsedMillis),
                  { style: "long" },
                )}`
              : "Not yet"}
          </span>
        </span>
      </li>
    {/if}
    {#if app.aiUsedMillis !== undefined}
      <li class={ROW}>
        <span aria-hidden="true" class={ICON}>
          <BotIcon class="size-4.5" />
        </span>
        <span class="flex min-w-0 flex-col gap-0.5">
          <span class="text-text-primary text-sm font-medium">AI usage</span>
          <span class="text-text-tertiary text-[13px] leading-[18px]">
            An AI agent used it {$formatRelative(new Date(app.aiUsedMillis), {
              style: "long",
            })}
          </span>
        </span>
      </li>
    {/if}
  </Elevated>

  <!-- Apart from the settings, with what it does. -->
  <div class="flex flex-col items-start px-4 pt-1">
    <button
      type="button"
      onclick={onRemove}
      class="btn btn-tertiary btn-danger btn-sm -ms-3 font-medium"
    >
      Remove app
    </button>
    <p class="text-text-tertiary text-[13px] leading-[18px]">
      Signs you out. The app stays in Explore.
    </p>
  </div>

  <span class="sr-only" aria-live="polite">{copied ? "Link copied" : ""}</span>
</div>
