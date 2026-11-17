<script lang="ts">
  import {
    ArrowUpRightIcon,
    BellIcon,
    BotIcon,
    ExternalLinkIcon,
    LinkIcon,
    MonitorSmartphoneIcon,
  } from "@lucide/svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { formatRelative } from "$lib/stores/locale.store";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { ConceptApp } from "../mockData";
  import Elevated from "./Elevated.svelte";
  import PanelHeader from "./PanelHeader.svelte";

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
  }: Props = $props();

  const host = $derived(new URL(app.url).host);
  const switchId = $props.id();

  const copyLink = () => {
    navigator.clipboard.writeText(app.url).then(
      () => toaster.success({ title: "Link copied", duration: 2500 }),
      () => toaster.create({ title: "Couldn't copy the link", duration: 3000 }),
    );
  };

  const ROW = "flex items-center gap-3 px-4 py-3.5";
  const ICON =
    "text-fg-secondary flex size-9 shrink-0 items-center justify-center rounded-lg bg-(--ii-selected)";
</script>

<div class="flex flex-col gap-4">
  <PanelHeader title={app.name} lead={host} {backLabel} onBack={onExit} />

  <!-- What the app is, and the way into it. -->
  <Elevated offset={2} as="section" class="flex flex-col gap-4 rounded-2xl p-4">
    <div class="flex items-center gap-3.5">
      <AppLogo logo={app.logo} size="lg" />
      <p
        class="text-text-secondary line-clamp-3 min-w-0 flex-1 text-[13px] leading-[18px]"
      >
        {app.oneLiner ?? `Signed in with Internet Identity at ${host}.`}
      </p>
    </div>
    <a
      href={app.url}
      target="_blank"
      rel="noopener noreferrer"
      onclick={onOpen}
      class="btn btn-primary w-full font-medium"
    >
      Open {app.name}
      <ExternalLinkIcon class="size-4" />
    </a>
  </Elevated>

  <!-- What the app sent: each one goes to its own place in the app. -->
  <section class="flex flex-col gap-2">
    <div class="flex items-baseline justify-between ps-4 pe-1">
      <h3 class="text-text-tertiary text-[13px] leading-[18px] font-medium">
        Latest
      </h3>
      {#if app.unread.length > 0}
        <button
          type="button"
          onclick={onReadAll}
          class="text-text-tertiary hover:text-text-secondary rounded-md px-1.5 text-[13px] leading-[18px] font-medium transition-colors duration-150"
        >
          Mark all read
        </button>
      {/if}
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
      {:else}
        <li class="text-text-tertiary px-4 py-3.5 text-center text-[13px]">
          {app.notificationsAllowed
            ? `Nothing new from ${app.name}.`
            : `Notifications are off for ${app.name}.`}
        </li>
      {/each}
    </Elevated>
  </section>

  <Elevated
    offset={1}
    as="ul"
    class="divide-border-tertiary flex flex-col divide-y overflow-hidden rounded-2xl"
  >
    <li class={ROW}>
      <span aria-hidden="true" class={ICON}>
        <BellIcon class="size-4.5" />
      </span>
      <span class="flex min-w-0 flex-1 flex-col gap-0.5">
        <span id={switchId} class="text-text-primary text-sm font-medium">
          Notifications
        </span>
        <span class="text-text-tertiary text-[13px] leading-[18px]">
          {app.notificationsAllowed ? "On" : "Off"}
        </span>
      </span>
      <Toggle
        checked={app.notificationsAllowed}
        onchange={(event) => onToggleNotifications(event.currentTarget.checked)}
        aria-labelledby={switchId}
      />
    </li>
    <li class={ROW}>
      <span aria-hidden="true" class={ICON}>
        <MonitorSmartphoneIcon class="size-4.5" />
      </span>
      <span class="flex min-w-0 flex-col gap-0.5">
        <span class="text-text-primary text-sm font-medium">
          Last signed in
        </span>
        <span class="text-text-tertiary text-[13px] leading-[18px]">
          {app.lastBrowser} · {$formatRelative(new Date(app.lastUsedMillis), {
            style: "long",
          })}
        </span>
      </span>
    </li>
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
    <li>
      <button
        type="button"
        onclick={copyLink}
        class={[
          ROW,
          "ii-hover focus-visible:ring-focus-ring w-full text-start outline-none focus-visible:ring-2 focus-visible:ring-inset",
        ]}
      >
        <span aria-hidden="true" class={ICON}>
          <LinkIcon class="size-4.5" />
        </span>
        <span class="text-text-primary text-sm font-medium">Copy link</span>
      </button>
    </li>
  </Elevated>

  <div class="flex flex-col items-center gap-1 pt-1">
    <button
      type="button"
      onclick={onRemove}
      class="btn btn-tertiary btn-danger btn-sm font-medium"
    >
      Remove app
    </button>
    <span class="text-text-tertiary text-[13px] leading-[18px]">
      Signs you out. It stays in Explore.
    </span>
  </div>
</div>
