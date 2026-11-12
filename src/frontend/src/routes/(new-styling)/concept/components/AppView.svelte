<script lang="ts">
  import {
    BellIcon,
    BotIcon,
    ExternalLinkIcon,
    MonitorSmartphoneIcon,
  } from "@lucide/svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { formatRelative } from "$lib/stores/locale.store";
  import AppLogo from "../../manage/(authenticated)/applications/components/AppLogo.svelte";
  import type { ConceptApp } from "../mockData";
  import Elevated from "./Elevated.svelte";
  import PanelHeader from "./PanelHeader.svelte";

  interface Props {
    app: ConceptApp;
    onExit: () => void;
    onToggleNotifications: (allowed: boolean) => void;
    onRemove: () => void;
  }

  const { app, onExit, onToggleNotifications, onRemove }: Props = $props();

  const host = $derived(new URL(app.url).host);
  const switchId = $props.id();
</script>

<div class="flex flex-col gap-4">
  <PanelHeader
    title="App details"
    lead="What this app can do with your identity."
    backLabel="Done"
    onBack={onExit}
  />

  <Elevated offset={2} as="section" class="flex flex-col gap-4 rounded-2xl p-4">
    <div class="flex items-center gap-3.5">
      <AppLogo logo={app.logo} size="lg" />
      <div class="flex min-w-0 flex-col">
        <h3 class="text-text-primary truncate text-sm font-medium">
          {app.name}
        </h3>
        <span class="text-text-tertiary truncate text-[13px] leading-[18px]"
          >{host}</span
        >
      </div>
    </div>
    <a
      href={app.url}
      target="_blank"
      rel="noopener noreferrer"
      class="btn btn-primary w-full font-medium"
    >
      Open app
      <ExternalLinkIcon class="size-4" />
    </a>
  </Elevated>

  <Elevated
    offset={1}
    as="ul"
    class="divide-border-tertiary flex flex-col divide-y overflow-hidden rounded-2xl"
  >
    <li class="flex items-center gap-3 px-4 py-3.5">
      <span
        aria-hidden="true"
        class="text-fg-secondary flex size-9 shrink-0 items-center justify-center rounded-lg bg-(--ii-selected)"
      >
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
    <li class="flex items-center gap-3 px-4 py-3.5">
      <span
        aria-hidden="true"
        class="text-fg-secondary flex size-9 shrink-0 items-center justify-center rounded-lg bg-(--ii-selected)"
      >
        <BotIcon class="size-4.5" />
      </span>
      <span class="flex min-w-0 flex-col gap-0.5">
        <span class="text-text-primary text-sm font-medium">AI usage</span>
        <span
          class={[
            "text-[13px] leading-[18px]",
            app.aiUsedMillis !== undefined
              ? "text-text-primary"
              : "text-text-tertiary",
          ]}
        >
          {#if app.aiUsedMillis !== undefined}
            An AI agent used it {$formatRelative(new Date(app.aiUsedMillis), {
              style: "long",
            })}
          {:else}
            Not used by AI
          {/if}
        </span>
      </span>
    </li>
    <li class="flex items-center gap-3 px-4 py-3.5">
      <span
        aria-hidden="true"
        class="text-fg-secondary flex size-9 shrink-0 items-center justify-center rounded-lg bg-(--ii-selected)"
      >
        <BellIcon class="size-4.5" />
      </span>
      <span class="flex min-w-0 flex-1 flex-col gap-0.5">
        <span id={switchId} class="text-text-primary text-sm font-medium">
          Notifications
        </span>
        <span class="text-text-tertiary text-[13px] leading-[18px]">
          {#if !app.notificationsAllowed}
            Off
          {:else if app.unread.length > 0}
            On · {app.unread.length} unread
          {:else}
            On
          {/if}
        </span>
      </span>
      <Toggle
        checked={app.notificationsAllowed}
        onchange={(event) => onToggleNotifications(event.currentTarget.checked)}
        aria-labelledby={switchId}
      />
    </li>
  </Elevated>

  <div class="flex flex-col items-center gap-1 pt-2">
    <button
      type="button"
      onclick={onRemove}
      class="btn btn-tertiary btn-danger btn-sm font-medium"
    >
      Remove app
    </button>
    <span class="text-text-tertiary text-[13px] leading-[18px]">
      Signs you out and clears its settings.
    </span>
  </div>
</div>
