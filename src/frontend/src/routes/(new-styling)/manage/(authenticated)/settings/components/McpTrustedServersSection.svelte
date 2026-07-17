<script lang="ts">
  import { onMount } from "svelte";
  import { GlobeIcon, Trash2Icon } from "@lucide/svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { t } from "$lib/stores/locale.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import {
    readMcpConfig,
    setMcpEnabled,
    setMcpTrustedServer,
    trustAndEnableMcp,
    clearAndDisableMcp,
  } from "$lib/utils/mcpConfig";
  import McpAddConnectorDialog from "./McpAddConnectorDialog.svelte";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();

  // The synced (on-chain) MCP config: a master toggle and the single trusted
  // server URL for this identity. Persisted on-chain (keyed by anchor), so it
  // follows the identity across devices. Read once on mount and kept in local
  // state that the handlers update after each canister write.
  let enabled = $state(false);
  let trusted = $state<string | undefined>(undefined);
  // True until the initial config read completes, so the toggle doesn't flicker
  // off-then-on and writes can't race the load.
  let loaded = $state(false);
  let saving = $state(false);

  let showAdd = $state(false);
  let enableOnSave = $state(false);

  const hostOf = (url: string): string => {
    try {
      return new URL(url).host;
    } catch {
      return url;
    }
  };

  onMount(() => {
    void (async () => {
      try {
        const config = await readMcpConfig(
          $authenticatedStore.actor,
          identityNumber,
        );
        enabled = config.enabled;
        trusted = config.url;
      } catch {
        toaster.error({
          title: $t`Couldn't load your trusted MCP server settings.`,
          duration: 4000,
        });
      } finally {
        loaded = true;
      }
    })();
  });

  const handleToggle = async () => {
    const next = enabled;
    if (next && trusted === undefined) {
      enabled = false;
      enableOnSave = true;
      showAdd = true;
      return;
    }
    try {
      await setMcpEnabled($authenticatedStore.actor, identityNumber, next);
      toaster.info({
        title: next ? $t`AI access is on.` : $t`AI access is off.`,
        duration: 4000,
      });
    } catch {
      enabled = !next;
      toaster.error({
        title: $t`Couldn't save your change. Please try again.`,
        duration: 4000,
      });
    }
  };

  const openAdd = () => {
    enableOnSave = !enabled;
    showAdd = true;
  };

  const handleAddClose = () => {
    showAdd = false;
    enableOnSave = false;
  };

  const handleAddSave = async (url: string) => {
    const enableNow = enableOnSave;
    saving = true;
    try {
      if (enableNow) {
        await trustAndEnableMcp($authenticatedStore.actor, identityNumber, url);
        enabled = true;
      } else {
        await setMcpTrustedServer(
          $authenticatedStore.actor,
          identityNumber,
          url,
        );
      }
      trusted = url;
      showAdd = false;
      enableOnSave = false;
      toaster.success({
        title: enableNow
          ? $t`You're all set. AI access is on with ${hostOf(url)}.`
          : $t`${hostOf(url)} has been added.`,
        duration: 4000,
      });
    } catch {
      toaster.error({
        title: $t`Couldn't save your trusted server. Please try again.`,
        duration: 4000,
      });
    } finally {
      saving = false;
    }
  };

  const handleRemove = async () => {
    if (trusted === undefined) return;
    const previousUrl = trusted;
    const previousHost = hostOf(trusted);
    const previousEnabled = enabled;
    trusted = undefined;
    enabled = false;
    try {
      await clearAndDisableMcp($authenticatedStore.actor, identityNumber);
      toaster.info({
        title: $t`${previousHost} was removed. AI access is now off.`,
        duration: 4000,
      });
    } catch {
      trusted = previousUrl;
      enabled = previousEnabled;
      toaster.error({
        title: $t`Couldn't remove the server. Please try again.`,
        duration: 4000,
      });
    }
  };
</script>

<section
  class="border-border-secondary bg-bg-secondary flex flex-col rounded-xl border p-4 sm:p-5"
>
  <div class="flex flex-row items-start gap-3 sm:gap-4">
    <span
      class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-12 shrink-0 items-center justify-center rounded-lg border"
      aria-hidden="true"
    >
      <McpIcon class="size-5.5" />
    </span>

    <div class="flex min-w-0 flex-1 flex-col gap-1">
      <div
        class="flex min-h-[1.5rem] flex-row flex-wrap items-center gap-x-2 gap-y-1"
      >
        <h3 id={titleId} class="text-text-primary text-base font-semibold">
          {$t`AI access`}
        </h3>
        <Badge color="surface" size="sm">
          {$t`Preview`}
        </Badge>
        {#if enabled && trusted !== undefined}
          <Badge color="success" size="sm" dot>
            {$t`Enabled on all devices`}
          </Badge>
        {/if}
      </div>
      <p class="text-text-tertiary text-sm">
        {$t`Ask questions and perform actions across your apps by chatting with AI.`}
      </p>
    </div>

    <div class="flex h-6 shrink-0 items-center">
      {#if loaded}
        <Toggle
          bind:checked={enabled}
          onchange={handleToggle}
          aria-labelledby={titleId}
        />
      {:else}
        <ProgressRing class="text-fg-tertiary size-5" />
      {/if}
    </div>
  </div>

  {#if enabled}
    <div class="border-border-tertiary mt-5 border-t pt-4">
      <p class="text-text-tertiary mb-3 text-xs font-semibold">
        {$t`Trusted connectors`}
      </p>

      {#if trusted !== undefined}
        <div
          class="border-border-tertiary bg-bg-primary flex flex-row items-center gap-3 rounded-lg border px-3 py-3 sm:px-4"
        >
          <span
            class="border-border-secondary bg-bg-secondary text-fg-tertiary flex size-10 shrink-0 items-center justify-center rounded-md border"
            aria-hidden="true"
          >
            <GlobeIcon class="size-4.5" />
          </span>

          <div class="flex min-w-0 flex-1 flex-col gap-1.5">
            <span class="text-text-primary truncate text-sm font-semibold">
              {hostOf(trusted)}
            </span>
            <span
              class="text-text-tertiary truncate font-mono text-xs"
              title={trusted}
            >
              {trusted}
            </span>
          </div>

          <Tooltip label={$t`Remove`}>
            <button
              class="btn btn-tertiary btn-sm btn-icon shrink-0"
              onclick={handleRemove}
              aria-label={$t`Remove this server`}
            >
              <Trash2Icon class="size-4.5" />
            </button>
          </Tooltip>
        </div>
      {:else}
        <button
          class="btn btn-secondary btn-sm w-full sm:w-auto"
          onclick={openAdd}
          disabled={saving}
        >
          {$t`Add connector`}
        </button>
      {/if}
    </div>
  {/if}
</section>

{#if showAdd}
  <McpAddConnectorDialog
    onClose={handleAddClose}
    onSave={handleAddSave}
    {saving}
  />
{/if}
