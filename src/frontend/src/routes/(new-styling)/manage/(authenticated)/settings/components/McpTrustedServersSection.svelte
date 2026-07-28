<script lang="ts">
  import { onMount } from "svelte";
  import {
    BotIcon,
    PlusIcon,
    RotateCcwIcon,
    SlidersHorizontalIcon,
    Trash2Icon,
  } from "@lucide/svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { t } from "$lib/stores/locale.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import {
    readMcpConfig,
    setMcpEnabled,
    trustAndEnableMcp,
    clearMcpTrustedServer,
    type McpConfig,
  } from "$lib/utils/mcpConfig";
  import { trustedUrl } from "../utils";
  import { backendCanisterConfig, officialMcpUrl } from "$lib/globals";
  import McpAddConnectorDialog from "./McpAddConnectorDialog.svelte";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();

  // The synced (on-chain) MCP config: a master toggle and the custom server
  // URL for this identity. Persisted on-chain (keyed by anchor), so it follows
  // the identity across devices. Read once on mount and kept in local state
  // that the handlers update after each canister write.
  let config = $state<McpConfig | undefined>(undefined);
  // True until the initial config read completes, so the toggle doesn't flicker
  // off-then-on and writes can't race the load.
  let loaded = $state(false);
  let showAdd = $state(false);

  const official = officialMcpUrl();

  const enabled = $derived(config?.enabled ?? false);
  const trusted = $derived(config?.url);
  const active = $derived.by(() => {
    const url = trustedUrl(config, backendCanisterConfig);
    return url === undefined
      ? undefined
      : { url, custom: trusted !== undefined };
  });

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
        config = await readMcpConfig($authenticatedStore.actor, identityNumber);
      } catch {
        toaster.error({
          title: $t`Couldn't load your AI access settings.`,
          duration: 4000,
        });
      } finally {
        loaded = true;
      }
    })();
  });

  const handleToggle = async (next: boolean) => {
    // Enabling with nothing to enable — no custom server and no official
    // connector — opens the dialog instead of writing a config that trusts
    // nothing.
    if (
      next &&
      trustedUrl({ enabled: true, url: trusted }, backendCanisterConfig) ===
        undefined
    ) {
      showAdd = true;
      return;
    }
    const previous = config;
    config = next
      ? { enabled: true, url: trusted }
      : { enabled: false, url: undefined };
    try {
      await setMcpEnabled($authenticatedStore.actor, identityNumber, next);
    } catch {
      config = previous;
      toaster.error({
        title: $t`Couldn't save your change. Please try again.`,
        duration: 4000,
      });
    }
  };

  const handleAddClose = () => {
    showAdd = false;
  };

  const handleAddSave = async (url: string) => {
    try {
      await trustAndEnableMcp($authenticatedStore.actor, identityNumber, url);
      config = { enabled: true, url };
      showAdd = false;
    } catch {
      toaster.error({
        title: $t`Couldn't save your connector. Please try again.`,
        duration: 4000,
      });
    }
  };

  const handleRestoreDefault = async () => {
    if (trusted === undefined) return;
    const previous = config;
    // Without an official connector to fall back to, dropping the custom one
    // would leave the feature on with nothing trusted, so turn it off instead.
    const fallsBack = official !== undefined;
    config = { enabled: fallsBack, url: undefined };
    try {
      if (fallsBack) {
        await clearMcpTrustedServer($authenticatedStore.actor, identityNumber);
      } else {
        await setMcpEnabled($authenticatedStore.actor, identityNumber, false);
      }
    } catch {
      config = previous;
      toaster.error({
        title: $t`Couldn't remove the connector. Please try again.`,
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
      class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
      aria-hidden="true"
    >
      <BotIcon class="size-5" />
    </span>

    <div class="flex min-w-0 flex-1 flex-col gap-1">
      <div
        class="flex min-h-[1.5rem] flex-row flex-wrap items-center gap-x-2 gap-y-1"
      >
        <h3 id={titleId} class="text-text-primary text-base font-semibold">
          {$t`AI access`}
        </h3>
        {#if active !== undefined}
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
          checked={enabled}
          onchange={() => handleToggle(!enabled)}
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
        {$t`Trusted connector`}
      </p>

      {#if active !== undefined}
        <div
          class="border-border-tertiary bg-bg-primary flex flex-row items-center gap-3 rounded-lg border px-3 py-3 sm:px-4"
        >
          <span
            class="border-border-secondary bg-bg-secondary text-fg-tertiary flex size-10 shrink-0 items-center justify-center rounded-md border"
            aria-hidden="true"
          >
            <McpIcon class="size-4.5" />
          </span>

          <div class="flex min-w-0 flex-1 flex-col gap-1">
            <span class="text-text-primary truncate text-sm font-semibold">
              {active.custom ? hostOf(active.url) : $t`Internet Computer MCP`}
            </span>
            <span class="text-text-secondary text-sm">
              {#if !active.custom}
                {$t`Official · by DFINITY`}
              {:else if official !== undefined}
                {$t`Added by you · Replaces the official connector`}
              {:else}
                {$t`Added by you`}
              {/if}
            </span>
            <span
              class="text-text-tertiary truncate font-mono text-xs"
              title={active.url}
            >
              {active.url}
            </span>
          </div>

          {#if active.custom}
            <button
              class="btn btn-secondary btn-sm shrink-0 gap-2"
              onclick={handleRestoreDefault}
            >
              {#if official !== undefined}
                <RotateCcwIcon class="size-4" />
                {$t`Restore default`}
              {:else}
                <Trash2Icon class="size-4" />
                {$t`Remove`}
              {/if}
            </button>
          {:else}
            <button
              class="btn btn-secondary btn-sm shrink-0 gap-2"
              onclick={() => (showAdd = true)}
            >
              <SlidersHorizontalIcon class="size-4" />
              {$t`Customize`}
            </button>
          {/if}
        </div>
      {:else}
        <button
          class="btn btn-secondary btn-sm gap-2"
          onclick={() => (showAdd = true)}
        >
          <PlusIcon class="size-4" />
          {$t`Add connector`}
        </button>
      {/if}
    </div>
  {/if}
</section>

{#if showAdd}
  <McpAddConnectorDialog onClose={handleAddClose} onSave={handleAddSave} />
{/if}
