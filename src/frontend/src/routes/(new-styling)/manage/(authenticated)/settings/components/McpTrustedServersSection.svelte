<script lang="ts">
  import { invalidateAll } from "$app/navigation";
  import {
    BotIcon,
    MonitorIcon,
    RotateCcwIcon,
    SlidersHorizontalIcon,
    Trash2Icon,
  } from "@lucide/svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import { isLoopbackUrl } from "$lib/utils/mcpServer";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { t } from "$lib/stores/locale.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import type { McpConfig } from "$lib/utils/mcpConfig";
  import { trustedUrl, writeMcpConfig } from "../utils";
  import { backendCanisterConfig } from "$lib/globals";
  import McpAddConnectorDialog from "./McpAddConnectorDialog.svelte";

  interface Props {
    identityNumber: bigint;
    /**
     * The identity's synced MCP config as the route load read it — certified,
     * since `identity_info` is an update call. `undefined` when the identity
     * never wrote one.
     */
    mcpConfig: McpConfig | undefined;
  }

  const { identityNumber, mcpConfig }: Props = $props();
  const titleId = $props.id();

  // The synced (on-chain) MCP config: a master toggle and the custom server
  // URL for this identity. Persisted on-chain (keyed by anchor), so it follows
  // the identity across devices. Derived from the route load so the section
  // renders its real state on first paint; the handlers assign the value they
  // wrote so the UI moves with the click, and `invalidateAll` re-derives this
  // from the canister once the write lands.
  let config = $derived(mcpConfig);
  let showAdd = $state(false);
  let adding = $state(false);

  const official = backendCanisterConfig.mcp_official_url[0];

  const trusted = $derived(config?.url);
  const active = $derived.by(() => {
    const url = trustedUrl(config, backendCanisterConfig);
    return url === undefined
      ? undefined
      : {
          url,
          custom: trusted !== undefined,
          // A local connector is stored port-less, because the program binds a
          // fresh port per sign-in. So it is named by what it is rather than by
          // a host that would read like a remote one.
          local: isLoopbackUrl(url),
        };
  });
  // The switch reports whether a connector is actually trusted, not what the
  // stored `enabled` flag says: on a deployment without an official connector,
  // `{ enabled: true, url: undefined }` trusts nothing and has to read off.
  // `adding` keeps it on while the dialog that supplies the first connector is
  // open, so cancelling puts it back down — the switch is driven by this value
  // alone, so anything that leaves it unchanged leaves the rendered position
  // wherever the browser put it.
  const switchOn = $derived(active !== undefined || adding);

  const hostOf = (url: string): string => {
    try {
      return new URL(url).host;
    } catch {
      return url;
    }
  };

  // Every write states the complete config, so nothing the user didn't choose
  // can ride along into it. On failure the previous value goes back and the
  // user is told; on success the route load re-reads the certified config.
  const write = async (next: McpConfig, error: string): Promise<boolean> => {
    const previous = config;
    config = next;
    try {
      await writeMcpConfig($authenticatedStore.actor, identityNumber, next);
    } catch {
      config = previous;
      toaster.error({ title: error, duration: 4000 });
      return false;
    }
    void invalidateAll();
    return true;
  };

  const handleToggle = async (next: boolean) => {
    // Enabling with nothing to enable — no custom server and no official
    // connector — opens the dialog instead of writing a config that trusts
    // nothing.
    if (
      next &&
      trustedUrl({ enabled: true, url: trusted }, backendCanisterConfig) ===
        undefined
    ) {
      adding = true;
      showAdd = true;
      return;
    }
    // Turning the feature off forgets the custom server too, so re-enabling
    // starts from the official connector rather than silently restoring a
    // server the user last trusted before switching off.
    await write(
      next
        ? { enabled: true, url: trusted }
        : { enabled: false, url: undefined },
      $t`Couldn't save your change. Please try again.`,
    );
  };

  const handleAddClose = () => {
    showAdd = false;
    adding = false;
  };

  const handleAddSave = async (url: string) => {
    const saved = await write(
      { enabled: true, url },
      $t`Couldn't save your connector. Please try again.`,
    );
    if (saved) {
      showAdd = false;
      adding = false;
    }
  };

  const handleRestoreDefault = async () => {
    if (trusted === undefined) return;
    // Without an official connector to fall back to, dropping the custom one
    // would leave the feature on with nothing trusted, so turn it off instead.
    const fallsBack = official !== undefined;
    await write(
      { enabled: fallsBack, url: undefined },
      $t`Couldn't remove the connector. Please try again.`,
    );
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
      <Toggle
        checked={switchOn}
        onchange={() => handleToggle(!switchOn)}
        aria-labelledby={titleId}
      />
    </div>
  </div>

  {#if active !== undefined}
    <div class="border-border-tertiary mt-5 border-t pt-4">
      <p class="text-text-tertiary mb-3 text-xs font-semibold">
        {$t`Trusted connector`}
      </p>

      <div
        class="border-border-tertiary bg-bg-primary flex flex-row items-center gap-3 rounded-lg border px-3 py-3 sm:px-4"
      >
        <span
          class="border-border-secondary bg-bg-secondary text-fg-tertiary flex size-10 shrink-0 items-center justify-center rounded-md border"
          aria-hidden="true"
        >
          {#if active.local}
            <MonitorIcon class="size-4.5" />
          {:else}
            <McpIcon class="size-4.5" />
          {/if}
        </span>

        <div class="flex min-w-0 flex-1 flex-col gap-1">
          <span class="text-text-primary truncate text-sm font-semibold">
            {#if active.local}
              {$t`Local server`}
            {:else if active.custom}
              {hostOf(active.url)}
            {:else}
              {$t`Internet Computer MCP`}
            {/if}
          </span>
          <span class="text-text-secondary text-sm">
            {#if !active.custom}
              {$t`Official · Hosted by DFINITY`}
            {:else if active.local}
              {$t`Runs on your own computer · Any port`}
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
    </div>
  {/if}
</section>

{#if showAdd}
  <McpAddConnectorDialog onClose={handleAddClose} onSave={handleAddSave} />
{/if}
