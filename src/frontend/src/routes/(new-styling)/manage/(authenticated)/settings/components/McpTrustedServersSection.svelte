<script lang="ts">
  import { onMount } from "svelte";
  import {
    PlusIcon,
    XIcon,
    RotateCwIcon,
    TriangleAlertIcon,
  } from "@lucide/svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import {
    readMcpConfig,
    setMcpEnabled,
    setMcpTrustedServer,
    clearMcpTrustedServer,
  } from "$lib/utils/mcpConfig";
  import { parseMcpServerUrl, probeMcpServer } from "$lib/utils/mcpServer";

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
  // A canister write (toggle / add / remove) is in flight.
  let saving = $state(false);

  let urlInput = $state("");
  let error = $state<string | undefined>();
  // Live MCP status of the trusted server: undefined while unknown / in flight,
  // true when it verified as MCP, false when it couldn't be.
  let verified = $state<boolean | undefined>(undefined);
  let checking = $state(false);

  const hostOf = (url: string): string => {
    try {
      return new URL(url).host;
    } catch {
      return url;
    }
  };

  // Probe the trusted server for a real MCP endpoint. Best-effort and advisory:
  // a server we can't reach/read (e.g. CORS) shows as unverified but stays
  // active. Guard against a stale result if the entry changed while in flight.
  // `notify` surfaces a success toast for explicit checks (add / re-check), not
  // the silent check on page load.
  const verify = async (options?: { notify?: boolean }) => {
    const url = trusted;
    if (url === undefined) {
      return;
    }
    checking = true;
    const ok = await probeMcpServer(url);
    if (trusted === url) {
      verified = ok;
      if (ok && options?.notify === true) {
        toaster.success({
          title: $t`Verified ${hostOf(url)} as an MCP server.`,
          duration: 4000,
        });
      }
    }
    checking = false;
  };

  // Load the synced config when the section opens, then verify a server that is
  // already set (and enabled).
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
      if (enabled && trusted !== undefined) {
        void verify();
      }
    })();
  });

  const handleToggle = async (event: Event) => {
    if (!(event.currentTarget instanceof HTMLInputElement)) {
      return;
    }
    const next = event.currentTarget.checked;
    const previous = enabled;
    enabled = next; // optimistic; reverted below if the write fails
    saving = true;
    try {
      await setMcpEnabled($authenticatedStore.actor, identityNumber, next);
      // Re-check a previously-set server when re-enabling.
      if (next && trusted !== undefined) {
        void verify({ notify: true });
      }
    } catch {
      enabled = previous;
      toaster.error({
        title: $t`Couldn't save your change. Please try again.`,
        duration: 4000,
      });
    } finally {
      saving = false;
    }
  };

  const handleAdd = async () => {
    error = undefined;
    const parsed = parseMcpServerUrl(urlInput);
    if (parsed === undefined) {
      error = $t`Enter a valid https URL (for example https://mcp.example.com/mcp).`;
      return;
    }
    saving = true;
    try {
      await setMcpTrustedServer(
        $authenticatedStore.actor,
        identityNumber,
        parsed.url,
      );
      // Activate (verification is advisory): the server is trusted even if the
      // probe can't confirm it speaks MCP.
      trusted = parsed.url;
      urlInput = "";
      verified = undefined;
      await verify({ notify: true });
    } catch {
      toaster.error({
        title: $t`Couldn't save your trusted server. Please try again.`,
        duration: 4000,
      });
    } finally {
      saving = false;
    }
  };

  const handleKeydown = (event: KeyboardEvent) => {
    if (event.key === "Enter" && !checking && !saving) {
      event.preventDefault();
      void handleAdd();
    }
  };

  const handleRemove = async () => {
    saving = true;
    try {
      await clearMcpTrustedServer($authenticatedStore.actor, identityNumber);
      trusted = undefined;
      verified = undefined;
    } catch {
      toaster.error({
        title: $t`Couldn't remove the server. Please try again.`,
        duration: 4000,
      });
    } finally {
      saving = false;
    }
  };
</script>

<section
  class="border-border-secondary bg-bg-secondary flex flex-col gap-4 rounded-xl border p-4 sm:p-5"
>
  <div class="flex flex-row items-start gap-3 sm:gap-4">
    <span
      class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
      aria-hidden="true"
    >
      <McpIcon class="size-5" />
    </span>

    <div
      class="flex min-h-[2.5rem] min-w-0 flex-1 flex-row flex-wrap items-center gap-x-2 gap-y-1"
    >
      <h3 id={titleId} class="text-text-primary text-base font-semibold">
        {$t`Trusted MCP server`}
      </h3>
      {#if enabled && trusted !== undefined}
        <Badge color="success" size="sm" dot>
          {$t`Enabled on all your devices`}
        </Badge>
      {/if}
    </div>

    <div class="flex h-10 shrink-0 items-center">
      {#if loaded}
        <Toggle
          checked={enabled}
          onchange={handleToggle}
          disabled={saving}
          aria-labelledby={titleId}
        />
      {:else}
        <ProgressRing class="text-fg-tertiary size-5" />
      {/if}
    </div>
  </div>

  <div
    class="border-fg-warning-primary bg-bg-warning-primary text-fg-warning-primary flex flex-row items-center gap-2 rounded-lg border px-3 py-2 text-sm font-medium"
  >
    <TriangleAlertIcon class="size-4 shrink-0" />
    <span>
      {$t`Internet Identity MCP connectors are in preview. Use this feature at your own risk.`}
    </span>
  </div>

  {#if enabled}
    <p class="text-text-tertiary text-sm">
      <Trans>
        Set the URL of an MCP server you trust. You will be able to add MCP
        connectors to your AI agents that will then act on your behalf across
        apps. This setting syncs across your devices.
      </Trans>
    </p>

    {#if trusted !== undefined}
      <div
        class="border-border-secondary bg-bg-primary flex flex-row items-center gap-2 rounded-lg border px-3 py-2.5 sm:gap-3 sm:px-4"
      >
        <span class="text-text-primary min-w-0 flex-1 text-sm font-medium">
          <Ellipsis text={trusted} position="middle" />
        </span>

        {#if !checking}
          <Tooltip
            label={verified === true
              ? $t`Verified MCP server`
              : verified === false
                ? $t`Couldn't verify this server`
                : $t`Not checked yet`}
          >
            <span
              class={[
                "size-2.5 shrink-0 rounded-full",
                verified === true
                  ? "bg-fg-success-primary"
                  : verified === false
                    ? "bg-fg-error-primary"
                    : "bg-fg-quaternary",
              ]}
            ></span>
          </Tooltip>
        {/if}

        <Tooltip label={$t`Re-check connection`}>
          <button
            class="btn btn-tertiary btn-sm btn-icon shrink-0"
            onclick={() => void verify({ notify: true })}
            disabled={checking}
            aria-label={$t`Re-check connection`}
          >
            {#if checking}
              <ProgressRing class="size-5" />
            {:else}
              <RotateCwIcon class="size-5" />
            {/if}
          </button>
        </Tooltip>

        <Tooltip label={$t`Remove this server`}>
          <button
            class="btn btn-tertiary btn-sm btn-icon shrink-0"
            onclick={handleRemove}
            disabled={saving}
            aria-label={$t`Remove this server`}
          >
            <XIcon class="size-5" />
          </button>
        </Tooltip>
      </div>

      {#if verified === false && !checking}
        <p class="text-text-tertiary text-sm">
          {$t`Couldn't verify an MCP server at this URL. Please check the URL and that the server is running and retry.`}
        </p>
      {/if}
    {:else}
      <div class="flex flex-col gap-2 sm:flex-row sm:items-start">
        <Input
          bind:value={urlInput}
          onkeydown={handleKeydown}
          class="w-full min-w-0 sm:flex-1"
          placeholder="https://mcp.example.com/mcp"
          aria-label={$t`MCP server URL`}
          {error}
          disabled={saving}
          autocomplete="off"
          autocapitalize="off"
          spellcheck={false}
        />
        <button
          class="btn btn-secondary h-11 w-full sm:w-auto sm:shrink-0"
          onclick={handleAdd}
          disabled={urlInput.trim() === "" || saving}
        >
          {#if saving}
            <ProgressRing class="size-5" />
          {:else}
            <PlusIcon class="size-5" />
          {/if}
          <span>{$t`Trust this server`}</span>
        </button>
      </div>
    {/if}
  {/if}
</section>
