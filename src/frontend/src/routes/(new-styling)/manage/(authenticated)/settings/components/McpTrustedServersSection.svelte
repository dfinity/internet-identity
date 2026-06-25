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
  import {
    mcpAccessStore,
    isMcpAccessEnabledStore,
  } from "$lib/stores/mcp-access.store";
  import { mcpTrustedServersStore } from "$lib/stores/mcp-trusted-servers.store";
  import { parseMcpServerUrl, probeMcpServer } from "$lib/utils/mcpServer";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();

  // Master switch for MCP on this device. The trusted-server box only shows when
  // it is on, and the connect flow rejects everything when it is off.
  const enabledStore = $derived(isMcpAccessEnabledStore(identityNumber));
  const enabled = $derived($enabledStore);

  // The single trusted MCP server URL for this identity (shared reactively with
  // the connect flow), or undefined when none is set yet.
  const trusted = $derived($mcpTrustedServersStore[identityNumber.toString()]);

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
    const url = $mcpTrustedServersStore[identityNumber.toString()];
    if (url === undefined) {
      return;
    }
    checking = true;
    const ok = await probeMcpServer(url);
    if ($mcpTrustedServersStore[identityNumber.toString()] === url) {
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

  // Verify a server that was already set (and enabled) when the page opens.
  onMount(() => {
    if (enabled && trusted !== undefined) {
      void verify();
    }
  });

  const handleToggle = (event: Event) => {
    if (!(event.currentTarget instanceof HTMLInputElement)) {
      return;
    }
    if (event.currentTarget.checked) {
      mcpAccessStore.enable(identityNumber);
      // Re-check a previously-set server when re-enabling.
      if (trusted !== undefined) {
        void verify({ notify: true });
      }
    } else {
      mcpAccessStore.disable(identityNumber);
    }
  };

  const handleAdd = async () => {
    error = undefined;
    const parsed = parseMcpServerUrl(urlInput);
    if (parsed === undefined) {
      error = $t`Enter a valid https URL (for example https://mcp.example.com/mcp).`;
      return;
    }
    // Activate immediately: verification is advisory, so the server is trusted
    // even if the probe can't confirm it speaks MCP.
    mcpTrustedServersStore.set(identityNumber, parsed.url);
    urlInput = "";
    verified = undefined;
    await verify({ notify: true });
  };

  const handleKeydown = (event: KeyboardEvent) => {
    if (event.key === "Enter" && !checking) {
      event.preventDefault();
      void handleAdd();
    }
  };

  const handleRemove = () => {
    mcpTrustedServersStore.clear(identityNumber);
    verified = undefined;
  };
</script>

<section
  class="border-border-secondary bg-bg-secondary flex flex-col gap-4 rounded-xl border p-5"
>
  <div class="flex flex-row items-start gap-4">
    <span
      class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
      aria-hidden="true"
    >
      <McpIcon class="size-5" />
    </span>

    <div class="flex flex-1 flex-col gap-2">
      <div class="flex min-h-[1.5rem] flex-row items-center gap-2">
        <h3 id={titleId} class="text-text-primary text-base font-semibold">
          {$t`Trusted MCP server`}
        </h3>
        {#if enabled && trusted !== undefined}
          <Badge color="success" size="sm" dot>
            {$t`Enabled`}
          </Badge>
        {/if}
      </div>
      <div
        class="border-fg-warning-primary bg-bg-warning-primary text-fg-warning-primary my-1.5 flex flex-row items-center gap-2 rounded-lg border px-3 py-2 text-sm font-medium"
      >
        <TriangleAlertIcon class="size-4 shrink-0" />
        <span>
          {$t`Internet Identity MCP connectors are in preview. Use this feature at your own risk.`}
        </span>
      </div>
    </div>

    <div class="shrink-0">
      <Toggle
        checked={enabled}
        onchange={handleToggle}
        aria-labelledby={titleId}
      />
    </div>
  </div>

  {#if enabled}
    <p class="text-text-tertiary text-sm">
      <Trans>
        Set the URL of an MCP server you trust. You will be able to add MCP
        connectors to your AI agents that will then act as you across apps.
      </Trans>
    </p>

    {#if trusted !== undefined}
      <div
        class="border-border-secondary bg-bg-primary flex flex-row items-center gap-3 rounded-lg border px-4 py-2.5"
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
      <div class="flex flex-row items-start gap-2">
        <Input
          bind:value={urlInput}
          onkeydown={handleKeydown}
          class="flex-1"
          placeholder="https://mcp.example.com/mcp"
          aria-label={$t`MCP server URL`}
          {error}
          disabled={checking}
          autocomplete="off"
          autocapitalize="off"
          spellcheck={false}
        />
        <Tooltip label={$t`Trust this server`}>
          <button
            class="btn btn-secondary h-11 shrink-0"
            onclick={handleAdd}
            disabled={urlInput.trim() === ""}
          >
            <PlusIcon class="size-5" />
            <span>{$t`Trust this server`}</span>
          </button>
        </Tooltip>
      </div>
    {/if}
  {/if}
</section>
