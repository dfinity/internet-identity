<script lang="ts">
  import { onMount } from "svelte";
  import {
    PlusIcon,
    Trash2Icon,
    RotateCwIcon,
    TriangleAlertIcon,
  } from "@lucide/svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { mcpTrustedServersStore } from "$lib/stores/mcp-trusted-servers.store";
  import { parseMcpServerUrl, probeMcpServer } from "$lib/utils/mcpServer";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();

  // The single trusted MCP server URL for this identity (shared reactively with
  // the connect flow), or undefined when none is set yet.
  const trusted = $derived($mcpTrustedServersStore[identityNumber.toString()]);

  let urlInput = $state("");
  let error = $state<string | undefined>();
  // Live MCP-handshake status of the trusted server: undefined while unknown /
  // in flight, true when it answered as MCP, false when it couldn't be verified.
  let verified = $state<boolean | undefined>(undefined);
  let checking = $state(false);

  // Probe the trusted server with a real MCP `initialize` handshake. Best-effort
  // and advisory: a server we can't reach/read (e.g. CORS) shows as unverified
  // but stays active. Guard against a stale result if the entry changed while
  // the probe was in flight.
  const verify = async () => {
    const url = $mcpTrustedServersStore[identityNumber.toString()];
    if (url === undefined) {
      return;
    }
    checking = true;
    const ok = await probeMcpServer(url);
    if ($mcpTrustedServersStore[identityNumber.toString()] === url) {
      verified = ok;
    }
    checking = false;
  };

  // Verify a server that was already set when the page opens.
  onMount(() => {
    if (trusted !== undefined) {
      void verify();
    }
  });

  const handleAdd = async () => {
    error = undefined;
    const parsed = parseMcpServerUrl(urlInput);
    if (parsed === undefined) {
      error = $t`Enter a valid https URL (for example https://mcp.example.com/mcp).`;
      return;
    }
    // Activate immediately (like before): verification is advisory, so the
    // server is trusted even if the probe can't confirm it speaks MCP.
    mcpTrustedServersStore.set(identityNumber, parsed.url);
    urlInput = "";
    verified = undefined;
    await verify();
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
      <h3 id={titleId} class="text-text-primary text-base font-semibold">
        {$t`Trusted MCP server`}
      </h3>
      <div
        class="border-fg-warning-primary bg-bg-warning-primary text-fg-warning-primary flex flex-row items-center gap-2 rounded-lg border px-3 py-1.5 text-sm font-medium"
      >
        <TriangleAlertIcon class="size-4 shrink-0" />
        <span>
          {$t`Internet Identity MCP connectors are in preview. Use this feature at your own risk.`}
        </span>
      </div>
      <p class="text-text-tertiary text-sm">
        <Trans>
          Set the URL of an MCP server you trust. You will be able to add MCP
          connectors to your AI agents that will then act as you across apps.
        </Trans>
      </p>
    </div>
  </div>

  {#if trusted !== undefined}
    <div
      class={[
        "bg-bg-primary flex flex-row items-center gap-3 rounded-lg border px-4 py-2.5",
        verified === false ? "border-border-error" : "border-border-secondary",
      ]}
    >
      <span
        class={[
          "min-w-0 flex-1 text-sm font-medium",
          verified === false ? "text-text-error" : "text-text-primary",
        ]}
      >
        <Ellipsis text={trusted} position="middle" />
      </span>
      <button
        class="btn btn-tertiary btn-sm btn-icon shrink-0"
        onclick={() => void verify()}
        disabled={checking}
        aria-label={$t`Re-check ${trusted}`}
      >
        {#if checking}
          <ProgressRing class="size-5" />
        {:else}
          <RotateCwIcon class="size-5" />
        {/if}
      </button>
      <button
        class="btn btn-tertiary btn-sm btn-icon shrink-0"
        onclick={handleRemove}
        aria-label={$t`Remove ${trusted}`}
      >
        <Trash2Icon class="size-5" />
      </button>
    </div>

    {#if verified === false && !checking}
      <p class="text-text-error text-sm">
        {$t`Couldn't verify an MCP server at this URL. It stays active — check the URL and that the server is running, then re-check.`}
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
      <button
        class="btn btn-secondary h-11 shrink-0"
        onclick={handleAdd}
        disabled={urlInput.trim() === ""}
      >
        <PlusIcon class="size-5" />
        <span>{$t`Add`}</span>
      </button>
    </div>
  {/if}
</section>
