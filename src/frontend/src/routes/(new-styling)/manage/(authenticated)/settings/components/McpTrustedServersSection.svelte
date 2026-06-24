<script lang="ts">
  import { PlusIcon, Trash2Icon } from "@lucide/svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { mcpTrustedServersStore } from "$lib/stores/mcp-trusted-servers.store";
  import {
    parseMcpServerUrl,
    checkMcpServerReachable,
  } from "$lib/utils/mcpServer";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();

  // Reactive: the connect flow and this list share `mcpTrustedServersStore`.
  const servers = $derived(
    $mcpTrustedServersStore[identityNumber.toString()] ?? [],
  );

  let urlInput = $state("");
  let error = $state<string | undefined>();
  let warning = $state<string | undefined>();
  let checking = $state(false);

  const handleAdd = async () => {
    error = undefined;
    warning = undefined;
    const parsed = parseMcpServerUrl(urlInput);
    if (parsed === undefined) {
      error = $t`Enter a valid https URL (or http://127.0.0.1 for a server you run yourself).`;
      return;
    }
    if (mcpTrustedServersStore.isTrusted(identityNumber, parsed.origin)) {
      error = $t`That server is already on your trusted list.`;
      return;
    }
    // Best-effort reachability: a no-cors probe can't read the response, so a
    // failure is only advisory — we still add the server (the user may know it
    // is correct or temporarily offline) but warn them to double-check.
    checking = true;
    const reachable = await checkMcpServerReachable(urlInput);
    checking = false;
    mcpTrustedServersStore.add(identityNumber, parsed.origin);
    if (!reachable) {
      warning = $t`Added ${parsed.host}, but it didn't respond. Double-check the URL and that the server is running.`;
    }
    urlInput = "";
  };

  const handleKeydown = (event: KeyboardEvent) => {
    if (event.key === "Enter" && !checking) {
      event.preventDefault();
      void handleAdd();
    }
  };

  const handleRemove = (origin: string) => {
    mcpTrustedServersStore.remove(identityNumber, origin);
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

    <div class="flex flex-1 flex-col gap-1">
      <h3 id={titleId} class="text-text-primary text-base font-semibold">
        {$t`Trusted MCP servers`}
      </h3>
      <p class="text-text-tertiary text-sm">
        <Trans>
          Add the MCP servers you trust to sign you in to apps — for example one
          you run yourself. A trusted server can act as you across apps, so only
          add servers you control or fully trust.
        </Trans>
      </p>
    </div>
  </div>

  {#if servers.length > 0}
    <ul class="flex flex-col gap-2" aria-labelledby={titleId}>
      {#each servers as origin (origin)}
        <li
          class="border-border-secondary bg-bg-primary flex flex-row items-center gap-3 rounded-lg border px-4 py-2.5"
        >
          <span class="text-text-primary min-w-0 flex-1 text-sm font-medium">
            <Ellipsis text={origin} position="middle" />
          </span>
          <button
            class="btn btn-tertiary btn-sm btn-icon shrink-0"
            onclick={() => handleRemove(origin)}
            aria-label={$t`Remove ${origin}`}
          >
            <Trash2Icon class="size-5" />
          </button>
        </li>
      {/each}
    </ul>
  {/if}

  <div class="flex flex-row items-start gap-2">
    <Input
      bind:value={urlInput}
      onkeydown={handleKeydown}
      class="flex-1"
      placeholder="https://mcp.example.com/mcp"
      aria-label={$t`MCP server URL`}
      error={error}
      disabled={checking}
      autocomplete="off"
      autocapitalize="off"
      spellcheck={false}
    />
    <button
      class="btn btn-secondary h-11 shrink-0"
      onclick={handleAdd}
      disabled={checking || urlInput.trim() === ""}
    >
      {#if checking}
        <ProgressRing class="size-5" />
      {:else}
        <PlusIcon class="size-5" />
      {/if}
      <span>{$t`Add`}</span>
    </button>
  </div>

  {#if warning !== undefined}
    <p class="text-text-warning_primary text-sm">{warning}</p>
  {/if}
</section>
