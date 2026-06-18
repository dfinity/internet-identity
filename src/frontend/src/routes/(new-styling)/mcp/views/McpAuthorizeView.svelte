<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import McpHero from "../components/McpHero.svelte";
  import { t } from "$lib/stores/locale.store";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";

  interface Props {
    /** Hostname of the app the delegation acts as. */
    app: string;
    /** Hostname of the configured MCP server. */
    mcpServer: string;
    onAuthorize: () => Promise<void>;
  }

  const { app, mcpServer, onAuthorize }: Props = $props();

  let busy = $state(false);

  const handleClick = async () => {
    busy = true;
    try {
      await onAuthorize();
    } finally {
      busy = false;
    }
  };

  // Name the app by its catalogue name when known, otherwise by hostname.
  const dapp = $derived(getDapps().find((a) => a.hasOrigin(`https://${app}`)));
  const accountScope = $derived(
    dapp?.name !== undefined
      ? $t`your ${dapp.name} account`
      : $t`your account on ${app}`,
  );
</script>

<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <McpHero {app} {mcpServer} />

    <h1 class="text-text-primary mt-2 text-2xl font-medium">
      {$t`Allow MCP access`}
    </h1>
    <p class="text-text-tertiary mt-1 text-base text-pretty">
      {$t`Let ${mcpServer} act as ${accountScope}`}
    </p>

    <button
      class="btn btn-primary btn-xl mt-8 w-full"
      onclick={handleClick}
      disabled={busy}
    >
      {#if busy}
        <ProgressRing class="size-5" />
        <span>{$t`Allowing access`}</span>
      {:else}
        <span>{$t`Allow access`}</span>
      {/if}
    </button>
  </AuthPanel>
</div>
