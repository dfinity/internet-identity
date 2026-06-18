<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import McpHero from "../components/McpHero.svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Hostname of the configured MCP server (the app the standing delegation
     *  acts as, and the relying party for this whole-session grant). */
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
</script>

<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <McpHero {app} {mcpServer} />

    <h1 class="text-text-primary mt-2 text-2xl font-medium">
      {$t`Connect ${mcpServer}`}
    </h1>
    <p class="text-text-tertiary mt-1 text-base text-pretty">
      {$t`Let ${mcpServer} act as you across your apps for this session. Access lasts about an hour, then you'll need to reconnect.`}
    </p>

    <button
      class="btn btn-primary btn-xl mt-8 w-full"
      onclick={handleClick}
      disabled={busy}
    >
      {#if busy}
        <ProgressRing class="size-5" />
        <span>{$t`Connecting`}</span>
      {:else}
        <span>{$t`Allow access`}</span>
      {/if}
    </button>
  </AuthPanel>
</div>
