<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import McpHero from "../components/McpHero.svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Hostname of the MCP server being connected. */
    mcpServer: string;
  }

  const { mcpServer }: Props = $props();
</script>

<!--
  Shown after the user confirms (and has authenticated) while the registration
  delegation is minted — i.e. between "Allow access" and the tab being handed
  to the trusted server. Gives the otherwise-blank gap a loading state, like
  the /authorize redirect screen.
-->
<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <McpHero {mcpServer} />
    <div class="mt-2 flex flex-col items-center gap-3">
      <ProgressRing class="text-fg-tertiary size-8" />
      <p class="text-text-primary text-base font-medium">
        {$t`Connecting ${mcpServer}…`}
      </p>
      <p class="text-text-tertiary text-sm text-pretty">
        {$t`Securely connecting your identity to the server.`}
      </p>
    </div>
  </AuthPanel>
</div>
