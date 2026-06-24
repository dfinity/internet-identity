<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import ContinueView from "../../authorize/views/ContinueView.svelte";
  import McpHero from "../components/McpHero.svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Hostname of the MCP server (display, e.g. mcp.id.ai). */
    mcpServerHost: string;
    /** Origin used for canister account calls (gateway origins remapped). */
    effectiveOrigin: string;
    /** The MCP server origin, shown to the user and used for the app lookup. */
    displayOrigin: string;
    /** Receives the chosen account (resolved after sign-in) and connects. */
    onAuthorize: (accountNumber: Promise<bigint | undefined>) => void;
  }

  const { mcpServerHost, effectiveOrigin, displayOrigin, onAuthorize }: Props =
    $props();
</script>

<!--
  The MCP connect screen reuses the regular continue-with-ii account picker
  (multi-account toggle, add/edit account, account list) so the user binds a
  specific, stable account — the same principal that enable and disable derive.
  The MCP-specific consent (which server, what it can do, and for how long)
  replaces the picker's default app header via the `header` snippet.
-->
<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <ContinueView
      {effectiveOrigin}
      {displayOrigin}
      {onAuthorize}
      continueLabel={$t`Allow access`}
    >
      {#snippet header()}
        <McpHero mcpServer={mcpServerHost} />
        <h1 class="text-text-primary mt-2 text-2xl font-medium">
          {$t`Connect ${mcpServerHost}`}
        </h1>
        <p class="text-text-tertiary mt-1 mb-6 text-base text-pretty">
          {$t`Let ${mcpServerHost} act as you across your apps for this session. Access lasts about an hour, then you'll need to reconnect.`}
        </p>
      {/snippet}
    </ContinueView>
  </AuthPanel>
</div>
