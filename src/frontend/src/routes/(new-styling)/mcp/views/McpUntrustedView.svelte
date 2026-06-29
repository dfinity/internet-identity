<script lang="ts">
  import { ExternalLinkIcon, ShieldAlertIcon } from "@lucide/svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Host of the MCP server the request wants to connect (e.g. mcp.id.ai). */
    mcpServerHost: string;
    /** Authenticate here, then open Settings in a new tab carrying the session
     *  (so the user doesn't have to sign in again to set the trusted server). */
    onManageTrustedServer: () => void;
    /** True while the sign-in ceremony for the handoff is in flight. */
    busy: boolean;
  }

  const { mcpServerHost, onManageTrustedServer, busy }: Props = $props();
</script>

<!--
  Shown when the request's callback origin doesn't match the chosen identity's
  trusted MCP server. Each user sets the one MCP server they trust, so we point
  them at Settings to set this one rather than silently connecting it.
-->
<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <FeaturedIcon size="lg" variant="warning" class="mb-5 self-start">
      <ShieldAlertIcon class="size-6" />
    </FeaturedIcon>

    <h1 class="text-text-primary text-2xl font-medium">
      {$t`This MCP server isn't trusted yet`}
    </h1>
    <div class="mt-4 self-start">
      <Badge size="sm" class="max-w-full">
        <Ellipsis text={mcpServerHost} position="middle" />
      </Badge>
    </div>
    <p class="text-text-tertiary mt-4 text-base text-pretty">
      <Trans>
        For security, Internet Identity only connects the MCP server you've set
        as trusted. Set this server in Settings, then try again.
      </Trans>
    </p>

    <button
      onclick={onManageTrustedServer}
      disabled={busy}
      class="btn btn-secondary btn-xl mt-8 w-full"
    >
      {#if busy}
        <ProgressRing class="size-5" />
      {:else}
        <ExternalLinkIcon class="size-5" />
      {/if}
      <span>{$t`Manage trusted server`}</span>
    </button>
  </AuthPanel>
</div>
