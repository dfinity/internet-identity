<script lang="ts">
  import { ExternalLinkIcon, ShieldAlertIcon } from "@lucide/svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Host of the MCP server the request wants to connect (e.g. mcp.id.ai). */
    mcpServerHost: string;
  }

  const { mcpServerHost }: Props = $props();
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

    <a
      href="/manage/settings"
      target="_blank"
      rel="noopener noreferrer"
      class="btn btn-secondary btn-xl mt-8 w-full"
    >
      <ExternalLinkIcon class="size-5" />
      <span>{$t`Manage trusted server`}</span>
    </a>
  </AuthPanel>
</div>
