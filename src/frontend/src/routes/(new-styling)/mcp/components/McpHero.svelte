<script lang="ts">
  import Badge from "$lib/components/ui/Badge.svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";

  interface Props {
    /** Hostname of the configured MCP server (e.g. mcp.id.ai). */
    mcpServer: string;
  }

  const { mcpServer }: Props = $props();
</script>

<!--
  Single MCP-server node + its hostname badge, mirroring the generic (non-app)
  /cli screen: the connection is whole-session and not scoped to one app, so
  there is no app tile — just the MCP server you're connecting.
-->
<div class="flex flex-1 flex-col items-center justify-center gap-6 py-5">
  <div
    class="border-border-tertiary text-fg-primary bg-bg-primary flex size-20 shrink-0 items-center justify-center rounded-2xl border"
    aria-hidden="true"
  >
    <McpIcon class="size-9" />
  </div>
  <Badge size="sm" class="max-w-[75%]">
    <Ellipsis text={mcpServer} position="middle" />
  </Badge>
</div>
