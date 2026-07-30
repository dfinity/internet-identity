<script lang="ts">
  import Badge from "$lib/components/ui/Badge.svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import McpIcon from "$lib/components/icons/McpIcon.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";

  interface Props {
    /** Hostname of the configured MCP server (e.g. mcp.id.ai). */
    mcpServer: string;
  }

  const { mcpServer }: Props = $props();

  const connectorDots = [
    "opacity-30",
    "opacity-50",
    "opacity-70",
    "opacity-50",
    "opacity-30",
  ];
</script>

<!--
  Internet Identity joined to the MCP server it is being connected to, with its
  hostname below: the connection is whole-session and not scoped to one app, so
  there is no app tile — just the two ends of the connection.
-->
<div class="flex flex-1 flex-col items-center justify-center gap-3 py-2">
  <div class="flex items-center gap-3" aria-hidden="true">
    <div
      class="border-border-tertiary text-fg-primary bg-bg-primary flex size-14 shrink-0 items-center justify-center rounded-xl border"
    >
      <Logo class="h-4" />
    </div>
    <div class="flex items-center gap-1.5">
      {#each connectorDots as dotOpacity, index (index)}
        <span class={["bg-fg-primary size-1 rounded-full", dotOpacity]}></span>
      {/each}
    </div>
    <div
      class="border-border-tertiary text-fg-primary bg-bg-primary flex size-14 shrink-0 items-center justify-center rounded-xl border"
    >
      <McpIcon class="size-6" />
    </div>
  </div>
  <Badge size="sm" class="max-w-[75%]">
    <Ellipsis text={mcpServer} position="middle" />
  </Badge>
</div>
