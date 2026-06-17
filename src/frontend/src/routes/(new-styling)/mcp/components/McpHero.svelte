<script lang="ts">
  import { GlobeIcon, LockIcon } from "@lucide/svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import McpLogo from "$lib/components/ui/McpLogo.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";

  interface Props {
    /** Hostname of the app the delegation acts as. */
    app: string;
    /** Hostname of the configured MCP server (e.g. mcp.id.ai). */
    mcpServer: string;
  }

  const { app, mcpServer }: Props = $props();

  const apps = getDapps();
  const dapp = $derived(apps.find((a) => a.hasOrigin(`https://${app}`)));
</script>

<!--
  The app and the MCP server as two nodes bridged by a dashed link with a
  centered lock chip (the link is secured by your Internet Identity) — conveys
  "your identity lets this app be acted on by the MCP server" before any copy.
-->
<div class="flex items-start justify-center gap-0 py-5">
  <div class="flex w-24 flex-col items-center gap-2.5">
    <div
      class={[
        "flex size-15 shrink-0 items-center justify-center overflow-hidden rounded-2xl",
        dapp?.logoSrc === undefined &&
          "border-border-secondary text-fg-primary bg-bg-primary border",
      ]}
    >
      {#if dapp?.logoSrc !== undefined}
        <img
          src={dapp.logoSrc}
          alt={`${dapp.name} logo`}
          class="size-15 object-cover"
        />
      {:else}
        <GlobeIcon class="size-6" />
      {/if}
    </div>
    <Badge size="sm" class="max-w-24">
      <Ellipsis text={app} position="middle" />
    </Badge>
  </div>

  <div
    class="relative flex h-15 flex-[0_0_2.75rem] items-center justify-center"
  >
    <div
      class="border-border-secondary absolute inset-x-[-0.5rem] top-1/2 -translate-y-1/2 border-t-2 border-dashed"
      aria-hidden="true"
    ></div>
    <span
      class="border-border-tertiary bg-bg-secondary text-fg-tertiary relative z-[1] flex size-6.5 items-center justify-center rounded-full border"
      aria-hidden="true"
    >
      <LockIcon class="size-3.5" />
    </span>
  </div>

  <div class="flex w-24 flex-col items-center gap-2.5">
    <div
      class="border-border-secondary text-fg-primary bg-bg-primary flex size-15 shrink-0 items-center justify-center rounded-2xl border"
      aria-hidden="true"
    >
      <McpLogo class="size-6.5" />
    </div>
    <Badge size="sm" class="max-w-24">
      <Ellipsis text={mcpServer} position="middle" />
    </Badge>
  </div>
</div>
