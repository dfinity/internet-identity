<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { channelErrorStore, channelStore } from "$lib/stores/channelStore";
  import ChannelError from "$lib/components/ui/ChannelError.svelte";
  import { goto } from "$app/navigation";

  const pendingChannelOrigin = sessionStorage.getItem(
    "ii-pending-channel-origin",
  );

  const { children }: LayoutProps = $props();

  if (pendingChannelOrigin !== null) {
    channelStore.establish({ allowedOrigin: pendingChannelOrigin });
  }

  $effect(() => {
    if ($channelErrorStore === "unsupported-browser") {
      goto("/unsupported");
    }
  });
</script>

{#if $channelStore !== undefined}
  {@render children()}
{:else if $channelErrorStore !== undefined || pendingChannelOrigin === null}
  <ChannelError error={$channelErrorStore ?? "connection-closed"} />
{/if}
