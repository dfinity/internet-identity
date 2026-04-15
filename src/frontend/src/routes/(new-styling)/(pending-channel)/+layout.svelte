<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { channelErrorStore, channelStore } from "$lib/stores/channelStore";
  import { goto } from "$app/navigation";
  import ChannelError from "$lib/components/ui/ChannelError.svelte";

  const { children }: LayoutProps = $props();
  channelStore.establish({ pending: true });

  $effect(() => {
    if ($channelErrorStore === "unsupported-browser") {
      goto("/unsupported");
    }
  });
</script>

{#if $channelErrorStore !== undefined && $channelErrorStore !== "unsupported-browser"}
  <ChannelError error={$channelErrorStore} />
{:else if $channelStore !== undefined}
  {@render children()}
{/if}
