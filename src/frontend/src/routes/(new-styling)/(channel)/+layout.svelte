<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { channelErrorStore, channelStore } from "$lib/stores/channelStore";
  import { authorizationStore } from "$lib/stores/authorization.store";
  import { goto } from "$app/navigation";
  import ChannelError from "$lib/components/ui/ChannelError.svelte";

  const { children }: LayoutProps = $props();
  channelStore.establish();

  $effect(() => {
    if ($channelErrorStore === "unsupported-browser") {
      goto("/unsupported");
    }
  });
</script>

{#if $channelErrorStore !== undefined && $channelErrorStore !== "unsupported-browser"}
  <ChannelError error={$channelErrorStore} />
{:else if $authorizationStore !== undefined}
  {@render children()}
{/if}
