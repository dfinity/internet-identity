<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { channelErrorStore, channelStore } from "$lib/stores/channelStore";
  import { authorizationStore } from "$lib/stores/authorization.store";
  import { goto } from "$app/navigation";

  const { children }: LayoutProps = $props();
  channelStore.establish();

  $effect(() => {
    if ($channelErrorStore === undefined) {
      return;
    }
    if ($channelErrorStore === "unsupported-browser") {
      goto("/unsupported");
    } else {
      goto(`/authorize/error?code=${$channelErrorStore}`);
    }
  });
</script>

{#if $authorizationStore !== undefined}
  {@render children()}
{/if}
