<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { channelErrorStore, channelStore } from "$lib/stores/channelStore";
  import { goto } from "$app/navigation";

  const { children }: LayoutProps = $props();
  channelStore.establish({ pending: true });

  $effect(() => {
    if ($channelErrorStore === "unsupported-browser") {
      goto("/unsupported");
    }
  });
</script>

{@render children()}
