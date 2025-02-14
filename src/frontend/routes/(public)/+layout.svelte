<script lang="ts">
  import { authenticationState } from "../../lib/authentication.svelte";
  import { goto } from "$app/navigation";
  import { page } from "$app/state";

  $effect(() => {
    if (authenticationState.connection) {
      goto(page.url.searchParams.get("next") ?? "/");
    }
  });

  const { children } = $props();
</script>


{#if !authenticationState.connection}
  {@render children()}
{/if}
