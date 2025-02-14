<script lang="ts">
  import { goto } from "$app/navigation";
  import { authenticationState } from "../../lib/authentication.svelte";
  import { setConnectionContext } from "./context.svelte";
  import { page } from "$app/state";

  if (authenticationState.connection) {
    setConnectionContext(authenticationState.connection);
  }

  $effect(() => {
    if (!authenticationState.connection) {
      goto(`/authenticate${page.url.pathname.length > 1 ? `?next=${page.url.pathname}` : ""}`);
    }
  });

  const { children } = $props();
</script>


{#if authenticationState.connection}
  {@render children()}
{/if}
