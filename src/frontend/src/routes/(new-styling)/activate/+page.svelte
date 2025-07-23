<script lang="ts">
  import { onMount } from "svelte";
  import { goto } from "$app/navigation";

  // Confirming a passkey from another device is handled in the dashboard
  onMount(() => {
    goto(`/manage?activate=${window.location.hash.slice(1)}`, {
      replaceState: true,
    });
  });
</script>
