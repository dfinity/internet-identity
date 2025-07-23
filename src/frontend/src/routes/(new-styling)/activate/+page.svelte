<script lang="ts">
  import { onMount } from "svelte";
  import { goto } from "$app/navigation";

  // Confirming a passkey from another device is handled in the dashboard,
  // this can't be a load function since URL hash can't be read in there.
  onMount(() => {
    goto(`/manage?activate=${window.location.hash.slice(1)}`, {
      replaceState: true,
    });
  });
</script>
