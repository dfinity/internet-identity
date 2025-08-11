<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { authorizationStore } from "$lib/stores/authorization.store";

  const handleRedirect = () => {
    authorizationStore.authorize(undefined, 0);
  };
</script>

<Button onclick={handleRedirect}>Go to app</Button>
