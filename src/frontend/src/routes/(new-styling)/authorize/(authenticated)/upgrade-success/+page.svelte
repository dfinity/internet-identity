<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { authorizationStore } from "$lib/stores/authorization.store";

  const handleRedirect = () => {
    authorizationStore.authorize(undefined);
  };
</script>

<Button onclick={handleRedirect}>Go to app</Button>
