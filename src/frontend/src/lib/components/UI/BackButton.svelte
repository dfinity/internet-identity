<script lang="ts">
  import Button from "$lib/components/UI/Button.svelte";

  const { ...others }: { [key: string]: any } = $props();

  function handleGoBack() {
    history.back();
  }
</script>

<Button {...others} onclick={handleGoBack}>Back</Button>
