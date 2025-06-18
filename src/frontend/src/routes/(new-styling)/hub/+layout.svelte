<script lang="ts">
  import Header from "$lib/components/layout/Header.svelte";
  const { children } = $props();
</script>

<Header customText="Internet Identity Hub" />
{@render children?.()}
