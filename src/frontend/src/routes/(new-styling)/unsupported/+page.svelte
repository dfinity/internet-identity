<script lang="ts">
  import X from "./components/views/X.svelte";
  import Generic from "./components/views/Generic.svelte";

  const isX = /\bTwitter/i.test(navigator.userAgent);
</script>

{#if isX}
  <X />
{:else}
  <Generic />
{/if}
