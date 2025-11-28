<script lang="ts">
  import XBrowser from "./components/views/XBrowser.svelte";
  import Generic from "./components/views/Generic.svelte";

  const isX = /\bTwitter/i.test(navigator.userAgent);
</script>

{#if isX}
  <XBrowser />
{:else}
  <Generic />
{/if}
