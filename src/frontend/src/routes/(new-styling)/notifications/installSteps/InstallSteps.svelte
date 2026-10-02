<script lang="ts">
  import { browserAndSystem } from "$lib/utils/describeBrowser";
  import SafariIosSteps from "./SafariIosSteps.svelte";
  import ChromeIosSteps from "./ChromeIosSteps.svelte";

  // Read synchronously, because this is the screen: a choice made from a promise would
  // paint one browser's steps first and swap them for another's.
  const { brand } = browserAndSystem();
  // Safari's route starts at the menu button; every other iOS browser shares a WebKit
  // view and puts the share icon in the address bar, which is Chrome's route.
  const safari = "Safari" in brand;

  const { host }: { host: string } = $props();
</script>

{#if safari}
  <SafariIosSteps {host} />
{:else}
  <ChromeIosSteps {host} />
{/if}
