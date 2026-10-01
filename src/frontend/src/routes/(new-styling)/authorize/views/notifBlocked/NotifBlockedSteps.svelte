<script lang="ts">
  import { browserAndSystem } from "$lib/utils/describeBrowser";
  import { blockedStepsVariant } from "./variant";
  import ChromeDesktopSteps from "./ChromeDesktopSteps.svelte";
  import ChromeAndroidSteps from "./ChromeAndroidSteps.svelte";
  import FirefoxDesktopSteps from "./FirefoxDesktopSteps.svelte";
  import FirefoxAndroidSteps from "./FirefoxAndroidSteps.svelte";
  import SafariMacosSteps from "./SafariMacosSteps.svelte";

  // Read synchronously, because this is the screen: a choice made from a promise
  // would paint the wrong steps first and swap them.
  const { brand, os } = browserAndSystem();
  const variant = blockedStepsVariant(brand, os);

  // What a browser puts in each place: the address bar carries the path, the site
  // panels name the host on its own.
  //
  // One departure from the design, agreed: it sets the address 8px from the ringed
  // control, where the outermost ring crosses the first character. The steps here
  // use 20px so the host reads whole.
  const host = window.location.host;
  const address = `${window.location.host}${window.location.pathname}`;
</script>

{#if variant === "chrome"}
  <ChromeDesktopSteps {host} {address} />
{:else if variant === "chrome-android"}
  <ChromeAndroidSteps {host} {address} />
{:else if variant === "firefox"}
  <FirefoxDesktopSteps {host} {address} />
{:else if variant === "firefox-android"}
  <FirefoxAndroidSteps {host} {address} />
{:else}
  <SafariMacosSteps {host} />
{/if}
