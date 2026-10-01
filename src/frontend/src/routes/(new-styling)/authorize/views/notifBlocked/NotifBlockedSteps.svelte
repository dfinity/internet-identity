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

  const host = window.location.hostname;
</script>

<ol class="flex flex-col gap-3">
  {#if variant === "chrome"}
    <ChromeDesktopSteps {host} />
  {:else if variant === "chrome-android"}
    <ChromeAndroidSteps {host} />
  {:else if variant === "firefox"}
    <FirefoxDesktopSteps {host} />
  {:else if variant === "firefox-android"}
    <FirefoxAndroidSteps {host} />
  {:else}
    <SafariMacosSteps {host} />
  {/if}
</ol>
