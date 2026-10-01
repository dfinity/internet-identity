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

<!-- The mocks are drawn with the colours and rings the design names, which are the
     ones `/unsupported` already uses for its own browser mock. Declared here so the
     ported markup keeps the variables it was drawn with. -->
<div>
  {#if variant === "chrome"}<ChromeDesktopSteps
      {host}
      {address}
    />{:else if variant === "chrome-android"}<ChromeAndroidSteps
      {host}
      {address}
    />{:else if variant === "firefox"}<FirefoxDesktopSteps
      {host}
      {address}
    />{:else if variant === "firefox-android"}<FirefoxAndroidSteps
      {host}
      {address}
    />{:else}<SafariMacosSteps {host} />{/if}
</div>

<style>
  /* The moves a step shows: a switch thrown, a label swapped. Global, because the
     markup they belong to lives in the variant files. */
  :global(.xb-track) {
    animation: xb-track 3s ease-in-out infinite;
  }
  :global(.xb-knob) {
    animation: xb-knob 3s ease-in-out infinite;
  }
  :global(.xb-out) {
    animation: xb-out 3s ease-in-out infinite;
  }
  :global(.xb-in) {
    animation: xb-in 3s ease-in-out infinite;
  }

  @keyframes -global-xb-track {
    0%,
    35% {
      background: var(--color-surface-light-300);
    }
    45%,
    90% {
      background: var(--bg-brand-solid);
    }
    100% {
      background: var(--color-surface-light-300);
    }
  }
  @keyframes -global-xb-knob {
    0%,
    35% {
      transform: translateX(0);
      background: var(--text-primary);
    }
    45%,
    90% {
      transform: translateX(12px);
      background: var(--text-primary-inversed);
    }
    100% {
      transform: translateX(0);
      background: var(--text-primary);
    }
  }
  @keyframes -global-xb-out {
    0%,
    35% {
      opacity: 1;
    }
    45%,
    90% {
      opacity: 0;
    }
    100% {
      opacity: 1;
    }
  }
  @keyframes -global-xb-in {
    0%,
    35% {
      opacity: 0;
    }
    45%,
    90% {
      opacity: 1;
    }
    100% {
      opacity: 0;
    }
  }

  @media (prefers-color-scheme: dark) {
    @keyframes -global-xb-track {
      0%,
      35% {
        background: var(--color-surface-dark-600);
      }
      45%,
      90% {
        background: var(--bg-brand-solid);
      }
      100% {
        background: var(--color-surface-dark-600);
      }
    }
  }
</style>
