<script lang="ts">
  import { analytics, initAnalytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig } from "$lib/globals";
  import { onMount } from "svelte";
  import { page } from "$app/state";

  const { children } = $props();

  onMount(() => {
    // 2. Show page once it has fully loaded (see app.html)
    document.documentElement.removeAttribute("data-hide-ssg");

    // Fix z position of password managers
    new MutationObserver(() => {
      // Currently this is only needed for 1Password
      const element = document.querySelector("com-1password-notification");
      if (!(element instanceof HTMLElement)) {
        return;
      }
      // Move 1Password to the top layer
      element.setAttribute("popover", "manual");
      element.showPopover();
    }).observe(document.documentElement, {
      childList: true,
      subtree: true,
    });

    // Initialize analytics
    initAnalytics(canisterConfig.analytics_config[0]?.[0]);
    analytics.pageView();
  });
</script>

<svelte:head>
  {#if page.url.pathname === "/"}
    <style>
      html[data-temp-hide-ssg] body > div {
        display: none !important;
      }

      html[data-temp-hide-ssg="legacy"] body {
        /* Make sure page background matches legacy (--nvc-surface-dark in src/frontend/src/lib/legacy/styles/main.css) */
        background: oklch(0.15 0.0236 261.52) !important;
      }
    </style>
    <script>
      if (
        // TODO: Remove this branch once we move legacy domains to new interface
        window.location.hostname.startsWith("identity.") ||
        window.location.hostname.startsWith("beta.identity.")
      ) {
        // 1A. Hide page until it has fully loaded on legacy domains,
        // this avoids showing the SSG landing page beforehand.
        document.documentElement.setAttribute("data-temp-hide-ssg", "legacy");
      } else if (window.location.hash.length > 0) {
        // 1B. Hide page until it has redirected on new domains,
        // this avoids showing the SSG landing page beforehand.
        document.documentElement.setAttribute("data-temp-hide-ssg", "fragment");
      }
    </script>
  {/if}
</svelte:head>

{@render children()}
