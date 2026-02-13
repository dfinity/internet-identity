<script lang="ts">
  import { analytics, initAnalytics } from "$lib/utils/analytics/analytics";
  import { canisterConfig, getPrimaryOrigin } from "$lib/globals";
  import { onMount } from "svelte";
  import { page } from "$app/state";

  const { children } = $props();

  onMount(() => {
    // Always redirect to primary origin
    const primaryOrigin = getPrimaryOrigin();
    if (
      primaryOrigin !== undefined &&
      window.location.origin !== primaryOrigin &&
      // Don't redirect if we're coming from legacy AuthClient
      !(
        window.location.pathname === "/" &&
        window.location.hash === "#authorize"
      ) &&
      // Don't redirect if we're visiting legacy verifiable credentials
      window.location.pathname !== "/vc-flow" &&
      // Don't redirect if we're visiting self-service
      window.location.pathname !== "/self-service" &&
      // Don't redirect if we're visiting webauthn iframe used for migration
      window.location.pathname !== "/iframe/webauthn" &&
      // Don't redirect if we're visiting callback used for OpenID flows
      window.location.pathname !== "/callback" &&
      // Don't redirect if we're visiting new authorize flow
      // TODO: Implement redirect with pending ICRC-29 state
      window.location.pathname !== "/authorize"
    ) {
      window.location.replace(
        primaryOrigin +
          window.location.pathname +
          window.location.search +
          window.location.hash,
      );
      return;
    }

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
  });
</script>

<svelte:head>
  {#if page.url.pathname === "/"}
    <style>
      html[data-temp-hide-ssg] body > div {
        display: none !important;
      }
    </style>
    <script>
      if (window.location.hash.length > 0) {
        // 1B. Hide page until it has redirected on new domains,
        // this avoids showing the SSG landing page beforehand.
        document.documentElement.setAttribute("data-temp-hide-ssg", "fragment");
      }
    </script>
  {/if}
</svelte:head>

{@render children()}
