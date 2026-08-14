<script lang="ts">
  import { isRedirectingToPrimaryOrigin } from "$lib/utils/primaryOrigin";
  import { onMount } from "svelte";
  import { page } from "$app/state";

  const { children } = $props();

  onMount(() => {
    // A redirect to the primary origin is in flight, keep the page hidden
    // rather than flashing this origin's render before it commits.
    if (isRedirectingToPrimaryOrigin()) {
      return;
    }

    // 2. Show page once it has fully loaded (see app.html)
    document.documentElement.removeAttribute("data-temp-hide-ssg");

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
