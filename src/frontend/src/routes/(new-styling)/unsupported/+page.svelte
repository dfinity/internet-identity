<script lang="ts">
  import XBrowser from "./components/views/XBrowser.svelte";
  import Generic from "./components/views/Generic.svelte";
  import { afterNavigate, beforeNavigate, goto } from "$app/navigation";
  import type { AfterNavigate, BeforeNavigate } from "@sveltejs/kit";

  const PAGE_RELOADED_KEY = "ii-unsupported-page-reloaded";

  const isX = /\bTwitter/i.test(navigator.userAgent);

  /**
   * Store boolean in session storage if user leaves the page,
   * this could be due to reload or closing the tab/window.
   *
   * Session storage is tied to the tab/window,
   * so effectively it's only ever set on reload.
   */
  const trackPageReload = (navigation: BeforeNavigate) => {
    const isReloadOrClose = navigation.type === "leave";
    if (!isReloadOrClose) {
      return;
    }
    sessionStorage.setItem(PAGE_RELOADED_KEY, "true");
  };
  /**
   * Redirect to landing page when this page is visited directly,
   * except if the page was reloaded after internal navigation.
   */
  const redirectOnEntryExceptReload = (navigation: AfterNavigate) => {
    const isEntry = navigation.type === "enter";
    const isReload = sessionStorage.getItem(PAGE_RELOADED_KEY) === "true";

    sessionStorage.removeItem(PAGE_RELOADED_KEY); // Always cleanup after it's read

    if (!isEntry || isReload) {
      return;
    }
    goto("/", { replaceState: true });
  };

  beforeNavigate(trackPageReload);
  afterNavigate(redirectOnEntryExceptReload);
</script>

{#if isX}
  <XBrowser />
{:else}
  <Generic />
{/if}
