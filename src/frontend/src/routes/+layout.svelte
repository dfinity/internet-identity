<script lang="ts">
  import { isRedirectingToPrimaryOrigin } from "$lib/utils/primaryOrigin";
  import { onMount } from "svelte";
  import { page } from "$app/state";
  import { t } from "$lib/stores/locale.store";

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

<!-- Without a boundary an uncaught render or effect error tears the whole app
     down, leaving an empty page; this keeps the error readable instead. -->
<svelte:boundary onerror={(error) => console.error(error)}>
  {@render children()}

  {#snippet failed(error, reset)}
    <div class="flex flex-col gap-4 p-6">
      <h1 class="text-fg-primary text-xl font-semibold">
        {$t`Something went wrong`}
      </h1>
      <pre
        class="text-fg-secondary overflow-auto text-xs whitespace-pre-wrap">{error instanceof
        Error
          ? `${error.name}: ${error.message}\n${error.stack ?? ""}`
          : String(error)}</pre>
      <button class="btn btn-primary" onclick={reset}>{$t`Try again`}</button>
    </div>
  {/snippet}
</svelte:boundary>
