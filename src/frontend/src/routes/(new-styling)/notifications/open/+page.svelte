<script lang="ts">
  /**
   * Where a notification tapped in the Home Screen app lands on its way to the app that
   * sent it.
   *
   * `clients.openWindow` to another origin gives a standalone web app an in-app browser
   * view rather than Safari. A standalone web app that navigates *itself* out of its own
   * scope is handed to Safari instead, so the worker opens this page, which is in scope,
   * and this page does the navigating.
   *
   * The target rides the fragment, so it never reaches the gateway, and it is checked
   * here before anything follows it: the worker already narrowed it to the sending app's
   * own origins, and this page accepts nothing but `https`.
   */
  import { onMount } from "svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  let target = $state<string | undefined>(undefined);

  const safeTarget = (hash: string): string | undefined => {
    try {
      const url = new URL(decodeURIComponent(hash.replace(/^#/, "")));
      return url.protocol === "https:" ? url.toString() : undefined;
    } catch {
      return undefined;
    }
  };

  onMount(() => {
    target = safeTarget(window.location.hash);
    if (target !== undefined) {
      // Replaced rather than pushed: this page is a doorway, and leaving it in the
      // history would put the user back on it when they come back.
      window.location.replace(target);
    }
  });
</script>

<svelte:head>
  <title>Internet Identity Notifications</title>
</svelte:head>

<main class="flex min-h-dvh flex-col items-center justify-center gap-4 p-6">
  {#if target === undefined}
    <p class="text-text-tertiary text-center text-sm">
      <Trans>There is nothing here to open.</Trans>
    </p>
  {:else}
    <!-- Shown only if the navigation above did not take, which leaves the user looking
         at this page with no way on. -->
    <a class="btn btn-primary btn-xl" href={target}>{$t`Continue`}</a>
  {/if}
</main>
