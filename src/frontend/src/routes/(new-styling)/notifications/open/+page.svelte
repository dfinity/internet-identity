<script lang="ts">
  /**
   * Where a notification tapped in the Home Screen app lands on its way out.
   *
   * iOS gives a standalone web app an in-app browser view for any window opened on
   * another origin, and that view has storage of its own: the user arrives at the app
   * signed out. `x-safari-https:` asks iOS to hand a link to Safari, where they are not.
   * It is honoured for a navigation a page makes, and not for a window a service worker
   * opens — the worker's one opens this page instead, in the app's own scope, and the
   * navigating happens here.
   *
   * Nothing about it is documented by Apple and containers differ in whether they want a
   * real tap first, so it is attempted and then offered: if the page is still here a
   * moment later, the buttons say what the automatic attempt could not do.
   */
  import { onMount } from "svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  let target = $state<string | undefined>(undefined);
  let stillHere = $state(false);

  const viaSafari = (url: string): string =>
    url.replace(/^https:/, "x-safari-https:");

  /** Only `https`, and only what parses: this page follows what it is handed. */
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
    if (target === undefined) {
      return;
    }
    window.location.href = viaSafari(target);
    // Long enough that a handover which worked has already taken the page away.
    const waited = setTimeout(() => (stillHere = true), 1_200);
    return () => clearTimeout(waited);
  });
</script>

<svelte:head>
  <title>ICP Alerts</title>
</svelte:head>

<main class="flex min-h-dvh flex-col items-center justify-center gap-5 p-6">
  {#if target === undefined}
    <p class="text-text-tertiary text-center text-sm">
      <Trans>There is nothing here to open.</Trans>
    </p>
  {:else if stillHere}
    <p class="text-text-tertiary max-w-sm text-center text-sm">
      <Trans>
        Opening this in Safari keeps you signed in to the app that sent it.
      </Trans>
    </p>
    <a class="btn btn-primary btn-xl" href={viaSafari(target)}>
      {$t`Open in Safari`}
    </a>
    <a class="btn btn-tertiary btn-xl" href={target}>{$t`Open here instead`}</a>
  {/if}
</main>
