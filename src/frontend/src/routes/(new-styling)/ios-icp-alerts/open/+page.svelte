<script lang="ts">
  /** iOS gives a standalone web app an in-app browser view, with storage of its own, for any window opened on another origin, so `x-safari-https:` hands the link to Safari instead.
   *  It is honoured for a navigation a page makes and not for a window a service worker opens, and nothing about it is documented, so it is attempted and then offered. */
  import { onMount } from "svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { NOTIFICATION_APP_NAME } from "../appName";
  import NotificationMock from "../NotificationMock.svelte";

  let target = $state<string | undefined>(undefined);
  let shown = $state<{ title: string; body: string } | undefined>(undefined);
  let stillHere = $state(false);

  const viaSafari = (url: string): string =>
    url.replace(/^https:/, "x-safari-https:");

  /** Only `https`, and only what parses: this page follows what it is handed. */
  const carried = (
    hash: string,
  ): { url: string; title: string; body: string } | undefined => {
    const params = new URLSearchParams(hash.replace(/^#/, ""));
    const raw = params.get("url");
    if (raw === null) {
      return undefined;
    }
    try {
      const url = new URL(raw);
      return url.protocol === "https:"
        ? {
            url: url.toString(),
            title: params.get("title") ?? "",
            body: params.get("body") ?? "",
          }
        : undefined;
    } catch {
      return undefined;
    }
  };

  onMount(() => {
    let waited: ReturnType<typeof setTimeout> | undefined;

    // iOS gives the app one window, so a second notification reaches this page as a
    // change of fragment rather than a fresh load, and mounting once would hand Safari
    // the previous notification's destination.
    const handOver = () => {
      clearTimeout(waited);
      stillHere = false;
      const carrying = carried(window.location.hash);
      target = carrying?.url;
      shown =
        carrying !== undefined && carrying.title !== ""
          ? { title: carrying.title, body: carrying.body }
          : undefined;
      if (target === undefined) {
        return;
      }
      window.location.href = viaSafari(target);
      // Long enough that a handover which worked has already taken the page away.
      waited = setTimeout(() => (stillHere = true), 1_200);
    };

    handOver();
    window.addEventListener("hashchange", handOver);
    return () => {
      clearTimeout(waited);
      window.removeEventListener("hashchange", handOver);
    };
  });
</script>

<svelte:head>
  <title>{NOTIFICATION_APP_NAME}</title>
</svelte:head>

{#if target === undefined}
  <main class="flex min-h-dvh flex-col items-center justify-center p-6">
    <p class="text-text-tertiary text-center text-sm">
      <Trans>There is nothing here to open.</Trans>
    </p>
  </main>
{:else if stillHere}
  <main class="flex min-h-dvh flex-col">
    <div
      class="flex min-h-0 flex-1 flex-col items-center justify-center gap-11"
    >
      {#if shown !== undefined}
        <NotificationMock title={shown.title} body={shown.body} />
      {/if}
      <div class="px-8 text-center">
        <h1
          class="text-text-primary text-[28px]/[34px] font-medium tracking-[-0.02em] text-balance"
        >
          <Trans>Almost there</Trans>
        </h1>
        <p class="text-text-tertiary mt-3 text-base text-balance">
          <Trans>
            One more tap opens the app that sent you this notification.
          </Trans>
        </p>
      </div>
    </div>
    <div class="flex flex-col gap-2.5 px-4 pt-8 pb-16">
      <a class="btn btn-primary btn-xl" href={viaSafari(target)}>
        {$t`Continue`}
      </a>
      <p
        class="text-text-tertiary flex items-center justify-center gap-2 text-sm"
      >
        <svg
          viewBox="0 0 24 24"
          width="14"
          height="14"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-linecap="round"
          stroke-linejoin="round"
          aria-hidden="true"
          class="block shrink-0"
        >
          <rect x="5" y="11" width="14" height="10" rx="2"></rect>
          <path d="M8 11V7a4 4 0 0 1 8 0v4"></path>
        </svg>
        <Trans>Delivered securely through id.ai.</Trans>
      </p>
    </div>
  </main>
{/if}
