<script lang="ts">
  /** iOS gives a standalone web app an in-app browser view, with storage of its own, for any window opened on another origin, so `x-safari-https:` hands the link to Safari instead.
   *  It is honoured for a navigation a page makes and not for a window a service worker opens, and nothing about it is documented, so it is attempted and then offered. */
  import { onMount } from "svelte";
  import LockIcon from "@lucide/svelte/icons/lock";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { NOTIFICATION_APP_NAME } from "../appName";
  import { NOTIFICATION_APP_PATH } from "$lib/utils/notifications/appPath";
  import { isStandalone } from "../standalone";
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

      // Taken out of the window before anything else, because iOS resumes a standalone
      // app on whatever page its window was left on rather than at `start_url`. A
      // fragment left here is a doorway the Home Screen icon can come back to, and the
      // icon must never land on the hand-over offer. What it carries is already read.
      if (window.location.hash !== "") {
        window.history.replaceState(
          null,
          "",
          window.location.pathname + window.location.search,
        );
      }

      target = carrying?.url;
      shown =
        carrying !== undefined && carrying.title !== ""
          ? { title: carrying.title, body: carrying.body }
          : undefined;
      if (target === undefined) {
        return;
      }
      // A browser reaches this page only because a worker cannot open another origin
      // directly, so there is nothing to hand over: it goes where the link points.
      // The Safari scheme is the installed app's problem alone.
      if (!isStandalone()) {
        window.location.replace(target);
        return;
      }
      window.location.href = viaSafari(target);
      // Long enough that a handover which worked has already taken the page away.
      waited = setTimeout(() => (stillHere = true), 1_200);
    };

    /**
     * Puts the window back on the app once this one has been left.
     *
     * The two ways back into a running app, Safari's chevron and the Home Screen icon,
     * are the same foreground of the same window: nothing distinguishes them, so the
     * offer cannot be kept for one without showing it to the other. The icon is the one
     * that must never see it, so neither does.
     *
     * Forgotten on the way out rather than on the way back in, so a resume renders
     * nothing from the notification even where iOS has throttled this page's timers,
     * and the navigation below only has to finish the job.
     */
    const onHidden = () => {
      clearTimeout(waited);
      stillHere = false;
      target = undefined;
      shown = undefined;
    };

    const onVisible = () => {
      // A notification clicked while the app is backgrounded is navigated in and only
      // then focused, so this page has already read and dropped its fragment by now.
      // What is left is `target`, and `onHidden` clears it, so one that is still set
      // was put there for this foreground rather than a previous one.
      if (target !== undefined) {
        return;
      }
      // The other order, where the navigation lands after the window is surfaced: the
      // fragment is still in the URL, and that is a doorway to open, not one to leave.
      if (carried(window.location.hash) !== undefined) {
        return;
      }
      window.location.replace(NOTIFICATION_APP_PATH);
    };

    const onVisibilityChange = () =>
      document.visibilityState === "hidden" ? onHidden() : onVisible();

    handOver();
    window.addEventListener("hashchange", handOver);
    document.addEventListener("visibilitychange", onVisibilityChange);
    return () => {
      clearTimeout(waited);
      window.removeEventListener("hashchange", handOver);
      document.removeEventListener("visibilitychange", onVisibilityChange);
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
        <LockIcon class="size-3.5 shrink-0" aria-hidden="true" />
        <Trans>Delivered securely through id.ai.</Trans>
      </p>
    </div>
  </main>
{/if}
