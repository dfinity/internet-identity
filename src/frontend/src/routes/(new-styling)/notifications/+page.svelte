<script lang="ts">
  import { onMount } from "svelte";
  import BellIcon from "@lucide/svelte/icons/bell";
  import BellOffIcon from "@lucide/svelte/icons/bell-off";
  import SmartphoneIcon from "@lucide/svelte/icons/smartphone";
  import SquarePlusIcon from "@lucide/svelte/icons/square-plus";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import { readBrowserPushState } from "$lib/utils/notifications/notificationState";
  import { isStandalone } from "./standalone";
  import { decodeLinkToken } from "./linkToken";
  import { linkAndRegister, linkedIdentity, type LinkOutcome } from "./link";
  import InstallSteps from "./installSteps/InstallSteps.svelte";
  import { watchForInstall } from "./watchInstall";

  /** What this page is doing, which is settled on mount and not before: the answer
   *  depends on the document it is running in and on storage this partition holds. */
  type Screen =
    | { kind: "working" }
    /** A browser tab: this is where the install steps belong. */
    | { kind: "install" }
    /** The app, with no token to claim with and nothing claimed before. */
    | { kind: "nothing-to-claim" }
    /** The app, with the permission refused. Only iOS Settings can lift it. */
    | { kind: "blocked" }
    | { kind: "ready"; identityNumber: bigint }
    | { kind: "refused"; reason: string };

  let screen = $state<Screen>({ kind: "working" });

  const settle = (outcome: LinkOutcome): Screen =>
    outcome.status === "linked"
      ? { kind: "ready", identityNumber: outcome.identityNumber }
      : { kind: "refused", reason: outcome.reason };

  onMount(() => {
    void (async () => {
      if (!isStandalone()) {
        screen = { kind: "install" };
        return;
      }

      // Asked before anything is claimed: a refused permission is the one thing this
      // app cannot do anything about, and claiming an entry it cannot deliver to would
      // leave a row behind that reaches nothing.
      const permission = await Notification.requestPermission();
      if (permission !== "granted") {
        screen = { kind: "blocked" };
        return;
      }

      const token = decodeLinkToken(window.location.hash);
      if (token === undefined) {
        const linked = await linkedIdentity();
        screen =
          linked === undefined
            ? { kind: "nothing-to-claim" }
            : { kind: "ready", identityNumber: linked };
        return;
      }

      screen = settle(await linkAndRegister(token));
      // The token is spent. Taking it out of the URL keeps a reload from presenting it
      // again, and keeps it out of what the Home Screen entry holds from here on.
      history.replaceState(null, "", window.location.pathname);
    })();
  });

  // Only while the steps are up, and only for the identity whose token brought the user
  // here: the tab goes when the app it is telling them to install has arrived.
  $effect(() => {
    if (screen.kind !== "install") {
      return;
    }
    const token = decodeLinkToken(window.location.hash);
    if (token === undefined) {
      return;
    }
    return watchForInstall(token.identityNumber, () => window.close());
  });

  const retryFromSettings = async () => {
    screen = { kind: "working" };
    const pushState = await readBrowserPushState();
    if (pushState === undefined || pushState.permission === "denied") {
      screen = { kind: "blocked" };
      return;
    }
    const linked = await linkedIdentity();
    const token = decodeLinkToken(window.location.hash);
    if (token !== undefined) {
      screen = settle(await linkAndRegister(token));
      return;
    }
    screen =
      linked === undefined
        ? { kind: "nothing-to-claim" }
        : { kind: "ready", identityNumber: linked };
  };
</script>

<svelte:head>
  <title>Internet Identity Notifications</title>
  <link rel="manifest" href="/notifications.webmanifest" />
  <!-- What iOS read before it supported a manifest, and still honours. -->
  <meta name="apple-mobile-web-app-capable" content="yes" />
  <meta name="apple-mobile-web-app-title" content="II Notifications" />
  <meta name="apple-mobile-web-app-status-bar-style" content="black" />
  <meta name="robots" content="noindex, nofollow" />
</svelte:head>

<div
  class="grid w-full flex-1 items-center max-sm:items-stretch sm:w-100 sm:max-w-100"
>
  <div class="relative col-start-1 row-start-1 flex min-w-0 flex-col gap-5">
    <AuthPanel class="z-1">
      {#if screen.kind === "working"}
        <div class="flex flex-col items-stretch">
          <div class="skeleton mb-4 h-12 w-12 rounded-lg"></div>
          <div class="skeleton mb-3 h-8 w-3/4 rounded"></div>
          <div class="skeleton h-6 w-full rounded"></div>
        </div>
      {:else if screen.kind === "install"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <SquarePlusIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Add to Home Screen`}
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>Follow these steps to get notifications:</Trans>
          </p>
          <InstallSteps host={window.location.host} />
        </div>
      {:else if screen.kind === "blocked"}
        <div class="flex min-w-0 flex-col items-stretch">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <BellOffIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Notifications are blocked`}
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>Follow these steps to turn them back on:</Trans>
          </p>
          <div class="mt-7 flex flex-col gap-2.5">
            <button
              class="btn btn-primary btn-xl"
              onclick={() => void retryFromSettings()}
            >
              {$t`Try again`}
            </button>
          </div>
        </div>
      {:else if screen.kind === "ready"}
        <div class="flex min-w-0 flex-col items-stretch">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <BellIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Notifications are on`}
          </h1>
          <p class="text-text-tertiary text-base">
            <Trans>
              You can close this. Notifications from your apps will arrive here.
            </Trans>
          </p>
        </div>
      {:else if screen.kind === "nothing-to-claim"}
        <div class="flex min-w-0 flex-col items-stretch">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <SmartphoneIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Start from your browser`}
          </h1>
          <p class="text-text-tertiary text-base">
            <Trans>
              Sign in to an app and allow notifications. That is what sets this
              up.
            </Trans>
          </p>
        </div>
      {:else}
        <div class="flex min-w-0 flex-col items-stretch">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <BellOffIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Set this up again`}
          </h1>
          <p class="text-text-tertiary mb-2 text-base">
            <Trans>
              Sign in to an app from your browser and allow notifications.
            </Trans>
          </p>
          <p class="text-text-tertiary text-sm">{screen.reason}</p>
        </div>
      {/if}
    </AuthPanel>
  </div>
</div>
