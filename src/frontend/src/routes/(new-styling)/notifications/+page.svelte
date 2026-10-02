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
  import IosAppSteps from "./blockedSteps/IosAppSteps.svelte";
  import { NOTIFICATION_APP_NAME } from "./appName";
  import { recordAlreadyInstalled } from "$lib/utils/notifications/alreadyInstalled";
  import { watchForInstall } from "./watchInstall";

  /** What this page is doing, which is settled on mount and not before: the answer
   *  depends on the document it is running in and on storage this partition holds. */
  type Screen =
    | { kind: "working" }
    /** A browser tab: this is where the install steps belong. */
    | { kind: "install" }
    /** The app, with no token to claim with and nothing claimed before. */
    | { kind: "nothing-to-claim" }
    /** The app, with the permission still to ask for. iOS raises the prompt only from
     *  a gesture, so there is a button and not a call on mount. */
    | { kind: "ask" }
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

      // Read, never asked for, on the way in. iOS raises the prompt only from a user
      // gesture: asking here answers `default` without showing anything, and treating
      // that as a refusal told the user they were blocked when they had not been asked.
      screen = await resolve(Notification.permission);
    })();
  });

  /** Where this launch lands, for a permission in whichever state it is in. */
  const resolve = async (
    permission: NotificationPermission,
  ): Promise<Screen> => {
    if (permission === "denied") {
      return { kind: "blocked" };
    }
    if (permission === "default") {
      return { kind: "ask" };
    }

    const token = decodeLinkToken(window.location.hash);
    if (token === undefined) {
      const linked = await linkedIdentity();
      return linked === undefined
        ? { kind: "nothing-to-claim" }
        : { kind: "ready", identityNumber: linked };
    }

    const landed = settle(await linkAndRegister(token));
    // Keeps a reload from presenting a spent token. It does not take it out of the
    // Home Screen entry, which captured the URL as it was at install: every launch
    // from there carries the token again. Harmless, because it expires in half an
    // hour and a second claim is refused once a browser has an app, but it is in the
    // bookmark either way.
    history.replaceState(null, "", window.location.pathname);
    return landed;
  };

  /**
   * Raises the prompt, which only a gesture may do, and goes where the answer leads.
   *
   * All three answers mean something different. `granted` carries on; `denied` cannot
   * be undone from here and becomes the Settings steps; a prompt dismissed without an
   * answer leaves the permission at `default`, so it can be raised again and the user
   * stays where they are with the button still in front of them.
   */
  const askForPermission = async () => {
    const asking = screen;
    screen = { kind: "working" };
    try {
      screen = await resolve(await Notification.requestPermission());
    } catch (error) {
      console.error(error);
      screen = asking;
    }
  };

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

  /**
   * Takes the user's word that this device already has the app, so this browser stops
   * offering the install.
   *
   * It is the only party that knows: the app runs in a partition of its own and the
   * canister records which browser installed it, so a second browser on the same phone
   * sees nothing and would keep asking. Recorded before the tab closes, because closing
   * is the part that may be refused — a tab the user opened from a link rather than
   * from the sign-in is not one this document may close.
   */
  const stopAsking = async () => {
    const token = decodeLinkToken(window.location.hash);
    if (token !== undefined) {
      await recordAlreadyInstalled(token.identityNumber);
    }
    window.close();
  };

  /** Picks up a permission the user changed in Settings, which is the only place a
   *  refusal can be lifted. One still refused leaves them on the steps. */
  const retryFromSettings = async () => {
    screen = { kind: "working" };
    const pushState = await readBrowserPushState();
    screen = await resolve(pushState?.permission ?? "denied");
  };
</script>

<svelte:head>
  <title>Internet Identity Notifications</title>
  <link rel="manifest" href="/notifications.webmanifest" />
  <!-- iOS takes the Home Screen icon from here, not from the manifest, so without it
       the tile is a screenshot of the page. 180px is what it asks for. -->
  <link
    rel="apple-touch-icon"
    sizes="180x180"
    href="/notifications-icon-180.png"
  />
  <!-- What iOS read before it supported a manifest, and still honours. -->
  <meta name="apple-mobile-web-app-capable" content="yes" />
  <meta
    name="apple-mobile-web-app-title"
    content="Internet Identity Notifications"
  />
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

          <div class="mt-7 flex flex-col gap-2.5">
            <button
              class="btn btn-primary btn-xl"
              onclick={() => void stopAsking()}
            >
              {$t`I've already added it`}
            </button>
            <button
              class="btn btn-tertiary btn-xl"
              onclick={() => window.close()}
            >
              {$t`Not now`}
            </button>
          </div>
        </div>
      {:else if screen.kind === "ask"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <BellIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Turn on notifications`}
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>
              This is the last step. Your apps' notifications arrive here.
            </Trans>
          </p>
          <div class="mt-7 flex flex-col gap-2.5">
            <button
              class="btn btn-primary btn-xl"
              onclick={() => void askForPermission()}
            >
              {$t`Allow`}
            </button>
          </div>
        </div>
      {:else if screen.kind === "blocked"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <BellOffIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Notifications are blocked`}
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>Follow these steps to turn them back on:</Trans>
          </p>
          <IosAppSteps appName={NOTIFICATION_APP_NAME} />
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
