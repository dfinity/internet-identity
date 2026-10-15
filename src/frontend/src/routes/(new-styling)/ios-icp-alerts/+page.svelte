<script lang="ts">
  import { onMount } from "svelte";
  import BellIcon from "@lucide/svelte/icons/bell";
  import BellOffIcon from "@lucide/svelte/icons/bell-off";
  import CircleAlertIcon from "@lucide/svelte/icons/circle-alert";
  import SmartphoneIcon from "@lucide/svelte/icons/smartphone";
  import SquarePlusIcon from "@lucide/svelte/icons/square-plus";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import { isCanisterError } from "$lib/utils/utils";
  import { readBrowserPushState } from "$lib/utils/notifications/notificationState";
  import { ensureAppKey } from "$lib/utils/notifications/notificationAppKey";
  import { ensureRegisteredApp } from "$lib/utils/notifications/subscribeApp";
  import { isStandalone } from "./standalone";
  import { decodeLinkToken } from "./linkToken";
  import { appKeyActor } from "./appKeyActor";
  import { appAnchors, browserAppStatus } from "./appStatus";
  import { linkWithToken } from "./link";
  import { openInSafari } from "./openInSafari";
  import { watchForInstall } from "./watchInstall";
  import { NOTIFICATION_APP_NAME } from "./appName";
  import SafariIosSteps from "./installSteps/SafariIosSteps.svelte";
  import IosAppSteps from "./blockedSteps/IosAppSteps.svelte";

  /** What this page is doing, which is settled on mount and not before: the answer
   *  depends on the document it is running in and on storage this partition holds. */
  type Screen =
    | { kind: "working" }
    /** A browser tab, with no app on this device to point at yet. */
    | { kind: "install" }
    /** A browser tab, where this browser already points at an app. Nothing here can open
     *  it: iOS has no link capturing, so only the user's finger crosses that boundary. */
    | { kind: "open-app" }
    /** The app, with the permission still to ask for. iOS raises the prompt only from a
     *  gesture, so there is a button and not a call on mount. */
    | { kind: "ask" }
    /** The app, with the permission refused. Only iOS Settings can lift it. */
    | { kind: "blocked" }
    /** The app, delivering. */
    | { kind: "ready" }
    /** The app, with no identity pointing at it. */
    | { kind: "orphaned" }
    | { kind: "failed"; reason: string };

  let screen = $state<Screen>({ kind: "working" });
  /** Read on mount, because this page is rendered at build time where there is no
   *  document to ask. */
  let host = $state("");

  /**
   * Where a launch that could not be settled lands.
   *
   * Registering is part of every launch and it can be refused, and `CanisterError`
   * carries no message, so the variant is the whole of what it said.
   */
  const failed = (error: unknown): Screen => {
    console.error(error);
    return {
      kind: "failed",
      reason: isCanisterError(error)
        ? String(error.type)
        : error instanceof Error
          ? error.message
          : String(error),
    };
  };

  onMount(() => {
    void (async () => {
      host = window.location.host;
      screen = isStandalone()
        ? await insideTheApp().catch(failed)
        : await insideTheBrowser().catch(failed);
    })();
  });

  /**
   * What the browser tab shows: the steps, or where to find the app it already has.
   *
   * The token names the identity, so a tab opened without one has nothing to ask about
   * and shows the steps, which is also what it shows for a browser pointing at nothing.
   */
  const insideTheBrowser = async (): Promise<Screen> => {
    const token = decodeLinkToken(window.location.hash);
    if (token === undefined) {
      return { kind: "install" };
    }
    const status = await browserAppStatus(token.identityNumber);
    return status === undefined ? { kind: "install" } : { kind: "open-app" };
  };

  /**
   * What the app shows, which is settled against the canister rather than against
   * anything it holds: a browser can point another identity at this app while it is not
   * running, and iOS can destroy its storage without telling it.
   */
  const insideTheApp = async (): Promise<Screen> => {
    const record = await ensureAppKey();
    const actor = await appKeyActor(record);

    const token = decodeLinkToken(window.location.hash);
    if (token !== undefined) {
      const outcome = await linkWithToken(actor, record, token);
      if (outcome.status !== "linked") {
        // Left in the URL where it did not land: it has not been spent, it is still
        // inside its half hour, and reopening the app is the whole of the retry.
        return { kind: "failed", reason: outcome.reason };
      }
      // Keeps a reload from presenting a spent token. The Home Screen entry captured the
      // URL as it was at install and carries it at every launch, which is harmless: a
      // token already spent on this key links the same record again.
      history.replaceState(null, "", window.location.pathname);
    }

    if ((await appAnchors(actor)).length === 0) {
      return { kind: "orphaned" };
    }
    return await settle(Notification.permission, actor);
  };

  /** Where this launch lands, for a permission in whichever state it is in. */
  const settle = async (
    permission: NotificationPermission,
    actor: Awaited<ReturnType<typeof appKeyActor>>,
  ): Promise<Screen> => {
    if (permission === "denied") {
      return { kind: "blocked" };
    }
    if (permission === "default") {
      return { kind: "ask" };
    }
    // Registered on every launch and not only the one that linked: the push service
    // hands out a new endpoint whenever it likes, and opening the app is the only moment
    // anything can write the new one.
    await ensureRegisteredApp(actor);
    return { kind: "ready" };
  };

  /**
   * Raises the prompt, which only a gesture may do, and goes where the answer leads.
   *
   * All three answers mean something different. `granted` carries on; `denied` cannot be
   * undone from here and becomes the Settings steps; a prompt dismissed without an answer
   * leaves the permission at `default`, so it can be raised again and the user stays
   * where they are with the button still in front of them.
   */
  const askForPermission = async () => {
    const asking = screen;
    screen = { kind: "working" };
    let permission: NotificationPermission;
    try {
      permission = await Notification.requestPermission();
    } catch (error) {
      console.error(error);
      screen = asking;
      return;
    }
    const record = await ensureAppKey();
    screen = await settle(permission, await appKeyActor(record)).catch(failed);
  };

  /** Picks up a permission the user changed in Settings, which is the only place a
   *  refusal can be lifted. One still refused leaves them on the steps. */
  const retryFromSettings = async () => {
    screen = { kind: "working" };
    const pushState = await readBrowserPushState();
    const record = await ensureAppKey();
    screen = await settle(
      pushState?.permission ?? "denied",
      await appKeyActor(record),
    ).catch(failed);
  };

  // Only while the steps are up, and only for the identity whose token brought the user
  // here: the tab says so once the app it is telling them to install has registered.
  $effect(() => {
    if (screen.kind !== "install" && screen.kind !== "open-app") {
      return;
    }
    const token = decodeLinkToken(window.location.hash);
    if (token === undefined) {
      return;
    }
    return watchForInstall(token.identityNumber, () => {
      screen = { kind: "ready" };
    });
  });
</script>

<svelte:head>
  <title>{NOTIFICATION_APP_NAME}</title>
  <link rel="manifest" href="/ios-icp-alerts.webmanifest" />
  <!-- iOS takes the Home Screen icon from here, not from the manifest, so without it the
       tile is a screenshot of the page. 180px is what it asks for. -->
  <link
    rel="apple-touch-icon"
    sizes="180x180"
    href="/ios-icp-alerts-icon-180.png"
  />
  <!-- What iOS read before it supported a manifest, and still honours. -->
  <meta name="apple-mobile-web-app-capable" content="yes" />
  <meta name="apple-mobile-web-app-title" content={NOTIFICATION_APP_NAME} />
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
            <Trans>Set up {NOTIFICATION_APP_NAME}</Trans>
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>Follow these steps to get notifications:</Trans>
          </p>
          <SafariIosSteps {host} />
        </div>
      {:else if screen.kind === "open-app"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <SmartphoneIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            <Trans>Open {NOTIFICATION_APP_NAME}</Trans>
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>
              It's on your Home Screen. Open it once and notifications are on
              for this identity.
            </Trans>
          </p>
          <div class="mt-7 flex flex-col gap-2.5">
            <button
              class="btn btn-tertiary btn-xl"
              onclick={() => (screen = { kind: "install" })}
            >
              {$t`I don't have the app any more`}
            </button>
          </div>
        </div>
      {:else if screen.kind === "ask"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <BellIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Let your apps notify you`}
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>
              Payments, votes and messages from the apps you use, right on this
              device.
            </Trans>
          </p>
          <div class="mt-7 flex flex-col gap-2.5">
            <button
              class="btn btn-primary btn-xl"
              onclick={() => void askForPermission()}
            >
              {$t`Allow notifications`}
            </button>
          </div>
          <p class="text-text-tertiary mt-5 text-sm">
            <Trans>Only apps you allow on {host} can notify you.</Trans>
          </p>
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
            {$t`You're all set`}
          </h1>
          <p class="text-text-tertiary text-base">
            <Trans>
              Notifications from your apps will show up on this device.
            </Trans>
          </p>
        </div>
      {:else if screen.kind === "orphaned"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <SmartphoneIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`This app isn't connected`}
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>
              Nothing can arrive here until it's set up again. Follow these
              steps:
            </Trans>
          </p>
          <ol class="text-text-tertiary flex flex-col gap-4 text-base">
            <li>
              <span class="text-text-primary font-medium"
                >{$t`1. Remove this app`}</span
              >
              <Trans>
                Touch and hold the {NOTIFICATION_APP_NAME} icon on your Home Screen,
                then tap Remove App.
              </Trans>
            </li>
            <li>
              <span class="text-text-primary font-medium"
                >{$t`2. Set up notifications`}</span
              >
              <Trans>
                Open {host}, sign in, and turn notifications on. It will show
                you how to add the app back.
              </Trans>
            </li>
          </ol>
          <div class="mt-7 flex flex-col gap-2.5">
            <button
              class="btn btn-primary btn-xl"
              onclick={() => openInSafari(`https://${host}`)}
            >
              <Trans>Open {host}</Trans>
            </button>
          </div>
        </div>
      {:else}
        <div class="flex min-w-0 flex-col items-stretch">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <CircleAlertIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Something went wrong`}
          </h1>
          <p class="text-text-tertiary mb-2 text-base">
            <Trans>
              Notifications couldn't be set up on this device. Try again, and if
              it keeps failing, set this up again from {host}.
            </Trans>
          </p>
          <p class="text-text-tertiary text-sm">{screen.reason}</p>
          <div class="mt-7 flex flex-col gap-2.5">
            <button
              class="btn btn-primary btn-xl"
              onclick={() => void retryFromSettings()}
            >
              {$t`Try again`}
            </button>
            <button
              class="btn btn-tertiary btn-xl"
              onclick={() => openInSafari(`https://${host}`)}
            >
              <Trans>Open {host}</Trans>
            </button>
          </div>
        </div>
      {/if}
    </AuthPanel>
  </div>
</div>
