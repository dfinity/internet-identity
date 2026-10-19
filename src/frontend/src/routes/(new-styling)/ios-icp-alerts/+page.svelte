<script lang="ts">
  import { onMount } from "svelte";
  import BellIcon from "@lucide/svelte/icons/bell";
  import BellOffIcon from "@lucide/svelte/icons/bell-off";
  import CircleAlertIcon from "@lucide/svelte/icons/circle-alert";
  import ExternalLinkIcon from "@lucide/svelte/icons/external-link";
  import SmartphoneIcon from "@lucide/svelte/icons/smartphone";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import { isStandalone } from "./standalone";
  import {
    decodeLinkToken,
    type FragmentLinkToken,
  } from "$lib/utils/notifications/linkToken";
  import {
    browserAppStatus,
    probeApp,
  } from "$lib/utils/notifications/appStatus";
  import { prepareInstallUrl } from "$lib/utils/notifications/installUrl";
  import {
    afterAlreadyAdded,
    failed,
    holdsTheInstallPlaceholder,
    insideTheApp,
    insideTheBrowser,
    type Screen,
    settleFromSettings,
    settleWithTheApp,
  } from "./screen";
  import { openInSafari } from "./openInSafari";
  import { watchForInstall, watchProbe } from "./watchInstall";
  import { NOTIFICATION_APP_NAME } from "./appName";
  import NotificationMock from "./NotificationMock.svelte";
  import ReinstallSteps from "./ReinstallSteps.svelte";
  import SafariIosSteps from "./installSteps/SafariIosSteps.svelte";
  import IosAppSteps from "./blockedSteps/IosAppSteps.svelte";

  let screen = $state<Screen>({ kind: "working" });
  /** Read on mount, because this page is rendered at build time where there is no document to ask. */
  let host = $state("");
  /** Read on mount for the same reason, and only to shape the screen that waits for the launch. */
  let inTheApp = $state(false);

  const buttonGroup = "mt-7 flex flex-col gap-2.5";
  /** The two screens the Home Screen app opens on centre their one illustration and read from the middle out. */
  const centredBlock =
    "flex min-h-0 flex-1 flex-col items-center justify-center gap-11";
  const centredText = "flex flex-col items-center px-8 text-center";
  const centredHeading =
    "text-text-primary text-[28px]/[34px] font-medium tracking-[-0.02em] text-balance";
  const centredBody = "text-text-tertiary mt-3 text-base text-balance";
  const actionBlock = "flex flex-col gap-3.5 px-4 pt-8 pb-16";

  const openTheHost = () => openInSafari(`https://${host}`);

  /** `null` rather than `undefined` so that an unhandled arm is a compile error. */
  const fragmentIdentity = (fragment: FragmentLinkToken): bigint | null => {
    switch (fragment.kind) {
      case "no-token":
      case "unreadable":
        return null;
      case "token":
        return fragment.token.identityNumber;
    }
  };

  /** The whole of what a launch does, which is also the whole of what retrying it is. */
  const launch = async (): Promise<Screen> =>
    isStandalone()
      ? await insideTheApp(window.location.hash, Notification.permission, () =>
          // Keeps a reload from presenting a spent token; replaying a spent one is harmless either way.
          history.replaceState(null, "", window.location.pathname),
        ).catch(failed)
      : await insideTheBrowser(window.location.hash).catch(failed);

  onMount(() => {
    void (async () => {
      host = window.location.host;
      inTheApp = isStandalone();
      // The tab opened for an install holds this placeholder only until the minted URL
      // replaces it, so it waits instead of deciding on a URL that carries no token.
      // A Home Screen entry added from it is not waiting for anything, so it launches
      // and is told what it is rather than sitting on the skeleton for good.
      if (
        holdsTheInstallPlaceholder({ inTheApp, search: window.location.search })
      ) {
        return;
      }
      screen = await launch();
      if (screen.kind === "install" && screen.staleKey === true) {
        // The steps are about to be followed on a fragment nothing is waiting for.
        void offerAFreshInstall();
        return;
      }
      if (screen.kind === "elsewhere") {
        // Replaced, not pushed: Back from the host would land on this same empty URL.
        window.location.replace("/");
      }
    })();
  });

  /** Runs the launch again: settling from where a failure left off would skip the link and the anchors. */
  const retryLaunch = async () => {
    screen = { kind: "working" };
    screen = await launch();
  };

  /** Raises the prompt, which only a gesture may do; a prompt dismissed without an answer leaves the permission at `default`. */
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
    screen = await settleWithTheApp(permission).catch(failed);
  };

  /** Settings is the only place a refusal can be lifted. */
  const retryFromSettings = async () => {
    if (!isStandalone()) {
      // A tab has no app key, record or push to offer, so settling against a permission here would show the prompt in a document that cannot deliver.
      await retryLaunch();
      return;
    }
    screen = { kind: "working" };
    screen = await settleFromSettings().catch(failed);
  };

  // Only while the tab is waiting for an app to register, and only for the identity whose
  // token brought the user here. `open-to-finish` waits for the same thing as the steps:
  // the user has said the app is on their Home Screen and has yet to open it.
  $effect(() => {
    if (screen.kind !== "install" && screen.kind !== "open-to-finish") {
      return;
    }
    const identityNumber = fragmentIdentity(
      decodeLinkToken(window.location.hash),
    );
    if (identityNumber === null) {
      return;
    }
    return watchForInstall(
      identityNumber,
      () => {
        screen = { kind: "ready", inTheApp: false };
      },
      { replacedLastSeen },
    );
  });

  /** Whether the app answered the probe, which is as close to "it is there" as anything gets. */
  let appAnswered = $state(false);

  /** Taken off this browser's queue means the app is there; a record that disappears means the push service said the endpoint is gone. */
  $effect(() => {
    const watchingForTheApp =
      screen.kind === "open-app" ||
      (screen.kind === "ready" && !screen.inTheApp);
    if (!watchingForTheApp) {
      return;
    }
    const identityNumber = fragmentIdentity(
      decodeLinkToken(window.location.hash),
    );
    if (identityNumber === null) {
      return;
    }
    // Both screens are reachable more than once in one visit, and the last probe says nothing about the app now.
    appAnswered = false;
    let cancelled = false;
    let stopWatching = () => {};
    void (async () => {
      // Read before the probe: only the app's own calls move this stamp, so it is what
      // says the app answered rather than the queue emptying for some other reason.
      const before = await browserAppStatus(identityNumber);
      if (cancelled || before.kind !== "app") {
        return;
      }
      const since = before.app.last_seen_ns;
      const outcome = await probeApp(identityNumber);
      // The probe is an update call and the user can leave this screen while it is in flight.
      if (cancelled) {
        return;
      }
      switch (outcome.kind) {
        case "no-app":
          // There is no app to open, and this URL's one-shot key went with the one that
          // is gone: the steps on it would add an entry that can never link.
          void offerAFreshInstall();
          return;
        case "not-sent":
          // Nothing left the canister, so there is nothing to watch for and nothing said about the app.
          return;
        case "sent":
          stopWatching = watchProbe(identityNumber, {
            since,
            onAnswered: () => (appAnswered = true),
            onGone: () => void offerAFreshInstall(),
          });
          return;
      }
    })();
    return () => {
      cancelled = true;
      stopWatching();
    };
  });

  const alreadyAdded = async () => {
    screen = await afterAlreadyAdded(window.location.hash).catch(failed);
  };

  /** The probe is housekeeping: waking a record that still has an endpoint is what gets the push service to report it dead. */
  /** How far the record the user is replacing had got, read while it is still the one
   *  this browser points at. The watcher needs a new app to beat it, and reading it
   *  later would read the replacement's own. */
  let replacedLastSeen = $state<bigint | undefined>();

  const startOver = async () => {
    const identityNumber = fragmentIdentity(
      decodeLinkToken(window.location.hash),
    );
    if (identityNumber === null) {
      screen = { kind: "install", replacingApp: true };
      return;
    }
    const replaced = await browserAppStatus(identityNumber);
    replacedLastSeen =
      replaced.kind === "app" ? replaced.app.last_seen_ns : undefined;
    void probeApp(identityNumber);
    screen = { kind: "working" };
    try {
      // iOS bookmarks whatever the address bar holds when the page is added, and the key
      // in this one was spent by the app the user has just said is gone.
      const url = await prepareInstallUrl({
        identityNumber,
        replacingExisting: true,
      });
      if (url === undefined) {
        screen = { kind: "unlinked" };
        return;
      }
      history.replaceState(null, "", url);
      screen = { kind: "install", replacingApp: true };
    } catch (error) {
      // `failed` reads the variant: a canister refusal carries no message, so building
      // the reason from `error.message` here would show an empty box.
      screen = failed(error);
    }
  };

  /** The steps on a URL whose key is spent would have the user add an entry that can
   *  never link, so a fresh one is minted into the address bar first. The copy stays the
   *  plain install: this tab cannot tell an app that has gone from one that never was. */
  const offerAFreshInstall = async () => {
    const identityNumber = fragmentIdentity(
      decodeLinkToken(window.location.hash),
    );
    if (identityNumber === null) {
      screen = { kind: "install" };
      return;
    }
    screen = { kind: "working" };
    try {
      // `replacing_existing`, because a record that still claims to deliver is exactly
      // what this path has just found is gone.
      const url = await prepareInstallUrl({
        identityNumber,
        replacingExisting: true,
      });
      if (url === undefined) {
        screen = { kind: "unlinked" };
        return;
      }
      history.replaceState(null, "", url);
      screen = { kind: "install" };
    } catch (error) {
      screen = failed(error);
    }
  };

  /** `window.close()` closes only a tab a script opened, so the screen says what the close could not. */
  const dismiss = () => {
    window.close();
    screen = { kind: "closed" };
  };
</script>

<svelte:head>
  <title>{NOTIFICATION_APP_NAME}</title>
  <link rel="manifest" href="/ios-icp-alerts.webmanifest" />
  <!-- iOS takes the Home Screen icon from here, not from the manifest; 180px is what it asks for. -->
  <link
    rel="apple-touch-icon"
    sizes="180x180"
    href="/ios-icp-alerts-icon-180.png"
  />
  <!-- iOS honours these as well as the manifest: the standalone launch, the Home Screen name and the status bar. -->
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
      {#if screen.kind === "working" || screen.kind === "elsewhere"}
        <!-- `elsewhere` is on its way out of the page, so it waits here too.
             Shaped like the screen it is about to become, so settling moves nothing:
             centred around the notification in the app, bottom-aligned steps in a tab. -->
        {#if inTheApp}
          <div class="flex min-w-0 flex-1 flex-col items-stretch">
            <div class={centredBlock}>
              <div class="flex h-[396px] w-[330px] items-center justify-center">
                <div class="skeleton h-[62px] w-full rounded-3xl"></div>
              </div>
              <div class="flex w-full flex-col items-center gap-3 px-8">
                <div class="skeleton h-8 w-3/4 rounded"></div>
                <div class="skeleton h-6 w-full rounded"></div>
              </div>
            </div>
            <div class={actionBlock}>
              <div class="skeleton h-12 w-full rounded-md"></div>
            </div>
          </div>
        {:else}
          <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
            <div class="skeleton mb-3 h-8 w-3/4 rounded"></div>
            <div class="skeleton mb-5 h-6 w-full rounded"></div>
            <div class="flex flex-col gap-3">
              {#each [0, 1, 2, 3] as step (step)}
                <div class="skeleton h-24 w-full rounded-xl"></div>
              {/each}
            </div>
          </div>
        {/if}
      {:else if screen.kind === "install"}
        <!-- No icon above the heading: the steps are the screen, and every row of
             chrome above them pushes one further below the fold. -->
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          {#if screen.replacingApp === true}
            <h1 class="text-text-primary mb-3 text-2xl font-medium">
              <Trans>Add {NOTIFICATION_APP_NAME} again</Trans>
            </h1>
            <p class="text-text-tertiary mb-5 text-base">
              <Trans>
                Follow these steps to put a new one on your Home Screen:
              </Trans>
            </p>
          {:else}
            <h1 class="text-text-primary mb-4 text-2xl font-medium">
              <Trans>Set up {NOTIFICATION_APP_NAME}</Trans>
            </h1>
          {/if}
          <SafariIosSteps {host} />

          <div class={buttonGroup}>
            <!-- Not for a user who has just told us the app is gone: that would record an app nothing can reach. -->
            {#if screen.replacingApp !== true}
              <button
                class="btn btn-primary btn-xl"
                onclick={() => void alreadyAdded()}
              >
                {$t`I've already added it`}
              </button>
            {/if}
            <button class="btn btn-tertiary btn-xl" onclick={dismiss}>
              {$t`Not now`}
            </button>
          </div>
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
            {#if appAnswered}
              <Trans>
                It's on your Home Screen and working. Open it once and
                notifications are on for this identity.
              </Trans>
            {:else}
              <Trans>
                It's on your Home Screen. Open it once and notifications are on
                for this identity.
              </Trans>
            {/if}
          </p>
          <div class={buttonGroup}>
            <button class="btn btn-tertiary btn-xl" onclick={startOver}>
              {$t`I don't have the app any more`}
            </button>
          </div>
        </div>
      {:else if screen.kind === "ask"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch">
          <div class={centredBlock}>
            <NotificationMock
              title={$t`OISY Wallet · Payment received`}
              body={$t`You received 12.5 ICP.`}
            />
            <div class={centredText}>
              <h1 class={centredHeading}>
                {$t`Let your apps notify you`}
              </h1>
              <p class={centredBody}>
                <Trans>
                  Payments, votes and messages from the apps you use, right on
                  this device.
                </Trans>
              </p>
            </div>
          </div>
          <div class={actionBlock}>
            <button
              class="btn btn-primary btn-xl w-full"
              onclick={() => void askForPermission()}
            >
              {$t`Allow notifications`}
            </button>
            <p class="text-text-tertiary text-center text-sm">
              <Trans>Only apps you allow on {host} can notify you.</Trans>
            </p>
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
          <div class={buttonGroup}>
            <button
              class="btn btn-primary btn-xl"
              onclick={() => void retryFromSettings()}
            >
              {$t`Try again`}
            </button>
          </div>
        </div>
      {:else if screen.kind === "ready" && screen.inTheApp}
        <div class="flex min-w-0 flex-1 flex-col items-stretch">
          <div class={centredBlock}>
            <NotificationMock
              title={$t`Notifications are on`}
              body={$t`You'll get alerts from your apps here.`}
            />
            <div class={centredText}>
              <h1 class={centredHeading}>
                {$t`You're all set`}
              </h1>
              <p class={centredBody}>
                <Trans>
                  Notifications from your apps will show up on this device.
                </Trans>
              </p>
            </div>
          </div>
          <div class="{actionBlock} pb-24">
            <button
              class="btn btn-tertiary btn-xl w-full"
              onclick={openTheHost}
            >
              <Trans>Manage apps on {host}</Trans>
            </button>
          </div>
        </div>
      {:else if screen.kind === "ready"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
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
          <!-- The probe cannot answer for a browser that is signed out or past the window the canister still wakes it in. -->
          <div class={buttonGroup}>
            <button class="btn btn-tertiary btn-xl" onclick={startOver}>
              {$t`I don't have the app any more`}
            </button>
          </div>
        </div>
      {:else if screen.kind === "unlinked"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <SmartphoneIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Set up notifications`}
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>
              Open {host}, sign in, and turn notifications on. It will show you
              how to add the app.
            </Trans>
          </p>
          <div class={buttonGroup}>
            <button class="btn btn-primary btn-xl" onclick={openTheHost}>
              <Trans>Open {host}</Trans>
            </button>
          </div>
        </div>
      {:else if screen.kind === "stale-link"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <CircleAlertIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`This link didn't work`}
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>
              Remove this app from your Home Screen, then open {host} and turn notifications
              on again to add it back.
            </Trans>
          </p>
          <div class={buttonGroup}>
            <button class="btn btn-primary btn-xl" onclick={openTheHost}>
              <Trans>Open {host}</Trans>
            </button>
          </div>
        </div>
      {:else if screen.kind === "open-to-finish"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <SmartphoneIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Open it once to finish`}
          </h1>
          <p class="text-text-tertiary text-base">
            <Trans>
              {NOTIFICATION_APP_NAME} is on your Home Screen but has not been opened
              yet. Open it once and notifications are on for this identity.
            </Trans>
          </p>
        </div>
      {:else if screen.kind === "link-expired"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <CircleAlertIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`This link has expired`}
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>
              An app added from it cannot finish setting up. Remove it from your
              Home Screen and add it again from here.
            </Trans>
          </p>
          <div class={buttonGroup}>
            <button
              class="btn btn-primary btn-xl"
              onclick={() => void startOver()}
            >
              {$t`Add it again`}
            </button>
          </div>
        </div>
      {:else if screen.kind === "unusable-key"}
        <div class="flex min-w-0 flex-1 flex-col items-stretch justify-end">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <CircleAlertIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Add this app again`}
          </h1>
          <p class="text-text-tertiary mb-5 text-base">
            <Trans>
              This app can't be connected to your identity. Adding it again
              gives it what it needs:
            </Trans>
          </p>
          <ReinstallSteps {host} />
          <div class={buttonGroup}>
            <button class="btn btn-primary btn-xl" onclick={openTheHost}>
              <Trans>Open {host}</Trans>
            </button>
          </div>
        </div>
      {:else if screen.kind === "closed"}
        <div class="flex min-w-0 flex-col items-stretch">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <SmartphoneIcon class="size-6" aria-hidden="true" />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`You can close this tab`}
          </h1>
          <p class="text-text-tertiary text-base">
            <Trans
              >Open {host} whenever you want to turn notifications on.</Trans
            >
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
          <ReinstallSteps {host} />
          <div class={buttonGroup}>
            <button class="btn btn-primary btn-xl" onclick={openTheHost}>
              <Trans>Open {host}</Trans>
            </button>
          </div>
        </div>
      {:else}
        <div class="flex min-w-0 flex-col items-stretch">
          <FeaturedIcon size="lg" class="mb-4 self-start">
            <CircleAlertIcon
              class="text-fg-error-primary size-6"
              aria-hidden="true"
            />
          </FeaturedIcon>
          <h1 class="text-text-primary mb-3 text-2xl font-medium">
            {$t`Something went wrong`}
          </h1>
          <p class="text-text-tertiary text-base text-pretty">
            <Trans>
              Notifications couldn't be set up on this device. Turn them off and
              on again on {host} to retry.
            </Trans>
          </p>
          <p
            class="bg-bg-secondary border-border-secondary text-text-tertiary mt-5 rounded-lg border px-3.5 py-3 font-mono text-xs/4 break-words"
          >
            {screen.reason}
          </p>
          <div class={buttonGroup}>
            <button class="btn btn-primary btn-xl" onclick={openTheHost}>
              <Trans>Open {host} settings</Trans>
              <ExternalLinkIcon
                class="size-4.5 opacity-70"
                aria-hidden="true"
              />
            </button>
            <button
              class="btn btn-tertiary btn-xl"
              onclick={() => void retryLaunch()}
            >
              {$t`Try again`}
            </button>
          </div>
        </div>
      {/if}
    </AuthPanel>
  </div>
</div>
