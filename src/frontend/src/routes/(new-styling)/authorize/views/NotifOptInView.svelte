<script lang="ts">
  import { BellOffIcon } from "@lucide/svelte";
  import type { ActorSubclass } from "@icp-sdk/core/agent";
  import type { _SERVICE } from "$lib/generated/internet_identity_types";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import NotifEnablePitch from "./NotifEnablePitch.svelte";
  import NotifInstallHandoff from "./NotifInstallHandoff.svelte";
  import {
    openInstallTab,
    prepareInstallUrl,
  } from "./startNotificationInstall";
  import NotifBlockedSteps from "./notifBlocked/NotifBlockedSteps.svelte";
  import { turnOnNotifications } from "$lib/utils/notifications/enableNotifications";
  import {
    readBrowserPushState,
    readDeviceState,
    watchNotificationPermission,
    type DeviceNotificationState,
  } from "$lib/utils/notifications/notificationState";
  import {
    clearFailure,
    recordFailure,
  } from "$lib/utils/notifications/notificationDiagnostics";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { isCanisterError } from "$lib/utils/utils";
  import type { NotificationConsentOutcome } from "$lib/stores/notificationConsent.store";

  interface Props {
    /** dApp name for the copy, or undefined when it isn't known. */
    appName: string | undefined;
    /** Its published logo, for the notifications the enable screen previews. */
    appLogo: string | undefined;
    identityNumber: bigint;
    origin: string;
    /** The authenticated actor for this identity. */
    actor: ActorSubclass<_SERVICE>;
    /** This browser as the resolution found it, so answering only does what is left.
     *  Not named `state`, which would read as the `$state` rune in this file. */
    device: DeviceNotificationState;
    /** Whether this app already holds consent from this identity. */
    consented: boolean;
    /** Answering sends the user to install the app that carries notifications, because
     *  this browser has no prompt to raise. The screen is the same; Allow differs. */
    installFirst: boolean;
    /** Continues sign-in: after enabling, allowing, skipping, or handing the user to
     *  the install. The outcome is only carried where the canister cannot be asked. */
    onDone: (outcome?: NotificationConsentOutcome) => void;
  }

  const {
    appName,
    appLogo,
    identityNumber,
    origin,
    actor,
    device,
    consented,
    installFirst,
    onDone,
  }: Props = $props();

  // Always opens on the ask, so the unblock guidance is only ever reached by asking
  // and being refused, with the reason for the question already on screen. A new
  // request arrives as a new context, which remounts this component.
  let variant = $state<"enable" | "blocked" | "handoff">("enable");

  /** Signed while this screen renders, because `window.open` needs the click's own
   *  gesture and an await between the two loses it. */
  let installUrl = $state<string | undefined>(undefined);
  $effect(() => {
    if (!installFirst) {
      return;
    }
    void prepareInstallUrl(identityNumber).then((url) => {
      installUrl = url;
    });
  });

  /**
   * Hands the user to the install, and settles either way.
   *
   * A tab the browser would not open is not a failure: the design answers it by showing
   * the link, which the user follows themselves. Both roads lead to the same place, so
   * both report the install as started.
   */
  /** The install is under way, which is as much as this browser can know. */
  const onInstallStarted = () => onDone({ installStarted: true });

  const startInstall = () => {
    if (installUrl === undefined) {
      reportFailure(new Error("this browser has not completed a sign-in"));
      return;
    }
    variant = "handoff";
    if (openInstallTab(installUrl)) {
      onInstallStarted();
    }
  };
  let busy = $state(false);

  /**
   * Reports a failure and leaves the user where they are, with the button live again.
   *
   * A canister refusal goes to the shared handler, which knows how to word one.
   * Anything else is the browser's own: the service worker, the push subscription, a
   * key or the store it lives in. Those carry no wording we could improve on, so the
   * message is shown as it came, which makes a screenshot enough to act on.
   */
  const reportFailure = (error: unknown) => {
    if (isCanisterError(error)) {
      recordFailure("register-failed", messageOf(error));
      handleError(error);
      return;
    }
    recordFailure("subscribe-failed", messageOf(error));
    toaster.error({
      title: $t`Notifications unavailable`,
      description: messageOf(error),
    });
  };

  const messageOf = (error: unknown): string =>
    error instanceof Error ? error.message : String(error);

  /** Runs what is left to do for `current`, which a retry reads afresh. */
  const runEnable = async (current: DeviceNotificationState): Promise<void> => {
    busy = true;
    try {
      const result = await turnOnNotifications({
        identityNumber,
        origin,
        actor,
        device: current,
        consented,
      });
      if (result.status === "denied") {
        recordFailure("permission-denied");
        variant = "blocked";
        return;
      }
      if (result.status === "dismissed") {
        // The permission is still `default`, so the prompt can be raised again.
        // Stay where the user is, with Allow still in front of them.
        return;
      }
      clearFailure();
      onDone();
    } catch (error) {
      reportFailure(error);
    } finally {
      busy = false;
    }
  };

  /**
   * Picks up a block the user has lifted in browser settings.
   *
   * Which way out depends on what they lifted it to. Chrome's switch grants outright,
   * and there is nothing left to ask, so the rest is finished for them. Firefox's
   * route is to clear the block, which returns the permission to `default`: a prompt
   * can be raised again, but only off a gesture, so they land back on the ask with
   * Allow in front of them rather than on a prompt they never asked for.
   *
   * Either way this leaves the guidance, so a refusal from here installs a fresh
   * watcher through the screen it lands on.
   */
  const resumeAfterUnblock = async (): Promise<void> => {
    busy = true;
    try {
      const pushState = await readBrowserPushState();
      if (pushState === undefined || pushState.permission === "denied") {
        return;
      }
      const current = await readDeviceState(identityNumber, pushState);
      variant = "enable";
      busy = false;
      if (pushState.permission !== "granted") {
        return;
      }
      await runEnable(current);
    } catch (error) {
      reportFailure(error);
    } finally {
      busy = false;
    }
  };

  // Watches only while the guidance is up, and only for as long as it is: the enable
  // screen raises the prompt itself, and a watcher left running would answer for a
  // screen the user has already left. Keyed on the variant, so landing back on the
  // guidance after a refusal installs a watcher again rather than stranding the user
  // there with nothing but "Not now".
  $effect(() => {
    if (variant !== "blocked") {
      return;
    }
    return watchNotificationPermission(() => void resumeAfterUnblock());
  });
</script>

{#if variant === "enable"}
  <NotifEnablePitch
    {appName}
    {appLogo}
    {origin}
    {busy}
    onEnable={installFirst ? startInstall : () => void runEnable(device)}
    onSkip={() => onDone()}
  />
{:else if variant === "handoff"}
  <NotifInstallHandoff url={installUrl} onDone={onInstallStarted} />
{:else}
  <!-- No app header: this screen is about the browser's own settings, not about the
       app that asked, and the design gives it the panel to itself. -->
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

    <NotifBlockedSteps />

    <div class="mt-7 flex flex-col gap-2.5">
      <button
        class="btn btn-tertiary btn-xl"
        onclick={() => onDone()}
        disabled={busy}
      >
        {$t`Not now`}
      </button>
    </div>
  </div>
{/if}
