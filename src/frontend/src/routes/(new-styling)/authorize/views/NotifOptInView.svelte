<script lang="ts">
  import { BellOffIcon } from "@lucide/svelte";
  import type { ActorSubclass } from "@icp-sdk/core/agent";
  import type { _SERVICE } from "$lib/generated/internet_identity_types";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import NotifEnablePitch from "./NotifEnablePitch.svelte";
  import { startAppInstall } from "./notifInstall";
  import NotifBlockedSteps from "./notifBlocked/NotifBlockedSteps.svelte";
  import {
    turnOnNotifications,
    type EnableNotificationsResult,
  } from "$lib/utils/notifications/enableNotifications";
  import {
    readBrowserPushState,
    readDeviceState,
    watchNotificationPermission,
    type DeviceNotificationState,
  } from "$lib/utils/notifications/notificationState";
  import {
    clearFailure,
    failureMessage,
    recordFailure,
  } from "$lib/utils/notifications/notificationDiagnostics";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { isCanisterError } from "$lib/utils/utils";

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
    /** Answering sends the user to install the app that carries notifications, because this browser has no prompt to raise. */
    installFirst: boolean;
    /** Continues sign-in: after enabling, allowing, skipping, or handing the user to the install. */
    onDone: () => void;
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
  let variant = $state<"enable" | "blocked">("enable");

  /** Whether answering installs the app rather than claiming the one this device has. Moves only one way: a claim that finds nothing published leaves the install as what is left. */
  let needsInstall = $state(installFirst);

  /** The consent this press records, awaited before sign-in hands back so a grant still in flight would not have the app told no. */
  let granting = $state<Promise<void> | undefined>(undefined);

  /** True while a press is still doing what it started. */
  let busy = $state(false);

  /** The steps are walked in a tab of their own and this one settles sign-in behind
   *  them, so the press leaves nothing else on screen.
   *
   *  Settling is what hands sign-in back, which takes this page with it, and the tab
   *  that was opened is pointed at the install from here: settling first would tear the
   *  page down with the URL still being minted and leave that tab on its placeholder for
   *  good. So the mint is waited for, and only the handing back comes after it. */
  const startInstall = () => {
    if (busy) {
      return;
    }
    busy = true;
    const started = startAppInstall({
      identityNumber,
      origin,
      actor,
      consented,
    });
    granting = started.granted;
    // Either way: a mint that failed closes the tab it opened, and sign-in still settles.
    void started.url.then(
      () => settleSignIn(),
      () => settleSignIn(),
    );
  };

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
      recordFailure("register-failed", failureMessage(error));
      handleError(error);
      return;
    }
    recordFailure("subscribe-failed", failureMessage(error));
    toaster.error({
      title: $t`Notifications unavailable`,
      description: failureMessage(error),
    });
  };

  /** The app is told whether it may notify this identity as sign-in hands back, so a grant still in flight would have it told no. */
  const settleSignIn = async (): Promise<void> => {
    try {
      await granting;
      clearFailure();
      onDone();
    } catch (error) {
      variant = "enable";
      reportFailure(error);
    } finally {
      busy = false;
    }
  };

  /** Every arm answers and none falls through, so a status this screen has never seen cannot reach the path that settles sign-in. */
  const enabledBy = (result: EnableNotificationsResult): boolean => {
    switch (result.status) {
      case "enabled":
        return true;
      case "denied":
        recordFailure("permission-denied");
        variant = "blocked";
        return false;
      case "unanswered":
        // Nothing was read about this device's app, so the reason is shown and Allow stays in front of the user.
        toaster.error({
          title: $t`Notifications unavailable`,
          description: result.reason,
        });
        return false;
      case "install-required":
        // The app published nothing this browser could read, so the install is what answering means.
        needsInstall = true;
        return false;
      case "dismissed":
        // The permission is still `default`, so the prompt can be raised again.
        // Stay where the user is, with Allow still in front of them.
        return false;
    }
  };

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
      if (!enabledBy(result)) {
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
    onEnable={needsInstall ? startInstall : () => void runEnable(device)}
    onSkip={() => onDone()}
  />
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
