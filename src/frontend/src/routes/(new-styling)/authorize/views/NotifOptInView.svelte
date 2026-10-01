<script lang="ts">
  import { BellOffIcon } from "@lucide/svelte";
  import type { ActorSubclass } from "@icp-sdk/core/agent";
  import type { _SERVICE } from "$lib/generated/internet_identity_types";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import NotifEnablePitch from "./NotifEnablePitch.svelte";
  import NotifBlockedSteps from "./notifBlocked/NotifBlockedSteps.svelte";
  import { turnOnNotifications } from "$lib/utils/notifications/enableNotifications";
  import {
    readBrowserPushState,
    readDeviceState,
    type DeviceNotificationState,
    type OptInQuestion,
  } from "$lib/utils/notifications/notificationState";
  import {
    clearFailure,
    recordFailure,
  } from "$lib/utils/notifications/notificationDiagnostics";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { isCanisterError } from "$lib/utils/utils";

  interface Props {
    /** dApp name for the copy, or undefined when it isn't known. */
    appName: string | undefined;
    identityNumber: bigint;
    origin: string;
    /** The authenticated actor for this identity. */
    actor: ActorSubclass<_SERVICE>;
    /** The question to ask, resolved before this screen was rendered. */
    screen: OptInQuestion;
    /** This browser as the resolution found it, so answering only does what is left.
     *  Not named `state`, which would read as the `$state` rune in this file. */
    device: DeviceNotificationState;
    /** Whether this app already holds consent from this identity. */
    consented: boolean;
    /** Continues sign-in: after enabling, allowing or skipping. */
    onDone: () => void;
  }

  const {
    appName,
    identityNumber,
    origin,
    actor,
    screen,
    device,
    consented,
    onDone,
  }: Props = $props();

  // Opens on the resolved question and moves on from there: a refused prompt replaces
  // it with the unblock guidance. A new request arrives as a new context, which
  // remounts this component.
  let variant = $state<OptInQuestion>(screen);
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
   * Picks up a permission the user changed in browser settings.
   *
   * Nothing here can re-raise a refused prompt, so this reads the permission again
   * and carries on where it has changed: the user reached this screen by asking for
   * notifications, so a retry continues that rather than asking again. A permission
   * still refused leaves them on the guidance.
   */
  const handleRetry = async (): Promise<void> => {
    busy = true;
    try {
      const pushState = await readBrowserPushState();
      if (pushState === undefined || pushState.permission === "denied") {
        return;
      }
      const current = await readDeviceState(identityNumber, pushState);
      busy = false;
      await runEnable(current);
    } catch (error) {
      reportFailure(error);
    } finally {
      busy = false;
    }
  };
</script>

{#if variant === "enable"}
  <NotifEnablePitch
    {appName}
    {origin}
    {busy}
    onEnable={() => void runEnable(device)}
    onSkip={onDone}
  />
{:else}
  <div
    class="flex flex-1 flex-col items-stretch p-4 sm:max-w-100 sm:justify-center sm:self-center"
  >
    <AuthorizeHeader {origin} />
    <div class="flex flex-col justify-center">
      <span
        class="border-border-secondary bg-bg-secondary text-text-primary mb-6 flex size-12 items-center justify-center rounded-full border"
      >
        <BellOffIcon class="size-6" aria-hidden="true" />
      </span>
      <h1 class="text-text-primary text-2xl font-medium text-balance">
        {$t`Notifications are blocked`}
      </h1>
      <p class="text-text-secondary mt-2 mb-4 text-sm">
        <Trans>Follow these steps to turn them back on:</Trans>
      </p>
      <NotifBlockedSteps />
    </div>

    <div class="mt-7 flex flex-col gap-2.5">
      <button
        class="btn btn-primary"
        onclick={() => void handleRetry()}
        disabled={busy}
      >
        {busy ? $t`Setting up…` : $t`Try again`}
      </button>
      <button class="btn btn-tertiary" onclick={onDone} disabled={busy}>
        {$t`Not now`}
      </button>
    </div>
  </div>
{/if}
