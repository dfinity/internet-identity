<script lang="ts">
  import { onMount } from "svelte";
  import {
    BellOffIcon,
    SmartphoneIcon,
    TriangleAlertIcon,
    Loader2Icon,
  } from "@lucide/svelte";
  import type { ActorSubclass } from "@icp-sdk/core/agent";
  import type {
    NotificationError,
    _SERVICE,
  } from "$lib/generated/internet_identity_types";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { isCanisterError } from "$lib/utils/utils";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import NotifEnablePitch from "./NotifEnablePitch.svelte";
  import NotifUnblockSteps from "$lib/components/notifications/NotifUnblockSteps.svelte";
  import {
    enableNotifications,
    allowApp,
  } from "$lib/utils/notifications/enableNotifications";
  import {
    readDeviceState,
    resolveOptInScreen,
    type OptInScreen,
  } from "$lib/utils/notifications/notificationState";
  import {
    clearFailure,
    detectBrowser,
    recordDeclined,
    recordFailure,
    recordPermission,
    type BrowserKind,
    type FailureReason,
  } from "$lib/utils/notifications/notificationDiagnostics";

  interface Props {
    /** dApp name for the copy, or undefined when it isn't known. */
    appName: string | undefined;
    identityNumber: bigint;
    origin: string;
    /** Resolves the authenticated actor for this identity. */
    resolveActor: () => Promise<ActorSubclass<_SERVICE> | undefined>;
    /** Continues sign-in: after enabling, allowing, skipping, or when there is
     * nothing worth showing. */
    onDone: () => void;
  }

  const { appName, identityNumber, origin, resolveActor, onDone }: Props =
    $props();

  const app = $derived(appName ?? $t`this app`);

  type Variant = "loading" | OptInScreen | "failed";
  let variant = $state<Variant>("loading");
  let busy = $state(false);
  let browser = $state<BrowserKind>("other");
  let actor: ActorSubclass<_SERVICE> | undefined;
  // A retry from the failed screen sets this device up, or only records consent
  // when the device is already registered. Read by the failed screen's copy.
  let retrySubscribes = $state(true);

  onMount(() => {
    void (async () => {
      try {
        actor = await resolveActor();
        if (actor === undefined) {
          onDone();
          return;
        }
        const consented = await actor
          .notification_consent_status(identityNumber, origin)
          .catch(() => false);
        const state = await readDeviceState(identityNumber, actor);
        recordPermission(state.permission);
        const screen = resolveOptInScreen(state, origin, consented);
        if (screen === "skip") {
          onDone();
          return;
        }
        browser = detectBrowser();
        retrySubscribes = screen !== "allow-app";
        variant = screen;
      } catch (err) {
        // The request is waiting on this window, so a rejection here has to land
        // on a screen the user can answer rather than leaving it spinning.
        const message = messageOf(err);
        recordFailure(classify(err, "subscribe-failed"), message);
        variant = "failed";
      }
    })();
  });

  const messageOf = (err: unknown): string =>
    err instanceof Error ? err.message : String(err);

  const classify = (err: unknown, fallback: FailureReason): FailureReason =>
    isCanisterError<NotificationError>(err) && err.type === "Disabled"
      ? "backend-disabled"
      : fallback;

  const runSubscribe = async (): Promise<void> => {
    if (actor === undefined) {
      onDone();
      return;
    }
    busy = true;
    try {
      const result = await enableNotifications({
        identityNumber,
        origin,
        actor,
      });
      if (result.status === "denied") {
        recordFailure("permission-denied");
        browser = detectBrowser();
        variant = "blocked";
        return;
      }
      if (result.status === "dismissed") {
        // The permission is still `default`, so the prompt can be raised again.
        // Stay where the user is, with Enable still in front of them.
        return;
      }
      clearFailure();
      onDone();
    } catch (err) {
      const message = messageOf(err);
      recordFailure(classify(err, "subscribe-failed"), message);
      // The registration may have landed before the consent failed, so a retry
      // that sets the device up again would drop a working endpoint.
      retrySubscribes = !(await readDeviceState(identityNumber, actor))
        .subscribed;
      variant = "failed";
    } finally {
      busy = false;
    }
  };

  const runAllow = async (): Promise<void> => {
    if (actor === undefined) {
      onDone();
      return;
    }
    busy = true;
    try {
      await allowApp({
        identityNumber,
        origin,
        actor,
      });
      clearFailure();
      onDone();
    } catch (err) {
      const message = messageOf(err);
      recordFailure(classify(err, "register-failed"), message);
      variant = "failed";
    } finally {
      busy = false;
    }
  };

  const handleSkip = () => {
    recordDeclined(origin);
    onDone();
  };

  const handleRetry = () => {
    void (retrySubscribes ? runSubscribe() : runAllow());
  };
</script>

{#if variant === "loading"}
  <div class="flex flex-1 items-center justify-center p-4">
    <Loader2Icon
      class="text-text-tertiary size-6 animate-spin"
      aria-label={$t`Loading`}
    />
  </div>
{:else if variant === "first-time"}
  <NotifEnablePitch
    {appName}
    {origin}
    {busy}
    onEnable={() => void runSubscribe()}
    onSkip={handleSkip}
  />
{:else}
  <div
    class="flex flex-1 flex-col items-stretch p-4 sm:max-w-100 sm:justify-center sm:self-center"
  >
    <AuthorizeHeader {origin} />
    <div class="flex flex-col justify-center">
      {#if variant === "new-device"}
        <span
          class="border-border-secondary bg-bg-secondary text-text-primary mb-6 flex size-12 items-center justify-center rounded-full border"
        >
          <SmartphoneIcon class="size-6" aria-hidden="true" />
        </span>
        <h1 class="text-text-primary text-2xl font-medium text-balance">
          {$t`Get ${app} notifications on this device`}
        </h1>
        <p class="text-text-secondary mt-2 text-sm">
          <Trans>
            You allowed this app on another device. Turn this device on to
            receive them here too.
          </Trans>
        </p>
      {:else if variant === "allow-app"}
        <span
          class="border-border-secondary bg-bg-secondary text-text-primary mb-6 flex size-12 items-center justify-center rounded-full border"
        >
          <SmartphoneIcon class="size-6" aria-hidden="true" />
        </span>
        <h1 class="text-text-primary text-2xl font-medium text-balance">
          {$t`Turn on notifications from ${app}?`}
        </h1>
        <p class="text-text-secondary mt-2 text-sm">
          <Trans>
            This device is already set up for notifications. Allow this app to
            send them too.
          </Trans>
        </p>
      {:else if variant === "blocked"}
        <span
          class="border-border-secondary bg-bg-secondary text-text-primary mb-6 flex size-12 items-center justify-center rounded-full border"
        >
          <BellOffIcon class="size-6" aria-hidden="true" />
        </span>
        <h1 class="text-text-primary text-2xl font-medium text-balance">
          {$t`Notifications are turned off for Internet Identity`}
        </h1>
        <p class="text-text-secondary mt-2 mb-4 text-sm">
          <Trans>
            Your browser is blocking notifications for this site, so this app
            can't reach you here. Turn them back on in your browser settings,
            then try again.
          </Trans>
        </p>
        <NotifUnblockSteps {browser} />
      {:else if variant === "failed"}
        <span
          class="border-border-secondary bg-bg-secondary text-text-primary mb-6 flex size-12 items-center justify-center rounded-full border"
        >
          <TriangleAlertIcon class="size-6" aria-hidden="true" />
        </span>
        <h1 class="text-text-primary text-2xl font-medium text-balance">
          {$t`Couldn't turn on notifications`}
        </h1>
        <p class="text-text-secondary mt-2 text-sm">
          {#if retrySubscribes}
            <Trans>
              Something went wrong setting up this device. You can try again
              now, or set it up later in Settings.
            </Trans>
          {:else}
            <Trans>
              Something went wrong allowing this app. You can try again now, or
              allow it later in Settings.
            </Trans>
          {/if}
        </p>
      {/if}
    </div>

    <div class="mt-7 flex flex-col gap-2.5">
      {#if variant === "new-device"}
        <button
          class="btn btn-primary"
          onclick={() => void runSubscribe()}
          disabled={busy}
        >
          {busy ? $t`Setting up…` : $t`Enable on this device`}
        </button>
        <button class="btn btn-tertiary" onclick={handleSkip} disabled={busy}>
          {$t`Maybe later`}
        </button>
      {:else if variant === "allow-app"}
        <button
          class="btn btn-primary"
          onclick={() => void runAllow()}
          disabled={busy}
        >
          {busy ? $t`Setting up…` : $t`Allow ${app}`}
        </button>
        <button class="btn btn-tertiary" onclick={handleSkip} disabled={busy}>
          {$t`Not now`}
        </button>
      {:else if variant === "blocked"}
        <button class="btn btn-tertiary" onclick={handleSkip}>
          {$t`Continue without`}
        </button>
      {:else if variant === "failed"}
        <button class="btn btn-primary" onclick={handleRetry} disabled={busy}>
          {busy ? $t`Setting up…` : $t`Try again`}
        </button>
        <button class="btn btn-tertiary" onclick={onDone} disabled={busy}>
          {$t`Continue`}
        </button>
      {/if}
    </div>
  </div>
{/if}
