<script lang="ts">
  import { onMount } from "svelte";
  import { BellIcon, XIcon, SmartphoneIcon } from "@lucide/svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { bufFromBufLike } from "$lib/utils/utils";
  import {
    listConsentedOrigins,
    revokeConsent,
    getVapidPublicKey,
    subscribeDevice,
    unsubscribeDevice,
  } from "$lib/utils/pushConsent";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();
  const deviceTitleId = `${titleId}-device`;

  let origins = $state<string[]>([]);
  // True until the initial list read completes.
  let loaded = $state(false);
  // The origin currently being revoked, if any — disables just that row's
  // button rather than the whole list.
  let revoking = $state<string | undefined>(undefined);

  // Whether the browser can support Web Push at all — false in unsupported
  // browsers or insecure contexts, in which case the device toggle is hidden.
  const devicePushSupported =
    typeof navigator !== "undefined" &&
    "serviceWorker" in navigator &&
    "PushManager" in window;

  let deviceSubscription = $state<PushSubscription | undefined>(undefined);
  // True until the initial subscription-status check completes.
  let deviceStatusLoaded = $state(false);
  // A subscribe/unsubscribe round-trip is in flight.
  let deviceBusy = $state(false);

  onMount(() => {
    void (async () => {
      try {
        origins = await listConsentedOrigins(
          $authenticatedStore.actor,
          identityNumber,
        );
      } catch (err) {
        // Log the raw error so a stale canister deploy or an IDL-cache
        // mismatch surfaces something concrete instead of a bland toast.
        console.error("push_list_consented_origins failed:", err);
        toaster.error({
          title: $t`Couldn't load your notification permissions.`,
          duration: 4000,
        });
      } finally {
        loaded = true;
      }
    })();

    if (!devicePushSupported) {
      deviceStatusLoaded = true;
      return;
    }
    void (async () => {
      try {
        const registration = await navigator.serviceWorker.getRegistration();
        deviceSubscription =
          (await registration?.pushManager.getSubscription()) ?? undefined;
      } catch (err) {
        console.error("push subscription status check failed:", err);
      } finally {
        deviceStatusLoaded = true;
      }
    })();
  });

  const handleRevoke = async (origin: string) => {
    revoking = origin;
    try {
      await revokeConsent($authenticatedStore.actor, identityNumber, origin);
      origins = origins.filter((entry) => entry !== origin);
    } catch (err) {
      console.error("push_revoke_consent failed:", err);
      toaster.error({
        title: $t`Couldn't remove this permission. Please try again.`,
        duration: 4000,
      });
    } finally {
      revoking = undefined;
    }
  };

  const handleEnableDevice = async () => {
    deviceBusy = true;
    try {
      const permission = await Notification.requestPermission();
      if (permission !== "granted") {
        toaster.error({
          title: $t`Notification permission was not granted.`,
          duration: 4000,
        });
        return;
      }

      const registration =
        await navigator.serviceWorker.register("/service-worker.js");
      await navigator.serviceWorker.ready;

      const vapidPublicKey = await getVapidPublicKey(
        $authenticatedStore.actor,
      );
      const subscription = await registration.pushManager.subscribe({
        userVisibleOnly: true,
        applicationServerKey: bufFromBufLike(vapidPublicKey),
      });
      const { endpoint, keys } = subscription.toJSON() as {
        endpoint: string;
        keys: { p256dh: string; auth: string };
      };

      await subscribeDevice(
        $authenticatedStore.actor,
        identityNumber,
        endpoint,
        keys.p256dh,
        keys.auth,
      );
      deviceSubscription = subscription;
    } catch (err) {
      console.error("enable push notifications failed:", err);
      toaster.error({
        title: $t`Couldn't enable notifications on this device. Please try again.`,
        duration: 4000,
      });
    } finally {
      deviceBusy = false;
    }
  };

  const handleDisableDevice = async () => {
    const subscription = deviceSubscription;
    if (subscription === undefined) {
      return;
    }
    deviceBusy = true;
    try {
      await unsubscribeDevice(
        $authenticatedStore.actor,
        identityNumber,
        subscription.endpoint,
      );
      await subscription.unsubscribe();
      deviceSubscription = undefined;
    } catch (err) {
      console.error("disable push notifications failed:", err);
      toaster.error({
        title: $t`Couldn't disable notifications on this device. Please try again.`,
        duration: 4000,
      });
    } finally {
      deviceBusy = false;
    }
  };
</script>

<section
  class="border-border-secondary bg-bg-secondary flex flex-col gap-4 rounded-xl border p-4 sm:p-5"
>
  <div class="flex flex-row items-start gap-3 sm:gap-4">
    <span
      class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
      aria-hidden="true"
    >
      <BellIcon class="size-5" />
    </span>

    <div class="flex min-h-[2.5rem] min-w-0 flex-1 flex-col justify-center">
      <h3 id={titleId} class="text-text-primary text-base font-semibold">
        {$t`Push notifications`}
      </h3>
      <p class="text-text-tertiary text-sm">
        <Trans>Apps you've allowed to send you push notifications.</Trans>
      </p>
    </div>
  </div>

  {#if devicePushSupported}
    <div
      class="border-border-secondary flex flex-col gap-3 rounded-lg border px-3 py-2.5 sm:px-4 sm:py-3"
    >
      <div class="flex flex-row flex-wrap items-center gap-x-2 gap-y-1">
        <span
          class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-8 shrink-0 items-center justify-center rounded-lg border"
          aria-hidden="true"
        >
          <SmartphoneIcon class="size-4" />
        </span>
        <span
          id={deviceTitleId}
          class="text-text-primary min-w-0 flex-1 text-sm font-semibold"
        >
          {$t`Notifications on this device`}
        </span>
        {#if deviceStatusLoaded && deviceSubscription !== undefined}
          <Badge color="success" size="sm" dot>
            {$t`Enabled`}
          </Badge>
        {/if}
      </div>

      <p class="text-text-tertiary text-sm">
        <Trans>
          When enabled, dApps you've granted permission to can send you push
          notifications on this device.
        </Trans>
      </p>

      {#if !deviceStatusLoaded}
        <div class="flex items-center justify-start py-1">
          <ProgressRing class="text-fg-tertiary size-5" />
        </div>
      {:else if deviceSubscription !== undefined}
        <button
          class="btn btn-secondary btn-sm btn-danger w-full sm:w-auto sm:self-start"
          onclick={() => void handleDisableDevice()}
          disabled={deviceBusy}
          aria-labelledby={deviceTitleId}
        >
          {#if deviceBusy}
            <ProgressRing class="size-5" />
          {/if}
          <span>{$t`Disable`}</span>
        </button>
      {:else}
        <button
          class="btn btn-secondary btn-sm w-full sm:w-auto sm:self-start"
          onclick={() => void handleEnableDevice()}
          disabled={deviceBusy}
          aria-labelledby={deviceTitleId}
        >
          {#if deviceBusy}
            <ProgressRing class="size-5" />
          {/if}
          <span>{$t`Enable notifications on this device`}</span>
        </button>
      {/if}
    </div>
  {/if}

  {#if !loaded}
    <div class="flex items-center justify-center py-4">
      <ProgressRing class="text-fg-tertiary size-5" />
    </div>
  {:else if origins.length === 0}
    <p class="text-text-tertiary text-sm">
      {$t`No apps have permission to send you push notifications yet.`}
    </p>
  {:else}
    <ul class="flex flex-col gap-2" aria-labelledby={titleId}>
      {#each origins as origin (origin)}
        <li
          class="border-border-secondary bg-bg-primary flex flex-row items-center gap-2 rounded-lg border px-3 py-2.5 sm:gap-3 sm:px-4"
        >
          <span class="text-text-primary min-w-0 flex-1 text-sm font-medium">
            <Ellipsis text={origin} position="middle" />
          </span>
          <Tooltip label={$t`Revoke permission`}>
            <button
              class="btn btn-tertiary btn-sm btn-icon shrink-0"
              onclick={() => void handleRevoke(origin)}
              disabled={revoking !== undefined}
              aria-label={$t`Revoke permission for ${origin}`}
            >
              {#if revoking === origin}
                <ProgressRing class="size-5" />
              {:else}
                <XIcon class="size-5" />
              {/if}
            </button>
          </Tooltip>
        </li>
      {/each}
    </ul>
  {/if}
</section>
