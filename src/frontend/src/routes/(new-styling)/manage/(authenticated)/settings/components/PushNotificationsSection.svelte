<script lang="ts">
  import { onMount } from "svelte";
  import { SmartphoneIcon, GlobeIcon, Trash2Icon } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { toaster } from "$lib/components/utils/toaster";
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

  const devicePushSupported =
    typeof navigator !== "undefined" &&
    "serviceWorker" in navigator &&
    "PushManager" in window;

  let deviceSubscription = $state<PushSubscription | undefined>(undefined);
  let deviceStatusLoaded = $state(false);
  let deviceBusy = $state(false);
  const deviceEnabled = $derived(deviceSubscription !== undefined);

  let origins = $state<string[]>([]);
  let originsLoaded = $state(false);
  let revoking = $state<string | undefined>(undefined);

  const hostOf = (url: string): string => {
    try {
      return new URL(url).host;
    } catch {
      return url;
    }
  };

  onMount(() => {
    void (async () => {
      try {
        origins = await listConsentedOrigins(
          $authenticatedStore.actor,
          identityNumber,
        );
      } catch (err) {
        console.error("push_list_consented_origins failed:", err);
        toaster.error({
          title: $t`Couldn't load your notification permissions.`,
          duration: 4000,
        });
      } finally {
        originsLoaded = true;
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

  const enableDevice = async () => {
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
      const vapidPublicKey = await getVapidPublicKey($authenticatedStore.actor);
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
      const detail =
        err instanceof Error ? `${err.name}: ${err.message}` : String(err);
      toaster.error({
        title: $t`Couldn't enable notifications on this device.`,
        description: detail,
        duration: 15000,
      });
    }
  };

  const disableDevice = async () => {
    const subscription = deviceSubscription;
    if (subscription === undefined) return;
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
    }
  };

  const handleToggle = async (event: Event) => {
    if (!(event.currentTarget instanceof HTMLInputElement)) return;
    if (deviceBusy) return;
    deviceBusy = true;
    try {
      if (event.currentTarget.checked) {
        await enableDevice();
      } else {
        await disableDevice();
      }
    } finally {
      deviceBusy = false;
    }
  };
</script>

{#if devicePushSupported}
  <section
    class="border-border-secondary bg-bg-secondary flex flex-col rounded-xl border p-4 sm:p-5"
  >
    <div class="flex flex-row items-start gap-3 sm:gap-4">
      <span
        class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
        aria-hidden="true"
      >
        <SmartphoneIcon class="size-5" />
      </span>

      <div class="flex min-w-0 flex-1 flex-col gap-1">
        <div
          class="flex min-h-[1.5rem] flex-row flex-wrap items-center gap-x-2 gap-y-1"
        >
          <h3 id={titleId} class="text-text-primary text-base font-semibold">
            {$t`Notifications on this device`}
          </h3>
          {#if deviceStatusLoaded && deviceEnabled}
            <Badge color="success" size="sm" dot>
              {$t`Enabled on this device`}
            </Badge>
          {/if}
        </div>
        <p class="text-text-tertiary text-sm">
          {$t`dApps you've granted permission to can send you push notifications on this device.`}
        </p>
      </div>

      <div class="flex h-6 shrink-0 items-center">
        {#if !deviceStatusLoaded}
          <ProgressRing class="text-fg-tertiary size-5" />
        {:else}
          <Toggle
            checked={deviceEnabled}
            onchange={handleToggle}
            disabled={deviceBusy}
            aria-labelledby={titleId}
          />
        {/if}
      </div>
    </div>

    {#if deviceEnabled}
      <div class="border-border-tertiary mt-5 border-t pt-4">
        <p class="text-text-tertiary mb-3 text-xs font-semibold">
          {$t`Allowed apps`}
        </p>

        {#if !originsLoaded}
          <div class="flex items-center justify-center py-3">
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
                class="border-border-tertiary bg-bg-primary flex flex-row items-center gap-3 rounded-lg border px-3 py-3 sm:px-4"
              >
                <span
                  class="border-border-secondary bg-bg-secondary text-fg-tertiary flex size-10 shrink-0 items-center justify-center rounded-md border"
                  aria-hidden="true"
                >
                  <GlobeIcon class="size-4.5" />
                </span>
                <div class="flex min-w-0 flex-1 flex-col gap-1.5">
                  <span
                    class="text-text-primary truncate text-sm font-semibold"
                  >
                    {hostOf(origin)}
                  </span>
                  <span
                    class="text-text-tertiary truncate font-mono text-xs"
                    title={origin}
                  >
                    {origin}
                  </span>
                </div>
                <Tooltip label={$t`Remove`}>
                  <button
                    class="btn btn-tertiary btn-sm btn-icon shrink-0"
                    onclick={() => void handleRevoke(origin)}
                    disabled={revoking !== undefined}
                    aria-label={$t`Revoke permission for ${origin}`}
                  >
                    {#if revoking === origin}
                      <ProgressRing class="size-5" />
                    {:else}
                      <Trash2Icon class="size-4.5" />
                    {/if}
                  </button>
                </Tooltip>
              </li>
            {/each}
          </ul>
        {/if}
      </div>
    {/if}
  </section>
{/if}
