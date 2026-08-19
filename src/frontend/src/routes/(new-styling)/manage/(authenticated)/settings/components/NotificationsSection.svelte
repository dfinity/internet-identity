<script lang="ts">
  import { onMount } from "svelte";
  import { BellIcon, GlobeIcon, Trash2Icon } from "@lucide/svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { t } from "$lib/stores/locale.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { isPushSupported } from "$lib/utils/notifications/pushSubscription";
  import {
    currentDeviceSubscription,
    enableDeviceNotifications,
    disableAllNotifications,
    unsubscribeDevice,
    revokeApp,
  } from "$lib/utils/notifications/deviceNotifications";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();

  const pushSupported = typeof navigator !== "undefined" && isPushSupported();

  let browserSubscribed = $state(false);
  let deviceStatusLoaded = $state(false);
  let busy = $state(false);

  // `undefined` while the first read is in flight.
  let origins = $state<string[]>();
  let revoking = $state<string>();

  // Notifications are "on" only when this browser holds a subscription AND at
  // least one app is allowed: either alone can't deliver anything.
  const on = $derived(browserSubscribed && (origins?.length ?? 0) > 0);

  // The switch is driven separately from `on` so a failed toggle can snap it
  // back: the user's click flips `switchOn`, and `syncSwitch()` (run after every
  // load and every action) resets it to the true state, reverting on failure.
  let switchOn = $state(false);
  const syncSwitch = () => {
    switchOn = browserSubscribed && (origins?.length ?? 0) > 0;
  };

  const errorDetail = (err: unknown): string =>
    err instanceof Error ? err.message : String(err);

  const dapps = getDapps();
  const appName = (origin: string): string =>
    dapps.find((dapp) => dapp.hasOrigin(origin))?.name ?? origin;

  onMount(() => {
    if (!pushSupported) {
      deviceStatusLoaded = true;
      return;
    }
    void (async () => {
      try {
        browserSubscribed = (await currentDeviceSubscription()) !== undefined;
      } catch {
        browserSubscribed = false;
      } finally {
        deviceStatusLoaded = true;
        syncSwitch();
      }
    })();
    void (async () => {
      try {
        origins =
          await $authenticatedStore.actor.notification_consented_origins(
            identityNumber,
          );
      } catch {
        origins = [];
      } finally {
        syncSwitch();
      }
    })();
  });

  const handleToggle = async (event: Event) => {
    if (!(event.currentTarget instanceof HTMLInputElement)) {
      return;
    }
    if (busy) {
      return;
    }
    const turnOn = event.currentTarget.checked;
    busy = true;
    try {
      if (turnOn) {
        const result = await enableDeviceNotifications(
          identityNumber,
          $authenticatedStore.actor,
        );
        if (result.status === "permission-denied") {
          toaster.error({
            title:
              typeof Notification !== "undefined" &&
              Notification.permission === "denied"
                ? $t`Notifications are blocked for this site. Allow them in your browser settings, then try again.`
                : $t`Notification permission was not granted.`,
            duration: 6000,
          });
          return;
        }
        browserSubscribed = true;
      } else {
        await disableAllNotifications(
          identityNumber,
          $authenticatedStore.actor,
        );
        browserSubscribed = false;
        origins = [];
      }
    } catch (err) {
      toaster.error({
        title: $t`Couldn't change notifications on this device. Please try again.`,
        description: errorDetail(err),
        duration: 8000,
      });
    } finally {
      // Reconcile the switch to the true state — reverts it if the change above
      // failed or was denied.
      syncSwitch();
      busy = false;
    }
  };

  const remove = async (origin: string) => {
    revoking = origin;
    const previous = origins ?? [];
    const next = previous.filter((consented) => consented !== origin);
    origins = next;
    try {
      await revokeApp(identityNumber, $authenticatedStore.actor, origin);
      // Removing the last allowed app drops the browser subscription too:
      // nothing is left to deliver to it.
      if (next.length === 0) {
        await unsubscribeDevice(identityNumber, $authenticatedStore.actor);
        browserSubscribed = false;
      }
    } catch {
      origins = previous;
      toaster.error({
        title: $t`Couldn't turn off notifications. Please try again.`,
        duration: 4000,
      });
    } finally {
      revoking = undefined;
      syncSwitch();
    }
  };
</script>

<section
  class="border-border-secondary bg-bg-secondary flex flex-col rounded-xl border p-4 sm:p-5"
>
  <div class="flex flex-row items-start gap-3 sm:gap-4">
    <span
      class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
      aria-hidden="true"
    >
      <BellIcon class="size-5" />
    </span>

    <div class="flex min-w-0 flex-1 flex-col gap-1">
      <div
        class="flex min-h-[1.5rem] flex-row flex-wrap items-center gap-x-2 gap-y-1"
      >
        <h3 id={titleId} class="text-text-primary text-base font-semibold">
          {$t`Notifications`}
        </h3>
        {#if deviceStatusLoaded && on}
          <Badge color="success" size="sm" dot>
            {$t`On`}
          </Badge>
        {/if}
      </div>
      <p class="text-text-tertiary text-sm">
        {$t`dApps you've allowed can send push notifications to this device.`}
      </p>
    </div>

    <div class="flex h-6 shrink-0 items-center">
      {#if pushSupported}
        <Toggle
          bind:checked={switchOn}
          onchange={handleToggle}
          disabled={busy || !deviceStatusLoaded}
          aria-labelledby={titleId}
        />
      {/if}
    </div>
  </div>

  {#if !pushSupported}
    <div class="border-border-tertiary mt-5 border-t pt-4">
      <p class="text-text-tertiary text-sm">
        {$t`This browser does not support push notifications.`}
      </p>
    </div>
  {:else if on}
    <div class="border-border-tertiary mt-5 border-t pt-4">
      <p
        class="text-text-tertiary mb-3 text-xs font-semibold tracking-wide uppercase"
      >
        {$t`Allowed apps`}
      </p>
      <ul class="flex flex-col gap-2" aria-labelledby={titleId}>
        {#each origins ?? [] as origin (origin)}
          <li
            class="border-border-secondary flex flex-row items-center gap-3 rounded-lg border px-3 py-3 sm:px-4"
          >
            <span
              class="border-border-secondary bg-bg-secondary text-fg-tertiary flex size-9 shrink-0 items-center justify-center rounded-md border"
              aria-hidden="true"
            >
              <GlobeIcon class="size-4.5" />
            </span>
            <div class="flex min-w-0 flex-1 flex-col">
              <span class="text-text-primary truncate text-sm font-semibold">
                {appName(origin)}
              </span>
              <span
                class="text-text-tertiary truncate font-mono text-xs"
                title={origin}
              >
                {origin}
              </span>
            </div>
            <button
              class="btn btn-tertiary btn-sm btn-icon shrink-0"
              onclick={() => remove(origin)}
              disabled={revoking === origin}
              aria-label={$t`Remove ${appName(origin)}`}
            >
              <Trash2Icon class="size-4.5" />
            </button>
          </li>
        {/each}
      </ul>
    </div>
  {:else}
    <div class="border-border-tertiary mt-5 border-t pt-4">
      <p class="text-text-tertiary text-sm">
        {$t`No apps can send you notifications.`}
      </p>
    </div>
  {/if}
</section>
