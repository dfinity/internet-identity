<script lang="ts">
  import { onMount } from "svelte";
  import {
    BellIcon,
    BellOffIcon,
    GlobeIcon,
    Trash2Icon,
    TriangleAlertIcon,
  } from "@lucide/svelte";
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
    unsubscribeDevice,
    revokeApp,
  } from "$lib/utils/notifications/deviceNotifications";
  import {
    clearFailure,
    detectBrowser,
    readDiagnostics,
    recordFailure,
  } from "$lib/utils/notifications/notificationDiagnostics";
  import NotifUnblockSteps from "$lib/components/notifications/NotifUnblockSteps.svelte";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();

  const pushSupported = typeof navigator !== "undefined" && isPushSupported();
  const browser = detectBrowser();
  const isIos = browser === "ios";

  let permission = $state<NotificationPermission>("default");
  let browserSubscribed = $state(false);
  let deviceStatusLoaded = $state(false);
  let busy = $state(false);
  // A reconcile failure recorded on boot; only surfaced while still subscribed.
  let lastFailureReason = $state<string>();

  // `undefined` while the first read is in flight.
  let origins = $state<string[]>();
  let revoking = $state<string>();

  // The toggle owns this browser's subscription. Fully deliverable ("On") also
  // needs at least one allowed app.
  const blocked = $derived(pushSupported && permission === "denied");
  const fullyOn = $derived(browserSubscribed && (origins?.length ?? 0) > 0);
  const needsAttention = $derived(
    browserSubscribed &&
      (lastFailureReason === "subscribe-failed" ||
        lastFailureReason === "register-failed"),
  );

  // The switch is driven separately so a failed toggle can snap back: the click
  // flips `switchOn`, and `syncSwitch()` (after every load and action) resets it
  // to the true subscription state.
  let switchOn = $state(false);
  const syncSwitch = () => {
    switchOn = browserSubscribed;
  };

  const errorDetail = (err: unknown): string =>
    err instanceof Error ? err.message : String(err);

  const dapps = getDapps();
  const appName = (origin: string): string =>
    dapps.find((dapp) => dapp.hasOrigin(origin))?.name ?? origin;

  onMount(() => {
    if (typeof Notification !== "undefined") {
      permission = Notification.permission;
    }
    lastFailureReason = readDiagnostics().lastFailure?.reason;
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
          recordFailure("permission-denied");
          // Re-read so a fresh denial flips the section to its blocked state.
          if (typeof Notification !== "undefined") {
            permission = Notification.permission;
          }
          if (permission !== "denied") {
            toaster.error({
              title: $t`Notification permission wasn't granted.`,
              duration: 6000,
            });
          }
          return;
        }
        browserSubscribed = true;
        lastFailureReason = undefined;
        clearFailure();
      } else {
        await unsubscribeDevice(identityNumber, $authenticatedStore.actor);
        browserSubscribed = false;
      }
    } catch (err) {
      recordFailure("subscribe-failed", errorDetail(err));
      toaster.error({
        title: $t`Couldn't change notifications on this device. Please try again.`,
        description: errorDetail(err),
        duration: 8000,
      });
    } finally {
      syncSwitch();
      busy = false;
    }
  };

  const remove = async (origin: string) => {
    revoking = origin;
    const previous = origins ?? [];
    origins = previous.filter((consented) => consented !== origin);
    try {
      await revokeApp(identityNumber, $authenticatedStore.actor, origin);
    } catch {
      origins = previous;
      toaster.error({
        title: $t`Couldn't remove this app. Please try again.`,
        duration: 4000,
      });
    } finally {
      revoking = undefined;
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
      {#if blocked}
        <BellOffIcon class="size-5" />
      {:else}
        <BellIcon class="size-5" />
      {/if}
    </span>

    <div class="flex min-w-0 flex-1 flex-col gap-1">
      <div
        class="flex min-h-[1.5rem] flex-row flex-wrap items-center gap-x-2 gap-y-1"
      >
        <h3 id={titleId} class="text-text-primary text-base font-semibold">
          {$t`Notifications`}
        </h3>
        {#if deviceStatusLoaded && fullyOn}
          <Badge color="success" size="sm" dot>
            {$t`On`}
          </Badge>
        {/if}
      </div>
      <p class="text-text-tertiary text-sm">
        {#if blocked}
          {$t`Blocked for this site in your browser.`}
        {:else}
          {$t`Apps you've allowed can send push notifications to this device.`}
        {/if}
      </p>
    </div>

    {#if pushSupported && !blocked}
      <div class="flex h-6 shrink-0 items-center">
        <Toggle
          bind:checked={switchOn}
          onchange={handleToggle}
          disabled={busy || !deviceStatusLoaded}
          aria-labelledby={titleId}
        />
      </div>
    {/if}
  </div>

  {#if !pushSupported}
    <div class="border-border-tertiary mt-5 border-t pt-4">
      <p class="text-text-tertiary text-sm">
        {#if isIos}
          {$t`This browser doesn't support push notifications. On iPhone and iPad, add Internet Identity to your Home Screen and open it from there to enable them.`}
        {:else}
          {$t`This browser doesn't support push notifications.`}
        {/if}
      </p>
    </div>
  {:else if blocked}
    <div class="border-border-tertiary mt-5 flex flex-col gap-3 border-t pt-4">
      <p class="text-text-tertiary text-sm">
        {$t`Your browser is blocking notifications for this site. Allow them in your browser settings, then reload this page.`}
      </p>
      <NotifUnblockSteps {browser} open />
    </div>
  {:else}
    {#if needsAttention}
      <div
        class="border-border-tertiary text-text-tertiary bg-bg-primary mt-5 flex items-start gap-2.5 rounded-lg border border-t p-3 text-sm"
      >
        <TriangleAlertIcon class="mt-0.5 size-4 shrink-0" aria-hidden="true" />
        <span
          >{$t`Notifications may not be arriving on this device. Turn them off and on to reconnect.`}</span
        >
      </div>
    {/if}
    <div class="border-border-tertiary mt-5 border-t pt-4">
      <p
        class="text-text-tertiary mb-3 text-xs font-semibold tracking-wide uppercase"
      >
        {$t`Allowed apps`}
      </p>
      {#if (origins?.length ?? 0) > 0}
        <ul class="flex flex-col gap-2" aria-labelledby={titleId}>
          {#each origins ?? [] as origin (origin)}
            <li
              class={[
                "border-border-secondary flex flex-row items-center gap-3 rounded-lg border px-3 py-3 sm:px-4",
                !browserSubscribed && "opacity-60",
              ]}
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
        {#if !browserSubscribed}
          <p class="text-text-tertiary mt-3 text-sm">
            {$t`Turn this device on to receive from these apps.`}
          </p>
        {/if}
      {:else if browserSubscribed}
        <p class="text-text-tertiary text-sm">
          {$t`No apps yet. Apps you allow when you sign in will show up here.`}
        </p>
      {:else}
        <p class="text-text-tertiary text-sm">
          {$t`Turn on to let apps you've allowed notify this device.`}
        </p>
      {/if}
    </div>
  {/if}
</section>
