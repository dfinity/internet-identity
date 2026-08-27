<script lang="ts">
  import { onMount } from "svelte";
  import {
    BellIcon,
    BellOffIcon,
    Trash2Icon,
    TriangleAlertIcon,
  } from "@lucide/svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { Trans } from "$lib/components/locale";
  import { toaster } from "$lib/components/utils/toaster";
  import { t } from "$lib/stores/locale.store";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { isPushSupported } from "$lib/utils/notifications/pushSubscription";
  import { readDeviceState } from "$lib/utils/notifications/notificationState";
  import {
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

  type App = {
    origin: string;
    name: string;
    initial: string;
    grantedAtNs: bigint;
    lastSentNs?: bigint;
    muted: boolean;
  };

  const identityNumber = $authenticatedStore.identityNumber;
  const pushTitleId = $props.id();

  const pushSupported = typeof navigator !== "undefined" && isPushSupported();
  const browser = detectBrowser();
  const isIos = browser === "ios";

  let permission = $state<NotificationPermission>("default");
  let browserSubscribed = $state(false);
  let deviceStatusLoaded = $state(false);
  let busy = $state(false);
  let lastFailureReason = $state<string>();

  // `undefined` while the first read is in flight.
  let apps = $state<App[]>();
  let pendingRemove = $state<App>();
  let removing = $state(false);

  const blocked = $derived(pushSupported && permission === "denied");
  const needsAttention = $derived(
    browserSubscribed &&
      (lastFailureReason === "subscribe-failed" ||
        lastFailureReason === "register-failed"),
  );

  // The device switch is driven separately so a failed toggle can snap back.
  let switchOn = $state(false);
  const syncSwitch = () => {
    switchOn = browserSubscribed;
  };

  const dapps = getDapps();
  const appName = (origin: string): string =>
    dapps.find((dapp) => dapp.hasOrigin(origin))?.name ?? origin;
  const initialOf = (name: string): string =>
    (name.trim()[0] ?? "?").toUpperCase();

  const dateFmt = new Intl.DateTimeFormat(undefined, {
    day: "numeric",
    month: "short",
  });
  const grantedLabel = (ns: bigint): string =>
    $t`Allowed ${dateFmt.format(new Date(Number(ns / BigInt(1_000_000))))}`;

  const sentLabel = (ns?: bigint): string => {
    if (ns === undefined) {
      return $t`not sent yet`;
    }
    const seconds = Math.max(
      0,
      Math.round((Date.now() - Number(ns / BigInt(1_000_000))) / 1000),
    );
    if (seconds < 90) {
      return $t`last sent just now`;
    }
    const minutes = Math.round(seconds / 60);
    if (minutes < 90) {
      return $t`last sent ${minutes}m ago`;
    }
    const hours = Math.round(minutes / 60);
    if (hours < 36) {
      return $t`last sent ${hours}h ago`;
    }
    const days = Math.round(hours / 24);
    return $t`last sent ${days}d ago`;
  };

  const appCountLabel = $derived.by(() => {
    const list = apps ?? [];
    if (list.length === 0) {
      return "";
    }
    const allowed = list.filter((a) => !a.muted).length;
    return $t`${allowed} of ${list.length} allowed to send`;
  });

  const toApp = (row: {
    origin: string;
    granted_at_ns: bigint;
    last_sent_ns: [] | [bigint];
    muted: boolean;
  }): App => ({
    origin: row.origin,
    name: appName(row.origin),
    initial: initialOf(appName(row.origin)),
    grantedAtNs: row.granted_at_ns,
    lastSentNs: row.last_sent_ns[0],
    muted: row.muted,
  });

  const loadApps = async () => {
    try {
      const rows =
        await $authenticatedStore.actor.notification_consented_apps(
          identityNumber,
        );
      apps = rows
        .map(toApp)
        .sort((a, b) => Number(b.grantedAtNs - a.grantedAtNs));
    } catch {
      apps = [];
    }
  };

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
        browserSubscribed = (await readDeviceState()).subscribed;
      } catch {
        browserSubscribed = false;
      } finally {
        deviceStatusLoaded = true;
        syncSwitch();
      }
    })();
    void loadApps();
  });

  const handleDeviceToggle = async (event: Event) => {
    if (!(event.currentTarget instanceof HTMLInputElement) || busy) {
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
    } catch {
      recordFailure("subscribe-failed");
      toaster.error({
        title: $t`Couldn't change notifications on this device. Please try again.`,
        duration: 8000,
      });
    } finally {
      syncSwitch();
      busy = false;
    }
  };

  const toggleMuted = async (app: App) => {
    const muted = !app.muted;
    apps = (apps ?? []).map((a) =>
      a.origin === app.origin ? { ...a, muted } : a,
    );
    try {
      await $authenticatedStore.actor
        .notification_set_app_muted(identityNumber, app.origin, muted)
        .then((result) => {
          if ("Err" in result) {
            throw new Error(result.Err);
          }
        });
    } catch {
      apps = (apps ?? []).map((a) =>
        a.origin === app.origin ? { ...a, muted: !muted } : a,
      );
      toaster.error({
        title: $t`Couldn't change this app. Please try again.`,
        duration: 4000,
      });
    }
  };

  const confirmRemove = async () => {
    const app = pendingRemove;
    if (app === undefined) {
      return;
    }
    removing = true;
    const previous = apps ?? [];
    apps = previous.filter((a) => a.origin !== app.origin);
    try {
      await revokeApp(identityNumber, $authenticatedStore.actor, app.origin);
      pendingRemove = undefined;
    } catch {
      apps = previous;
      toaster.error({
        title: $t`Couldn't remove this app. Please try again.`,
        duration: 4000,
      });
    } finally {
      removing = false;
    }
  };
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">
    {$t`Notifications`}
  </h1>
  <p class="text-text-tertiary text-base">
    <Trans
      >Choose what reaches this device, and which apps are allowed to send.</Trans
    >
  </p>
</header>

<div class="mt-10 flex max-w-3xl flex-col gap-5">
  <!-- Device push toggle -->
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
        <h2 id={pushTitleId} class="text-text-primary text-base font-semibold">
          {$t`Push notifications`}
        </h2>
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
            onchange={handleDeviceToggle}
            disabled={busy || !deviceStatusLoaded}
            aria-labelledby={pushTitleId}
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
      <div
        class="border-border-tertiary mt-5 flex flex-col gap-3 border-t pt-4"
      >
        <p class="text-text-tertiary text-sm">
          {$t`Your browser is blocking notifications for this site. Allow them in your browser settings, then reload this page.`}
        </p>
        <NotifUnblockSteps {browser} open />
      </div>
    {:else if needsAttention}
      <div
        class="border-border-tertiary text-text-tertiary bg-bg-primary mt-5 flex items-start gap-2.5 rounded-lg border p-3 text-sm"
      >
        <TriangleAlertIcon class="mt-0.5 size-4 shrink-0" aria-hidden="true" />
        <span
          >{$t`Notifications may not be arriving on this device. Turn them off and on to reconnect.`}</span
        >
      </div>
    {/if}
  </section>

  <!-- Allowed apps -->
  {#if !blocked && pushSupported}
    <section
      class="border-border-secondary bg-bg-secondary flex flex-col rounded-xl border"
    >
      <div
        class="flex flex-row flex-wrap items-baseline gap-x-3 gap-y-1 p-4 pb-3 sm:p-5 sm:pb-3"
      >
        <h2 class="text-text-primary text-base font-semibold">
          {$t`Allowed apps`}
        </h2>
        {#if appCountLabel !== ""}
          <span class="text-text-tertiary text-sm">{appCountLabel}</span>
        {/if}
      </div>

      <div
        class={[
          "flex flex-col",
          !browserSubscribed && "pointer-events-none opacity-60",
        ]}
      >
        {#if (apps?.length ?? 0) > 0}
          {#each apps ?? [] as app (app.origin)}
            <div
              class="border-border-tertiary flex flex-row items-center gap-3 border-t px-4 py-3 sm:gap-4 sm:px-5"
            >
              <span
                class="border-border-tertiary bg-bg-primary text-fg-tertiary flex size-9 shrink-0 items-center justify-center rounded-md border text-sm font-semibold"
                aria-hidden="true"
              >
                {app.initial}
              </span>
              <div class="flex min-w-0 flex-1 flex-col">
                <span class="text-text-primary truncate text-sm font-semibold">
                  {app.name}
                </span>
                <span
                  class="text-text-tertiary truncate text-xs"
                  title={app.origin}
                >
                  {grantedLabel(app.grantedAtNs)} · {sentLabel(app.lastSentNs)}
                </span>
              </div>
              <Toggle
                checked={!app.muted}
                onchange={() => toggleMuted(app)}
                disabled={!browserSubscribed}
                aria-label={$t`Allow ${app.name} to send`}
              />
              <button
                class="btn btn-tertiary btn-sm btn-icon shrink-0"
                onclick={() => (pendingRemove = app)}
                aria-label={$t`Remove ${app.name}`}
              >
                <Trash2Icon class="size-4.5" />
              </button>
            </div>
          {/each}
        {:else if apps !== undefined}
          <p
            class="text-text-tertiary border-border-tertiary border-t px-4 py-4 text-sm sm:px-5"
          >
            {$t`No apps yet. Apps you allow when you sign in will show up here.`}
          </p>
        {/if}
      </div>

      <p
        class="border-border-tertiary text-text-tertiary border-t px-4 py-4 text-xs leading-relaxed text-pretty sm:px-5"
      >
        <Trans
          >Internet Identity relays notifications without reading them. Each app
          only knows the pseudonym it was given, so turning one off never
          affects the others.</Trans
        >
      </p>
    </section>
  {/if}
</div>

{#if pendingRemove !== undefined}
  {@const app = pendingRemove}
  <Dialog onClose={() => (pendingRemove = undefined)}>
    <div class="flex flex-col gap-2 p-1">
      <h2 class="text-text-primary text-lg font-medium">
        {$t`Remove ${app.name}?`}
      </h2>
      <p class="text-text-tertiary text-sm leading-relaxed text-pretty">
        <Trans
          >It will stop sending notifications to this device. You can allow it
          again next time you sign in to it.</Trans
        >
      </p>
      <div class="mt-4 flex flex-row gap-3">
        <button
          class="btn btn-secondary flex-1"
          onclick={() => (pendingRemove = undefined)}
          disabled={removing}
        >
          {$t`Cancel`}
        </button>
        <button
          class="btn btn-secondary btn-danger flex-1"
          onclick={confirmRemove}
          disabled={removing}
        >
          {$t`Remove`}
        </button>
      </div>
    </div>
  </Dialog>
{/if}
