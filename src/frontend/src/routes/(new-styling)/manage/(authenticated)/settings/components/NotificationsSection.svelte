<script lang="ts">
  import BellIcon from "@lucide/svelte/icons/bell";
  import { invalidateAll } from "$app/navigation";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
  import type { BrowserInfo } from "$lib/generated/internet_identity_types";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { ensureRegisteredDevice } from "$lib/utils/notifications/subscribeDevice";
  import { claimDeviceApp } from "$lib/utils/notifications/deviceApp";
  import {
    notificationsNeedInstallHere,
    readBrowserPushState,
    readDeviceState,
  } from "$lib/utils/notifications/notificationState";
  import { requestNotificationPermission } from "$lib/utils/notifications/pushSubscription";
  import {
    openInstallTab,
    prepareInstallUrl,
  } from "$lib/utils/notifications/installUrl";

  const {
    identityNumber,
    browsers,
    browserId,
  }: {
    identityNumber: bigint;
    /** From `identity_info`, so this renders its real state on first paint. */
    browsers: BrowserInfo[];
    /** Which of them this browser is, or `undefined` before a sign-in registered one. */
    browserId: number | undefined;
  } = $props();

  const titleId = $props.id();

  /** Where notifications arrive is a Home Screen app on iOS and this browser anywhere
   *  else, which changes what the switch reports and what turning it on does. */
  const needsApp = notificationsNeedInstallHere();

  /**
   * Signed while this section renders, because `window.open` needs the click's own
   * gesture and an await between the two loses it.
   *
   * Prepared again whenever the switch is pressed as well, since a token is good for
   * half an hour and a settings page can sit open for longer.
   */
  let installUrl = $state<string | undefined>(undefined);
  $effect(() => {
    if (!needsApp) {
      return;
    }
    void prepareInstallUrl(identityNumber).then((url) => {
      installUrl = url;
    });
  });

  /** What this browser's own entry says about the app it points at. */
  const ownApp = $derived(
    browserId === undefined
      ? undefined
      : browsers.find((browser) => browser.id === browserId)
          ?.notification_app[0],
  );

  /** What this browser holds for itself, which is what the switch reports where
   *  notifications arrive in the browser rather than in an app. */
  let browserDelivers = $state(false);
  $effect(() => {
    if (needsApp) {
      return;
    }
    void (async () => {
      const pushState = await readBrowserPushState();
      const state =
        pushState === undefined
          ? undefined
          : await readDeviceState(identityNumber, pushState);
      browserDelivers = state?.permission === "granted" && state.registered;
    })();
  });

  /**
   * What the canister says, which is what the switch shows until a press moves it.
   *
   * On this device's app, delivering means the app has registered and this identity has
   * it switched on. The app's subscription is shared with every other identity pointing
   * at it, so this identity's half of the answer is its own flag and nothing else.
   */
  const live = $derived(
    needsApp ? ownApp?.subscribed === true && ownApp.enabled : browserDelivers,
  );
  let enabled = $derived(live);
  let busy = $state(false);

  /**
   * Takes away what brings notifications here.
   *
   * On this device's app that is this identity's flag on the app, never the app's
   * subscription: the subscription is the app's own and every other identity pointing at
   * it delivers through the same one. Anywhere else it is this browser's registration.
   * The permission is left alone either way: that is the browser's to give and the
   * user's to take back in its settings.
   */
  const turnOff = async () => {
    if (needsApp) {
      await $authenticatedStore.actor
        .set_notification_app_enabled({
          anchor_number: identityNumber,
          enabled: false,
        })
        .then(throwCanisterError);
      return;
    }
    if (browserId !== undefined) {
      await $authenticatedStore.actor
        .remove_webpush_subscription({
          anchor_number: identityNumber,
          browser_id: browserId,
        })
        .then(throwCanisterError);
    }
  };

  /**
   * On this device notifications arrive in a Home Screen app, and this page cannot
   * install one: it opens the page that explains how, in this click's own gesture, which
   * is the only time a browser allows it. An app another identity here already has is
   * pointed at instead, which needs no install at all. Anywhere else this browser
   * receives them, so it asks for the permission and registers.
   *
   * Answers whether notifications are on now. An install that has only been opened is
   * not: what the switch shows is what the canister says, and it says nothing yet.
   */
  const turnOn = async (): Promise<boolean> => {
    if (!needsApp) {
      const permission = await requestNotificationPermission();
      if (permission !== "granted") {
        toaster.error({
          title: $t`Notifications are blocked`,
          description: $t`Allow them for this site in your browser settings, then turn this on again.`,
        });
        return false;
      }
      await ensureRegisteredDevice(identityNumber);
      return true;
    }

    if (ownApp !== undefined) {
      await $authenticatedStore.actor
        .set_notification_app_enabled({
          anchor_number: identityNumber,
          enabled: true,
        })
        .then(throwCanisterError);
      return ownApp.subscribed;
    }
    if (await claimDeviceApp(identityNumber)) {
      return true;
    }
    if (installUrl === undefined) {
      toaster.error({
        title: $t`Couldn't start the install`,
        description: $t`This browser hasn't registered its key yet. Reload the page and try again.`,
      });
      return false;
    }
    openInstallTab(installUrl);
    return false;
  };

  const handleToggle = (event: Event) => {
    if (!(event.currentTarget instanceof HTMLInputElement) || busy) {
      return;
    }
    const wanted = event.currentTarget.checked;
    enabled = wanted;
    busy = true;
    void (async () => {
      try {
        if (!wanted) {
          await turnOff();
        }
        enabled = wanted ? await turnOn() : false;
      } catch (error) {
        enabled = live;
        if (isCanisterError(error)) {
          handleError(error);
          return;
        }
        toaster.error({
          title: $t`Notifications unavailable`,
          description: error instanceof Error ? error.message : String(error),
        });
      } finally {
        busy = false;
        void invalidateAll();
      }
    })();
  };
</script>

<section
  class="border-border-secondary bg-bg-secondary flex flex-row items-start gap-3 rounded-xl border p-4 sm:gap-4 sm:p-5"
>
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
        {$t`Allow notifications`}
      </h3>
      {#if enabled}
        <Badge color="success" size="sm" dot>
          {$t`On for this device`}
        </Badge>
      {/if}
    </div>
    <p class="text-text-tertiary text-sm">
      <Trans>Get notified in this browser when your apps need you.</Trans>
    </p>
  </div>

  <div class="shrink-0">
    <Toggle
      checked={enabled}
      onchange={handleToggle}
      disabled={busy}
      aria-labelledby={titleId}
    />
  </div>
</section>
