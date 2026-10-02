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
  import { browserKeyActor } from "$lib/utils/notifications/browserActor";
  import { ensureRegisteredDevice } from "$lib/utils/notifications/subscribeDevice";
  import { forgetAlreadyInstalled } from "$lib/utils/notifications/alreadyInstalled";
  import { notificationsNeedInstallHere } from "$lib/utils/notifications/notificationState";
  import { requestNotificationPermission } from "$lib/utils/notifications/pushSubscription";

  const {
    identityNumber,
    browsers,
    browserId,
    saidAlreadyInstalled,
  }: {
    identityNumber: bigint;
    /** From `identity_info`, so this renders its real state on first paint. */
    browsers: BrowserInfo[];
    /** Which of them this browser is, or `undefined` before a sign-in registered one. */
    browserId: number | undefined;
    /** What the user told this browser about their own device, which nothing on chain
     *  can know. Only consulted where notifications arrive through an installed app. */
    saidAlreadyInstalled: boolean;
  } = $props();

  const titleId = $props.id();

  /** Where notifications arrive is a Home Screen app on iOS and this browser anywhere
   *  else, which changes what the switch reports and what turning it on does. */
  const needsApp = notificationsNeedInstallHere();

  /** The app this browser installed, where it has one. */
  const ownApp = $derived(
    browsers.find((browser) => browser.linked_from_browser[0] === browserId),
  );

  /**
   * What the canister holds, which is what the switch shows until a press moves it.
   * Assigned by the handlers so the switch answers the click, and re-derived from
   * `identity_info` once `invalidateAll` has refreshed it.
   */
  const live = $derived(
    needsApp
      ? (ownApp?.notifications_on ?? false) || saidAlreadyInstalled
      : (browsers.find((browser) => browser.id === browserId)
          ?.notifications_on ?? false),
  );
  let enabled = $derived(live);
  let busy = $state(false);

  /**
   * Takes away the registration that brings notifications here. The permission is left
   * alone: that is the browser's to give and the user's to take back in its settings.
   *
   * On iOS the registration belongs to the app this browser installed, not to this
   * browser. Their word that the device already had one goes too, so turning the switch
   * back on offers the install again, which is the way out of having said it by mistake.
   */
  const turnOff = async () => {
    const target = needsApp ? ownApp?.id : browserId;
    if (target !== undefined) {
      const actor = await browserKeyActor(identityNumber);
      await actor
        .remove_webpush_subscription({
          anchor_number: identityNumber,
          browser_id: target,
        })
        .then(throwCanisterError);
    }
    if (needsApp) {
      await forgetAlreadyInstalled(identityNumber);
    }
  };

  /**
   * On iOS notifications arrive in a Home Screen app, and this page cannot install
   * one: it opens the page that explains how, in this click's own gesture, which is
   * the only time a browser allows it. Anywhere else this browser receives them, so it
   * asks for the permission and registers.
   */
  const turnOn = async () => {
    if (needsApp) {
      window.open("/notifications", "_blank");
      return;
    }
    const permission = await requestNotificationPermission();
    if (permission !== "granted") {
      toaster.error({
        title: $t`Notifications are blocked`,
        description: $t`Allow them for this site in your browser settings, then turn this on again.`,
      });
      return;
    }
    await ensureRegisteredDevice(identityNumber);
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
        await (wanted ? turnOn() : turnOff());
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
      {#if needsApp}
        <Trans>
          Get notified on this device when your apps need you. iPhone and iPad
          receive them through an app on your Home Screen.
        </Trans>
      {:else}
        <Trans>Get notified in this browser when your apps need you.</Trans>
      {/if}
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
