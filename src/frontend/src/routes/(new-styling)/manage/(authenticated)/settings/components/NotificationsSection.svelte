<script lang="ts">
  import BellIcon from "@lucide/svelte/icons/bell";
  import { invalidateAll } from "$app/navigation";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { isCanisterError } from "$lib/utils/utils";
  import type { BrowserInfo } from "$lib/generated/internet_identity_types";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { setAppEnabled } from "$lib/utils/notifications/appEnabled";
  import {
    forgetAlreadyInstalled,
    saidAlreadyInstalled,
  } from "$lib/utils/notifications/alreadyInstalled";
  import {
    notificationsNeedInstallHere,
    readBrowserPushState,
    readDeviceState,
  } from "$lib/utils/notifications/notificationState";
  import { failureMessage } from "$lib/utils/notifications/notificationDiagnostics";
  import { openPendingInstallTab } from "$lib/utils/notifications/installUrl";
  import { turnOnWithApp } from "./notificationsSwitch";
  import {
    turnOffInBrowser,
    turnOnInBrowser,
  } from "./browserNotificationsSwitch";

  const {
    identityNumber,
    browsers,
    browserId,
  }: {
    identityNumber: bigint;
    /** From `identity_info`, so this renders its real state on first paint. */
    browsers: BrowserInfo[];
    /** Which of them this browser is; settings cannot mint one, so the section stays off until a sign-in has registered one. */
    browserId: number;
  } = $props();

  const titleId = $props.id();

  /** Where notifications arrive is a Home Screen app on iOS and this browser anywhere else. */
  const needsApp = notificationsNeedInstallHere();

  /** Where the install lives, shown where the browser refused the tab. */
  let refusedInstallUrl = $state<string | undefined>(undefined);

  /** What this browser's own entry says about the app it points at. */
  const ownApp = $derived(
    browsers.find((browser) => browser.id === browserId)?.notification_app[0],
  );

  /** What the user told this browser about their own device; read here so the switch answers on first paint. */
  let saidInstalled = $state(false);
  $effect(() => {
    if (!needsApp) {
      return;
    }
    void saidAlreadyInstalled(identityNumber).then(
      (said) => {
        saidInstalled = said;
      },
      // Nothing here can act on it, so the reason is logged rather than thrown into a render.
      (error: unknown) => console.error(error),
    );
  });

  /** Counts presses rather than tracking them: a read that started before one must not
   *  write what it found over the answer the press came back with. Not `$state`, so
   *  reading it here does not make every press re-run the read. */
  let presses = 0;

  /** What this browser holds for itself, which is what the switch reports where notifications arrive in the browser. */
  let browserDelivers = $state(false);
  $effect(() => {
    if (needsApp) {
      return;
    }
    const startedAt = presses;
    void (async () => {
      const pushState = await readBrowserPushState();
      const state =
        pushState === undefined
          ? undefined
          : await readDeviceState(identityNumber, pushState);
      if (startedAt !== presses) {
        // The user has acted since this started, and what they did is the newer answer.
        return;
      }
      browserDelivers = state?.permission === "granted" && state.registered;
    })().catch((error: unknown) => {
      // The switch answers off, which is what it answered before this ran.
      console.error(error);
    });
  });

  /** The install happens in another tab, and what finishes it lands in this browser's
   *  own storage and on the canister. Neither reaches a page that is already open, so
   *  coming back to this one is when both are read again. */
  $effect(() => {
    const refresh = () => {
      if (document.visibilityState !== "visible") {
        return;
      }
      void invalidateAll();
      void saidAlreadyInstalled(identityNumber).then(
        (said) => {
          saidInstalled = said;
        },
        (error: unknown) => console.error(error),
      );
    };
    document.addEventListener("visibilitychange", refresh);
    window.addEventListener("focus", refresh);
    return () => {
      document.removeEventListener("visibilitychange", refresh);
      window.removeEventListener("focus", refresh);
    };
  });

  /** The app's subscription is shared with every other identity pointing at it, so this identity's half of the answer is its own flag. */
  const live = $derived(
    needsApp
      ? (ownApp?.subscribed === true && ownApp.enabled) || saidInstalled
      : browserDelivers,
  );
  let enabled = $derived(live);

  /** On because the user said this device already has the app, which the canister knows nothing about. */
  const onTheirWord = $derived(
    needsApp && saidInstalled && ownApp?.subscribed !== true,
  );
  let busy = $state(false);

  /** On this device's app that is this identity's flag and never the app's subscription, which every other identity delivers through; the permission is left alone either way. */
  const turnOff = async () => {
    if (needsApp) {
      if (ownApp !== undefined) {
        await setAppEnabled({ identityNumber, enabled: false });
      }
      // Their word that the device already had one goes too, which is the way out of having said it by mistake.
      await forgetAlreadyInstalled(identityNumber);
      saidInstalled = false;
      return;
    }
    await turnOffInBrowser({
      identityNumber,
      browserId,
      actor: $authenticatedStore.actor,
    });
  };

  /** Opens the install page in this click's own gesture, which is the only time a browser allows it.
   *  Answers whether notifications are on now; an install that has only been opened is not. */
  const turnOn = async (): Promise<boolean> => {
    if (!needsApp) {
      switch (await turnOnInBrowser(identityNumber)) {
        case "delivering":
          return true;
        case "permission-refused":
          toaster.error({
            title: $t`Notifications are blocked`,
            description: $t`Allow them for this site in your browser settings, then turn this on again.`,
          });
          return false;
      }
    }

    // An app this browser already points at and that is delivering needs nothing but this identity's say-so.
    if (ownApp?.subscribed === true) {
      await setAppEnabled({ identityNumber, enabled: true });
      return true;
    }
    // The tab is opened before anything is awaited, because a browser allows `window.open` only inside the press itself.
    const tab = openPendingInstallTab();
    const outcome = await turnOnWithApp({ identityNumber, ownApp });
    if (outcome.delivering) {
      tab.point(undefined);
      return true;
    }
    tab.point(outcome.installUrl);
    // A tab the browser refused is not a failure: the link goes on the section.
    refusedInstallUrl = tab.opened ? undefined : outcome.installUrl;
    return false;
  };

  const handleToggle = (event: Event) => {
    if (!(event.currentTarget instanceof HTMLInputElement) || busy) {
      return;
    }
    const wanted = event.currentTarget.checked;
    presses += 1;
    enabled = wanted;
    refusedInstallUrl = undefined;
    busy = true;
    void (async () => {
      try {
        if (!wanted) {
          await turnOff();
        }
        const outcome = wanted ? await turnOn() : false;
        if (!needsApp) {
          // The read that answers on first paint runs once, so leaving it to that would roll a later failure back.
          browserDelivers = outcome;
        }
        enabled = outcome;
      } catch (error) {
        enabled = live;
        if (isCanisterError(error)) {
          handleError(error);
          return;
        }
        toaster.error({
          title: $t`Notifications unavailable`,
          description: failureMessage(error),
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
          Get notified on this device when your apps need you. They arrive in
          the app on your Home Screen.
        </Trans>
      {:else}
        <Trans>Get notified in this browser when your apps need you.</Trans>
      {/if}
    </p>
    {#if onTheirWord}
      <p class="text-text-tertiary text-sm">
        <Trans>
          On because you said this device already has the app. Turn this off and
          on again if it does not.
        </Trans>
      </p>
    {/if}
    {#if refusedInstallUrl !== undefined}
      <a
        class="btn btn-secondary btn-sm mt-2 self-start"
        href={refusedInstallUrl}
        target="_blank"
        rel="noreferrer"
      >
        {$t`Set up notifications`}
      </a>
    {/if}
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
