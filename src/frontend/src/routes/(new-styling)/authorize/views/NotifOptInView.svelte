<script lang="ts">
  import { ClockIcon, ZapIcon } from "@lucide/svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { bufFromBufLike, throwTextCanisterError } from "$lib/utils/utils";
  import { getVapidPublicKey, subscribeDevice } from "$lib/utils/pushConsent";

  interface Props {
    effectiveOrigin: string;
    displayOrigin: string;
    onContinue: () => void;
  }

  const { effectiveOrigin, displayOrigin, onContinue }: Props = $props();

  const dapps = getDapps();
  const application = $derived(
    dapps.find((dapp) => dapp.hasOrigin(displayOrigin))?.name,
  );
  const dappName = $derived(application ?? new URL(displayOrigin).hostname);

  let enabling = $state(false);

  const pushSupported =
    typeof navigator !== "undefined" &&
    "serviceWorker" in navigator &&
    "PushManager" in window;

  const subscribeThisDevice = async (): Promise<void> => {
    const { actor, identityNumber } = $authenticatedStore;
    const registration =
      await navigator.serviceWorker.register("/service-worker.js");
    await navigator.serviceWorker.ready;
    const vapidPublicKey = await getVapidPublicKey(actor);
    const subscription = await registration.pushManager.subscribe({
      userVisibleOnly: true,
      applicationServerKey: bufFromBufLike(vapidPublicKey),
    });
    const { endpoint, keys } = subscription.toJSON() as {
      endpoint: string;
      keys: { p256dh: string; auth: string };
    };
    await subscribeDevice(
      actor,
      identityNumber,
      endpoint,
      keys.p256dh,
      keys.auth,
    );
    await actor
      .push_grant_consent(identityNumber, effectiveOrigin)
      .then(throwTextCanisterError);
  };

  const handleEnable = async () => {
    if (!pushSupported) {
      onContinue();
      return;
    }
    enabling = true;
    try {
      const permission = await Notification.requestPermission();
      if (permission === "granted") {
        await subscribeThisDevice();
      }
    } catch (error) {
      console.warn("Enable notifications failed:", error);
    } finally {
      onContinue();
    }
  };
</script>

<div class="flex min-w-0 flex-1 flex-col">
  <div
    class="border-border-tertiary from-bg-secondary to-bg-primary relative mt-2 overflow-hidden rounded-2xl border bg-gradient-to-b p-4"
  >
    <div
      class="pointer-events-none absolute inset-0 opacity-15"
      style="background-image: radial-gradient(circle at center, var(--fg-quaternary) 1px, transparent 1.4px); background-size: 20px 20px;"
    ></div>
    <div class="relative">
      <div
        class="border-border-tertiary bg-bg-secondary relative flex origin-top scale-95 items-start gap-2.5 rounded-xl border px-3 py-2.5 opacity-75"
      >
        <span
          class="border-border-tertiary bg-bg-tertiary text-text-secondary flex size-8 shrink-0 items-center justify-center rounded-lg border"
          aria-hidden="true"
        >
          <img src="/favicon.svg" alt="" class="size-4.5" />
        </span>
        <div class="min-w-0 flex-1">
          <div class="flex items-baseline justify-between gap-2">
            <span class="text-text-primary text-sm font-semibold">
              {$t`Caffeine`}
            </span>
            <span class="text-text-tertiary text-xs">{$t`2m`}</span>
          </div>
          <p class="text-text-secondary mt-0.5 truncate text-sm">
            {$t`Your app is built and live at recipe-box.caffeine.ai`}
          </p>
        </div>
      </div>
      <div
        class="border-border-tertiary bg-bg-tertiary relative z-10 -mt-3.5 flex items-start gap-2.5 rounded-xl border px-3 py-2.5 shadow-2xl backdrop-blur-sm"
      >
        <span
          class="border-border-tertiary bg-bg-primary text-text-secondary flex size-8 shrink-0 items-center justify-center rounded-lg border"
          aria-hidden="true"
        >
          <img src="/favicon.svg" alt="" class="size-4.5" />
        </span>
        <div class="min-w-0 flex-1">
          <div class="flex items-baseline justify-between gap-2">
            <span class="text-text-primary text-sm font-semibold">
              {$t`OISY Wallet`}
            </span>
            <span class="text-text-tertiary text-xs">{$t`now`}</span>
          </div>
          <p class="text-text-secondary mt-0.5 truncate text-sm">
            {$t`Your transfer of 25 ICP is confirmed.`}
          </p>
        </div>
      </div>
    </div>
  </div>

  <h1
    class="text-text-primary mt-6 max-w-full min-w-0 self-start text-2xl font-medium tracking-tight break-words"
  >
    {$t`Let ${dappName} notify you`}
  </h1>
  <p class="text-text-secondary mt-2 text-sm leading-relaxed">
    <Trans>
      Receive alerts from the apps you approve, and stay on top of what matters
      seamlessly.
    </Trans>
  </p>

  <ul class="mt-5 flex flex-col gap-4">
    <li class="flex items-start gap-3.5">
      <span
        class="border-border-secondary bg-bg-secondary text-text-primary flex size-9 shrink-0 items-center justify-center rounded-full border"
        aria-hidden="true"
      >
        <ZapIcon class="size-4" />
      </span>
      <div class="min-w-0 flex-1">
        <div class="text-text-primary text-sm font-semibold">
          {$t`Instant activity alerts`}
        </div>
        <p class="text-text-tertiary mt-0.5 text-sm leading-snug">
          <Trans>Transfers, replies and mentions the moment they happen.</Trans>
        </p>
      </div>
    </li>
    <li class="flex items-start gap-3.5">
      <span
        class="border-border-secondary bg-bg-secondary text-text-primary flex size-9 shrink-0 items-center justify-center rounded-full border"
        aria-hidden="true"
      >
        <ClockIcon class="size-4" />
      </span>
      <div class="min-w-0 flex-1">
        <div class="text-text-primary text-sm font-semibold">
          {$t`Reachable anytime`}
        </div>
        <p class="text-text-tertiary mt-0.5 text-sm leading-snug">
          <Trans>Updates reach your device without keeping the app open.</Trans>
        </p>
      </div>
    </li>
  </ul>

  <div class="mt-6 flex flex-col gap-2">
    <button
      class="btn btn-primary btn-xl w-full"
      onclick={handleEnable}
      disabled={enabling}
    >
      {#if enabling}
        <ProgressRing />
        <span>{$t`Enabling...`}</span>
      {:else}
        <span>{$t`Enable notifications`}</span>
      {/if}
    </button>
    <button
      class="btn btn-tertiary btn-lg w-full"
      onclick={onContinue}
      disabled={enabling}
    >
      {$t`Maybe later`}
    </button>
  </div>
</div>
