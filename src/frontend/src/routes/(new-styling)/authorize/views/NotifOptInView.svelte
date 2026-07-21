<script lang="ts">
  import { ClockIcon, ZapIcon } from "@lucide/svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
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
    class="mt-2"
    style="position:relative;border-radius:20px;padding:20px 20px 34px;background:var(--bg-primary_alt);border:1px solid var(--border-tertiary);overflow:hidden"
  >
    <div
      style="position:absolute;inset:0;background-image:radial-gradient(circle at center, var(--fg-quaternary) 1px, transparent 1.4px);background-size:22px 22px;opacity:.14"
    ></div>
    <div
      style="position:absolute;left:50%;top:62%;transform:translate(-50%,-50%);width:320px;height:220px;background:radial-gradient(ellipse at center, rgba(255,255,255,.09), transparent 70%);pointer-events:none"
    ></div>
    <div
      style="position:absolute;left:0;right:0;bottom:0;height:60px;background:linear-gradient(to top, var(--bg-primary_alt), transparent);pointer-events:none;z-index:3"
    ></div>

    <span
      style="position:relative;z-index:4;display:inline-flex;align-self:flex-start;padding:4px 11px;border-radius:9999px;background:var(--bg-secondary);border:1px solid var(--border-tertiary);font-size:12px;font-weight:600;color:var(--text-tertiary);margin-bottom:16px"
    >
      {$t`Example`}
    </span>

    <div
      style="position:relative;height:20px;margin:0 26px;border-radius:14px 14px 8px 8px;background:rgba(255,255,255,.02);border:1px solid var(--border-tertiary);border-bottom:0;z-index:0"
      aria-hidden="true"
    ></div>

    <div
      style="position:relative;display:flex;gap:11px;padding:12px;border-radius:15px;background:rgba(255,255,255,.03);border:1px solid var(--border-tertiary);align-items:flex-start;margin:-6px 12px 0;z-index:1;opacity:.9"
    >
      <span
        style="width:34px;height:34px;border-radius:9px;background:var(--bg-tertiary);border:1px solid var(--border-tertiary);display:flex;align-items:center;justify-content:center;flex-shrink:0"
        aria-hidden="true"
      >
        <Logo class="text-text-primary" style="width:22px" />
      </span>
      <div style="flex:1;min-width:0">
        <div
          style="display:flex;justify-content:space-between;align-items:baseline;gap:8px"
        >
          <span
            style="font-size:13px;font-weight:600;color:var(--text-primary)"
          >
            {$t`A marketplace`}
          </span>
          <span style="font-size:11px;color:var(--text-tertiary)">
            {$t`2m`}
          </span>
        </div>
        <div
          style="font-size:13px;color:var(--text-secondary);line-height:1.4;margin-top:1px"
        >
          {$t`Your item just sold.`}
        </div>
      </div>
    </div>

    <div
      style="position:relative;display:flex;gap:11px;padding:13px 12px;border-radius:15px;background:rgba(255,255,255,.07);backdrop-filter:blur(8px);border:1px solid var(--border-secondary);align-items:flex-start;box-shadow:0 12px 30px -10px rgba(0,0,0,.6);margin:-34px 0 0;z-index:2"
    >
      <span
        style="width:34px;height:34px;border-radius:9px;background:var(--bg-tertiary);border:1px solid var(--border-tertiary);display:flex;align-items:center;justify-content:center;flex-shrink:0"
        aria-hidden="true"
      >
        <Logo class="text-text-primary" style="width:22px" />
      </span>
      <div style="flex:1;min-width:0">
        <div
          style="display:flex;justify-content:space-between;align-items:baseline;gap:8px"
        >
          <span
            style="font-size:13px;font-weight:600;color:var(--text-primary)"
          >
            {$t`Your wallet`}
          </span>
          <span style="font-size:11px;color:var(--text-tertiary)">
            {$t`now`}
          </span>
        </div>
        <div
          style="font-size:13px;color:var(--text-secondary);line-height:1.4;margin-top:1px"
        >
          {$t`Your transfer is confirmed.`}
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
        <span>{$t`Turning on...`}</span>
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
