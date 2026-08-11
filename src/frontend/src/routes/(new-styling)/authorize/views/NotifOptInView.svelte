<script lang="ts">
  import { ClockIcon, ZapIcon } from "@lucide/svelte";
  import Logo from "$lib/components/ui/Logo.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { throwTextCanisterError } from "$lib/utils/utils";
  import { onMount } from "svelte";
  import {
    ensureDeviceSubscription,
    listConsentedOrigins,
  } from "$lib/utils/pushConsent";
  import { mintPushDelegation } from "$lib/utils/authentication/pushDelegation";
  import { storePushDelegation } from "$lib/stores/push-delegation.store";
  import {
    notificationsGloballyGranted,
    recordNotifOptInDecision,
    pushCapable,
    applePushDeferred,
  } from "../notifOptIn";

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
  let enableError = $state<string | undefined>(undefined);

  // iOS in a tab: consent can be granted here, but the subscription has to be
  // created later from II's installed app. When that is the situation, Enable
  // grants consent and shows how to finish rather than trying to subscribe.
  const deferred = applePushDeferred();
  // After a deferred grant, the screen becomes the "finish on your device" step
  // instead of continuing straight through.
  let finishOnDevice = $state(false);

  // Whether this identity already allowed this app on some OTHER device.
  //
  // Consent lives with the identity and is shared by every device, but a push
  // subscription belongs to one browser — so "allowed" and "reachable here" are
  // different questions, and the ask is a different ask. Asking someone to
  // "allow this app to notify you" when they already did on their phone reads
  // as though the earlier answer was lost.
  let allowedOnAnotherDevice = $state(false);

  const canSubscribeHere = pushCapable();

  // Resolved after the screen is already up, deliberately: the decision to SHOW
  // it is local and synchronous so nothing is added to the pre-redirect path,
  // and only the wording depends on this.
  onMount(() => {
    void (async () => {
      if (!canSubscribeHere) return;
      try {
        const { actor, identityNumber } = $authenticatedStore;
        const [origins, registration] = await Promise.all([
          listConsentedOrigins(actor, identityNumber),
          navigator.serviceWorker.getRegistration(),
        ]);
        if (!origins.includes(effectiveOrigin)) return;
        const subscription = await registration?.pushManager.getSubscription();
        allowedOnAnotherDevice =
          subscription === null || subscription === undefined;
      } catch {
        // Leave the first-run wording: it is never wrong, only less specific.
      }
    })();
  });

  // Consent (per identity + origin) and a subscription (per device) are separate
  // grants. Splitting them is what lets iOS grant consent in a tab now and
  // subscribe from the installed app later.
  const grantConsent = async (): Promise<void> => {
    const { actor, identityNumber } = $authenticatedStore;
    await actor
      .push_grant_consent(identityNumber, effectiveOrigin)
      .then(throwTextCanisterError);
  };

  const subscribeThisDevice = async (): Promise<void> => {
    const { actor, identityNumber } = $authenticatedStore;
    await ensureDeviceSubscription(actor, identityNumber);
    await grantConsent();
    // Mint the read-only delegation the service worker will use to pull this
    // app's notification content, and store it where the worker can read it.
    // Non-fatal: without it the worker falls back to a generic notification.
    try {
      const record = await mintPushDelegation({
        identityNumber,
        origin: effectiveOrigin,
        actor,
      });
      await storePushDelegation(record);
    } catch (err) {
      console.warn("[push] could not mint pull delegation:", err);
    }
  };

  const remember = (decision: "enabled" | "dismissed") => {
    recordNotifOptInDecision(
      $authenticatedStore.identityNumber,
      effectiveOrigin,
      decision,
    );
  };

  // Layer 1 (browser permission for II's origin) is granted once and shared by
  // every dApp; layer 2 (this app's consent) is asked per app in II's own UI.
  // The screen looks the same either way — only the browser prompt is skipped
  // once layer 1 is in place, since there is nothing left for it to ask.
  const globallyGranted = notificationsGloballyGranted();

  const handleEnable = async () => {
    if (!canSubscribeHere && !deferred) {
      onContinue();
      return;
    }
    enabling = true;
    enableError = undefined;
    try {
      if (deferred) {
        // iOS tab: record the consent, which is all this context can do, and
        // turn the screen into the "finish on your device" step. The
        // subscription is created later when they open II's installed app and
        // turn notifications on there — consent is already waiting for it.
        await grantConsent();
        remember("enabled");
        enabling = false;
        finishOnDevice = true;
        return;
      }
      // Already-granted permission resolves immediately without prompting, but
      // skip the call entirely so the intent is explicit: nothing browser-native
      // happens on the per-app path.
      const permission = globallyGranted
        ? "granted"
        : await Notification.requestPermission();
      if (permission === "granted") {
        await subscribeThisDevice();
        remember("enabled");
      } else {
        // Blocked or dismissed at the browser prompt. Either way the user has
        // answered, so don't re-ask on the next sign-in.
        remember("dismissed");
      }
    } catch (error) {
      // Deliberately not remembered: a failure here (canister call, service
      // worker registration) is our problem, not a user decision, so they
      // should get the chance again rather than silently losing the feature.
      //
      // Surfaced rather than only logged, because the symptom of a swallowed
      // failure here is "the screen keeps coming back" — indistinguishable
      // from a bug in the remembering itself.
      console.warn("Enable notifications failed:", error);
      enableError = error instanceof Error ? error.message : String(error);
      enabling = false;
      return;
    }
    onContinue();
  };

  const handleMaybeLater = () => {
    remember("dismissed");
    onContinue();
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
    {#if allowedOnAnotherDevice}
      {$t`Also notify you on this device?`}
    {:else}
      {$t`Let ${dappName} notify you`}
    {/if}
  </h1>
  {#if allowedOnAnotherDevice}
    <!-- Consent is already granted for this app; only this browser is missing a
         subscription. Saying "allow this app to notify you" here would read as
         though the earlier answer had been lost. -->
    <p class="text-text-secondary mt-2 text-sm leading-relaxed">
      <Trans>
        You already allowed {dappName} to notify you. Turn it on for this device too.
      </Trans>
    </p>
  {:else}
    <p class="text-text-secondary mt-2 text-sm leading-relaxed">
      <Trans>
        Receive alerts from the apps you approve, and stay on top of what
        matters seamlessly.
      </Trans>
    </p>
  {/if}

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

  {#if enableError !== undefined}
    <div
      class="border-border-secondary bg-bg-secondary mt-5 rounded-xl border p-3"
      role="alert"
    >
      <div class="text-text-primary text-sm font-semibold">
        {$t`Couldn't turn on notifications`}
      </div>
      <p class="text-text-tertiary mt-1 text-sm break-words">
        {enableError}
      </p>
    </div>
  {/if}

  {#if finishOnDevice}
    <!-- iOS: consent is granted; the subscription still has to be created from
         II's installed app, which is the only place iOS exposes Web Push. -->
    <div
      class="border-border-secondary bg-bg-secondary mt-5 rounded-xl border p-4"
    >
      <div class="text-text-primary text-sm font-semibold">
        {$t`One more step on your device`}
      </div>
      <p class="text-text-tertiary mt-1 text-sm leading-relaxed">
        <Trans>
          {dappName} can now notify you. To receive alerts on this iPhone or iPad,
          add Internet Identity to your Home Screen, open it, and turn on notifications
          under Settings.
        </Trans>
      </p>
    </div>
  {/if}

  <div class="mt-6 flex flex-col gap-2">
    {#if finishOnDevice}
      <button class="btn btn-primary btn-xl w-full" onclick={onContinue}>
        <span>{$t`Continue`}</span>
      </button>
    {:else}
      <button
        class="btn btn-primary btn-xl w-full"
        onclick={handleEnable}
        disabled={enabling}
      >
        {#if enabling}
          <ProgressRing />
          <span>{$t`Turning on...`}</span>
        {:else if allowedOnAnotherDevice}
          <span>{$t`Enable on this device`}</span>
        {:else}
          <span>{$t`Enable notifications`}</span>
        {/if}
      </button>
      <button
        class="btn btn-tertiary btn-lg w-full"
        onclick={handleMaybeLater}
        disabled={enabling}
      >
        {$t`Maybe later`}
      </button>
    {/if}
  </div>
</div>
