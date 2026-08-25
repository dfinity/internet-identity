<script lang="ts">
  import { ZapIcon, ClockIcon, ShieldIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    /** dApp name for the heading, or undefined when it isn't known. */
    appName: string | undefined;
    /** True while the enable request is in flight. */
    busy: boolean;
    onEnable: () => void;
    onSkip: () => void;
  }

  const { appName, busy, onEnable, onSkip }: Props = $props();

  const app = $derived(appName ?? $t`this app`);
</script>

<div
  class="flex flex-1 flex-col items-stretch p-4 sm:max-w-100 sm:justify-center sm:self-center"
>
  <!-- Lock-screen notification preview -->
  <div
    class="border-border-tertiary bg-bg-primary_alt relative overflow-hidden rounded-2xl border p-5 pb-8"
  >
    <span
      class="border-border-tertiary bg-bg-secondary text-text-tertiary mb-4 inline-flex rounded-full border px-2.5 py-1 text-xs font-semibold"
    >
      {$t`Example`}
    </span>
    <div
      class="border-border-tertiary text-text-secondary mx-6 h-5 rounded-t-xl border border-b-0 bg-white/2"
    ></div>
    <div
      class="border-border-tertiary mx-3 -mt-1.5 flex items-start gap-3 rounded-2xl border bg-white/3 p-3 opacity-90"
    >
      <span
        class="border-border-tertiary bg-bg-tertiary size-8 shrink-0 rounded-lg border"
      ></span>
      <div class="min-w-0 flex-1">
        <div class="flex items-baseline justify-between gap-2">
          <span class="text-text-primary text-[13px] font-semibold"
            >{$t`A marketplace`}</span
          >
          <span class="text-text-tertiary text-[11px]">2m</span>
        </div>
        <div class="text-text-secondary text-[13px]">
          {$t`Your item just sold.`}
        </div>
      </div>
    </div>
    <div
      class="border-border-secondary -mt-8 flex items-start gap-3 rounded-2xl border bg-white/8 p-3 shadow-lg backdrop-blur"
    >
      <span
        class="border-border-tertiary bg-bg-tertiary size-8 shrink-0 rounded-lg border"
      ></span>
      <div class="min-w-0 flex-1">
        <div class="flex items-baseline justify-between gap-2">
          <span class="text-text-primary text-[13px] font-semibold"
            >{$t`Your wallet`}</span
          >
          <span class="text-text-tertiary text-[11px]">{$t`now`}</span>
        </div>
        <div class="text-text-secondary text-[13px]">
          {$t`Your transfer is confirmed.`}
        </div>
      </div>
    </div>
  </div>

  <h1 class="text-text-primary mt-6 text-2xl font-medium text-balance">
    {$t`Let ${app} notify you`}
  </h1>
  <p class="text-text-secondary mt-2 text-sm">
    <Trans>
      Receive alerts from the apps you approve, and stay on top of what matters.
    </Trans>
  </p>

  <div class="mt-6 flex flex-col gap-4">
    <div class="flex items-start gap-3.5">
      <span
        class="border-border-secondary bg-bg-secondary text-text-primary flex size-9 shrink-0 items-center justify-center rounded-full border"
      >
        <ZapIcon class="size-4.5" aria-hidden="true" />
      </span>
      <div>
        <div class="text-text-primary text-[15px] font-semibold">
          {$t`Instant activity alerts`}
        </div>
        <div class="text-text-tertiary mt-0.5 text-sm">
          {$t`Transfers, replies and mentions the moment they happen.`}
        </div>
      </div>
    </div>
    <div class="flex items-start gap-3.5">
      <span
        class="border-border-secondary bg-bg-secondary text-text-primary flex size-9 shrink-0 items-center justify-center rounded-full border"
      >
        <ClockIcon class="size-4.5" aria-hidden="true" />
      </span>
      <div>
        <div class="text-text-primary text-[15px] font-semibold">
          {$t`Reachable anytime`}
        </div>
        <div class="text-text-tertiary mt-0.5 text-sm">
          {$t`Updates reach your device without keeping the app open.`}
        </div>
      </div>
    </div>
    <div class="flex items-start gap-3.5">
      <span
        class="border-border-secondary bg-bg-secondary text-text-primary flex size-9 shrink-0 items-center justify-center rounded-full border"
      >
        <ShieldIcon class="size-4.5" aria-hidden="true" />
      </span>
      <div>
        <div class="text-text-primary text-[15px] font-semibold">
          {$t`Private by design`}
        </div>
        <div class="text-text-tertiary mt-0.5 text-sm">
          {$t`Delivery routes through your identity. Revoke access anytime.`}
        </div>
      </div>
    </div>
  </div>

  <div class="mt-7 flex flex-col gap-2.5">
    <button class="btn btn-primary" onclick={onEnable} disabled={busy}>
      {busy ? $t`Setting up…` : $t`Enable notifications`}
    </button>
    <button class="btn btn-tertiary" onclick={onSkip} disabled={busy}>
      {$t`Maybe later`}
    </button>
  </div>
</div>
