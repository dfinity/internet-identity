<script lang="ts">
  import GlobeIcon from "@lucide/svelte/icons/globe";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    /** dApp name for the copy, or undefined when it isn't known. */
    appName: string | undefined;
    /** The app the notifications would come from. Its hostname stands in for the
     * name where the app has published none, which is what a delivered
     * notification does too. */
    origin: string;
    /** Its published logo. The placeholder stands in where an app has none, as the
     * hostname stands in where it has published no name. */
    appLogo: string | undefined;
    /** True while the request is in flight. */
    busy: boolean;
    onEnable: () => void;
    onSkip: () => void;
  }

  const { appName, appLogo, origin, busy, onEnable, onSkip }: Props = $props();

  const app = $derived(appName ?? $t`this app`);
  /** What a delivered notification leads its title with. See `senderOf` in the
   *  worker's wake-up, which resolves the same two in the same order. */
  const sender = $derived(appName ?? new URL(origin).hostname);
</script>

<!-- The tile an app's logo sits in. The border and fill are what make the globe
     read as one when an app has published no logo; a logo of its own fills the
     tile, and ringing it would be drawing on the app's artwork. -->
{#snippet icon()}
  {#if appLogo === undefined}
    <span
      class="border-border-tertiary bg-bg-tertiary text-fg-primary flex h-[39px] w-[39px] shrink-0 items-center justify-center rounded-lg border"
      ><GlobeIcon class="size-4" /></span
    >
  {:else}
    <img
      src={appLogo}
      alt=""
      class="h-[39px] w-[39px] shrink-0 rounded-lg object-contain"
    />
  {/if}
{/snippet}

<div class="flex min-w-0 flex-col items-stretch">
  <!-- Ported from the design. Hidden from assistive technology: the messages are
       made up, and a screen reader would read them ahead of the heading as though
       they were real. -->
  <div aria-hidden="true" class="relative h-[113px]">
    <div
      class="absolute top-0 right-0 left-0 z-1 h-[66px] origin-top scale-[0.84]"
    >
      <div class="absolute inset-0 rounded-2xl backdrop-blur-sm"></div>
      <div
        class="border-border-secondary relative h-full rounded-2xl border bg-white/8 opacity-30 shadow-lg"
      >
        <div class="flex items-start gap-3 p-3">
          {@render icon()}
          <div class="min-w-0 flex-1">
            <div class="flex items-baseline justify-between gap-2">
              <span
                class="text-text-primary min-w-0 overflow-hidden text-[13px] font-semibold text-ellipsis whitespace-nowrap"
                >{sender} • {$t`Reminder`}</span
              >
              <span class="text-text-tertiary text-[11px]">5m</span>
            </div>
            <div class="text-text-secondary text-[13px]">
              {$t`Your event starts in 15 minutes.`}
            </div>
          </div>
        </div>
      </div>
    </div>
    <div
      class="absolute top-[21px] right-0 left-0 z-2 h-[66px] origin-top scale-[0.92]"
    >
      <div class="absolute inset-0 rounded-2xl backdrop-blur-sm"></div>
      <div
        class="border-border-secondary relative h-full rounded-2xl border bg-white/8 opacity-60 shadow-lg"
      >
        <div class="flex items-start gap-3 p-3">
          {@render icon()}
          <div class="min-w-0 flex-1">
            <div class="flex items-baseline justify-between gap-2">
              <span
                class="text-text-primary min-w-0 overflow-hidden text-[13px] font-semibold text-ellipsis whitespace-nowrap"
                >{sender} • {$t`Request approved`}</span
              >
              <span class="text-text-tertiary text-[11px]">2m</span>
            </div>
            <div class="text-text-secondary text-[13px]">
              {$t`Your request was approved.`}
            </div>
          </div>
        </div>
      </div>
    </div>
    <div
      class="absolute top-[47px] right-0 left-0 z-3 h-[66px] origin-top scale-100"
    >
      <div class="absolute inset-0 rounded-2xl backdrop-blur-sm"></div>
      <div
        class="border-border-secondary relative h-full rounded-2xl border bg-white/8 opacity-100 shadow-lg"
      >
        <div class="flex items-start gap-3 p-3">
          {@render icon()}
          <div class="min-w-0 flex-1">
            <div class="flex items-baseline justify-between gap-2">
              <span
                class="text-text-primary min-w-0 overflow-hidden text-[13px] font-semibold text-ellipsis whitespace-nowrap"
                >{sender} • {$t`New message`}</span
              >
              <span class="text-text-tertiary text-[11px]">now</span>
            </div>
            <div class="text-text-secondary text-[13px]">
              <Trans>You have 1 new message.</Trans>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>

  <h1
    class="text-text-primary mt-6 text-2xl leading-8 font-medium text-balance"
  >
    {$t`Let ${app} notify you`}
  </h1>
  <p class="text-text-secondary mt-2 text-sm leading-5 text-pretty">
    <Trans>
      Get notifications from this app when something needs your attention. You
      can turn them off anytime.
    </Trans>
  </p>

  <div class="mt-7 flex flex-col gap-2.5">
    <button class="btn btn-primary btn-xl" onclick={onEnable} disabled={busy}>
      {busy ? $t`Setting up…` : $t`Allow`}
    </button>
    <button class="btn btn-tertiary btn-xl" onclick={onSkip} disabled={busy}>
      {$t`Not now`}
    </button>
  </div>
</div>
