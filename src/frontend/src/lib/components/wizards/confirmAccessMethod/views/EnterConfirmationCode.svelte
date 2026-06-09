<script lang="ts">
  import ConfirmDeviceIllustration from "$lib/components/illustrations/ConfirmDeviceIllustration.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import CodeInput from "$lib/components/ui/CodeInput.svelte";
  import HoldToConfirm from "$lib/components/ui/HoldToConfirm.svelte";
  import {
    ChevronLeftIcon,
    CircleCheckIcon,
    RotateCcwIcon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";

  const host = typeof window === "undefined" ? "" : window.location.host;

  const CODE_LENGTH = 6;
  // Non-breaking space rendered through CodeInput's `hint` slot to reserve
  // the line's vertical space when no error is shown — keeping the slot
  // mounted prevents the layout from shifting when the error appears.
  const HINT_PLACEHOLDER = " ";

  const SLIDE_DURATION = 380;
  const HOLD_ADVANCE_DELAY = 250;

  interface Props {
    confirm: (confirmationCode: string) => Promise<void>;
    restart: () => void;
  }

  const { confirm, restart }: Props = $props();

  let step = $state(0);
  let holdCompleted = $state(false);

  let inputRef = $state<HTMLInputElement>();
  let confirmationCode = $state<string>("");
  let isConfirming = $state(false);
  let isInvalidCode = $state(false);

  let step0El = $state<HTMLDivElement>();
  let step1El = $state<HTMLDivElement>();
  let wrapperHeight = $state<number | undefined>(undefined);
  let advanceTimer: ReturnType<typeof setTimeout> | undefined;

  const handleSubmit = async () => {
    isConfirming = true;
    try {
      await confirm(confirmationCode);
    } catch {
      isInvalidCode = true;
      confirmationCode = "";
    } finally {
      isConfirming = false;
    }
  };

  const goToStep1 = () => {
    advanceTimer = undefined;
    step = 1;
  };

  const goToStep0 = () => {
    if (advanceTimer !== undefined) {
      clearTimeout(advanceTimer);
      advanceTimer = undefined;
    }
    step = 0;
    holdCompleted = false;
  };

  const handleRestart = () => {
    if (advanceTimer !== undefined) {
      clearTimeout(advanceTimer);
      advanceTimer = undefined;
    }
    restart();
  };

  $effect(() => () => {
    if (advanceTimer !== undefined) {
      clearTimeout(advanceTimer);
    }
  });

  $effect(() => {
    if (confirmationCode.length > 0) {
      isInvalidCode = false;
    }
  });

  $effect(() => {
    if (step === 1 && !isConfirming) {
      inputRef?.focus();
    }
  });

  $effect(() => {
    const activeEl = step === 0 ? step0El : step1El;
    if (activeEl === undefined) return;

    const observer = new ResizeObserver(() => {
      wrapperHeight = activeEl.scrollHeight;
    });
    observer.observe(activeEl);
    wrapperHeight = activeEl.scrollHeight;

    return () => observer.disconnect();
  });
</script>

<div
  class="overflow-hidden transition-[height]"
  style="height: {wrapperHeight !== undefined
    ? `${wrapperHeight}px`
    : 'auto'}; transition-duration: {SLIDE_DURATION}ms; transition-timing-function: cubic-bezier(0.2, 0, 0, 1);"
>
  <div
    class="flex items-start transition-transform will-change-transform"
    style="transform: translateX({step === 0
      ? '0'
      : '-100%'}); transition-duration: {SLIDE_DURATION}ms; transition-timing-function: cubic-bezier(0.2, 0, 0, 1);"
  >
    <div
      bind:this={step0El}
      aria-hidden={step !== 0 ? true : undefined}
      inert={step !== 0 ? true : undefined}
      class="flex w-full shrink-0 flex-col items-stretch"
    >
      <div class={["illustration self-center max-sm:hidden"]}>
        <ConfirmDeviceIllustration class="text-text-primary mt-4 mb-8 h-32" />
      </div>
      <h1 class="text-text-primary mb-2 text-2xl font-medium">
        {$t`Authorize new device`}
      </h1>
      <p class="text-text-tertiary mb-5 text-base">
        {$t`You're about to authorize a new device.`}
      </p>
      <div class="border-border-secondary border-t"></div>
      <p class="text-text-primary mt-5 mb-4 text-base font-semibold">
        {$t`Please confirm`}
      </p>
      <ul class="mb-8 flex flex-col">
        <li class="flex items-start gap-3 py-2 first:pt-0 last:pb-0">
          <CircleCheckIcon
            class="text-text-tertiary mt-0.5 size-5 shrink-0"
            aria-hidden="true"
          />
          <p class="text-text-primary text-base font-semibold">
            {$t`I control both devices.`}
          </p>
        </li>
        <li class="flex items-start gap-3 py-2 first:pt-0 last:pb-0">
          <CircleCheckIcon
            class="text-text-tertiary mt-0.5 size-5 shrink-0"
            aria-hidden="true"
          />
          <div class="flex flex-col items-start gap-1.5">
            <p class="text-text-primary text-base font-semibold">
              {$t`Both devices are on the same domain:`}
            </p>
            <code
              class="bg-bg-tertiary text-text-primary rounded-md px-2 py-1 font-mono text-sm"
            >
              {host}
            </code>
          </div>
        </li>
      </ul>
      <p class="text-text-tertiary mb-2 text-center text-xs">
        {$t`Hold for a few seconds. Release to cancel.`}
      </p>
      <HoldToConfirm
        label={$t`Hold to confirm`}
        completed={holdCompleted}
        class="mb-2"
        onComplete={() => {
          holdCompleted = true;
          advanceTimer = setTimeout(goToStep1, HOLD_ADVANCE_DELAY);
        }}
      />
      <button
        class="btn btn-secondary btn-xl"
        onclick={handleRestart}
        type="button"
      >
        <RotateCcwIcon class="size-5" />
        <span>{$t`Start over`}</span>
      </button>
    </div>

    <div
      bind:this={step1El}
      aria-hidden={step !== 1 ? true : undefined}
      inert={step !== 1 ? true : undefined}
      class="w-full shrink-0"
    >
      <form
        class="flex flex-col items-stretch"
        onsubmit={(e) => {
          e.preventDefault();
          void handleSubmit();
        }}
      >
        <div class="mb-6 flex items-center gap-3">
          <button
            type="button"
            class="btn btn-tertiary btn-icon btn-sm rounded-full"
            aria-label={$t`Back`}
            onclick={goToStep0}
          >
            <ChevronLeftIcon class="size-4" />
          </button>
          <span
            class="text-text-tertiary text-xs font-semibold tracking-wider uppercase"
          >
            {$t`Step 2 of 2`}
          </span>
        </div>
        <h1 class="text-text-primary mb-3 text-2xl font-medium">
          {$t`Enter the code`}
        </h1>
        <p class="text-text-tertiary mb-4 text-base font-medium">
          {$t`Type the 6-digit code shown on the new device.`}
        </p>
        <CodeInput
          bind:element={inputRef}
          bind:value={confirmationCode}
          length={CODE_LENGTH}
          class="mb-3"
          error={isInvalidCode
            ? $t`Invalid code. Please check and try again.`
            : undefined}
          hint={HINT_PLACEHOLDER}
          disabled={isConfirming}
        />
        <button
          class="btn btn-primary btn-xl mb-3"
          type="submit"
          disabled={confirmationCode.length < CODE_LENGTH || isConfirming}
        >
          {#if isConfirming}
            <ProgressRing />
            <span>{$t`Confirming...`}</span>
          {:else}
            <span>{$t`Confirm sign-in`}</span>
          {/if}
        </button>
        <button
          class="btn btn-secondary btn-xl"
          onclick={handleRestart}
          type="button"
          disabled={isConfirming}
        >
          <RotateCcwIcon class="size-5" />
          <span>{$t`Start over`}</span>
        </button>
      </form>
    </div>
  </div>
</div>

<style>
  @media (max-height: 700px) {
    /*noinspection CssUnusedSymbol*/
    .illustration {
      display: none !important;
    }
  }
</style>
