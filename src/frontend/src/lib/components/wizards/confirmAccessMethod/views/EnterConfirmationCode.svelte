<script lang="ts">
  import ConfirmDeviceIllustration from "$lib/components/illustrations/ConfirmDeviceIllustration.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import CodeInput from "$lib/components/ui/CodeInput.svelte";
  import HoldToConfirm from "$lib/components/ui/HoldToConfirm.svelte";
  import { ChevronLeftIcon, RotateCcwIcon, SquareIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  const host = typeof window === "undefined" ? "" : window.location.host;

  const CODE_LENGTH = 6;
  // Non-breaking space rendered through CodeInput's `hint` slot to reserve
  // the line's vertical space when no error is shown — keeping the slot
  // mounted prevents the layout from shifting when the error appears.
  const HINT_PLACEHOLDER = " ";

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
    step = 1;
  };

  const goToStep0 = () => {
    step = 0;
    holdCompleted = false;
  };

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
      <h1 class="text-text-primary mb-3 text-2xl font-medium">
        {$t`Authorize new device`}
      </h1>
      <p class="text-text-tertiary mb-4 text-base font-medium">
        <Trans>
          You're about to authorize a <b class="text-text-primary">new device</b
          >.
        </Trans>
      </p>
      <p class="text-text-primary mb-4 text-base font-medium">
        {$t`Before continuing, please check:`}
      </p>
      <ul class="mb-8 flex flex-col gap-3">
        <li
          class="text-text-tertiary flex items-start gap-3 text-base font-medium"
        >
          <SquareIcon
            class="text-text-tertiary mt-0.5 size-5 shrink-0"
            strokeWidth={2.5}
            aria-hidden="true"
          />
          <span>
            <Trans>
              <b class="text-text-primary">You</b> started this from the new device
            </Trans>
          </span>
        </li>
        <li
          class="text-text-tertiary flex items-start gap-3 text-base font-medium"
        >
          <SquareIcon
            class="text-text-tertiary mt-0.5 size-5 shrink-0"
            strokeWidth={2.5}
            aria-hidden="true"
          />
          <span>
            <Trans>
              The new device is on <b class="text-text-primary">{host}</b>
            </Trans>
          </span>
        </li>
        <li
          class="text-text-tertiary flex items-start gap-3 text-base font-medium"
        >
          <SquareIcon
            class="text-text-tertiary mt-0.5 size-5 shrink-0"
            strokeWidth={2.5}
            aria-hidden="true"
          />
          <span>
            <Trans>
              You <b class="text-text-primary">control and trust</b> the new device
            </Trans>
          </span>
        </li>
      </ul>
      <HoldToConfirm
        label={$t`Hold to confirm you started this`}
        completed={holdCompleted}
        class="mb-2"
        onComplete={() => {
          holdCompleted = true;
          setTimeout(goToStep1, HOLD_ADVANCE_DELAY);
        }}
      />
      <p class="text-text-tertiary mb-6 text-center text-xs">
        {$t`Hold for 1.5 seconds. Release to cancel.`}
      </p>
      <button class="btn btn-secondary btn-xl" onclick={restart} type="button">
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
          onclick={handleSubmit}
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
          onclick={restart}
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
