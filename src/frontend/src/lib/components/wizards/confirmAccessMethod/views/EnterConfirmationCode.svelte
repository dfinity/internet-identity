<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import ConfirmDeviceIllustration from "$lib/components/illustrations/ConfirmDeviceIllustration.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import CodeInput from "$lib/components/ui/CodeInput.svelte";
  import { RotateCcwIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  const CODE_LENGTH = 6;
  // Non-breaking space rendered through CodeInput's `hint` slot to reserve
  // the line's vertical space when no error is shown — keeping the slot
  // mounted prevents the layout from shifting when the error appears.
  const HINT_PLACEHOLDER = " ";

  interface Props {
    confirm: (confirmationCode: string) => Promise<void>;
    restart: () => void;
  }

  const { confirm, restart }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let confirmationCode = $state<string>("");
  let isConfirming = $state(false);
  let isInvalidCode = $state(false);

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

  $effect(() => {
    if (confirmationCode.length > 0) {
      isInvalidCode = false;
    }
  });

  $effect(() => {
    if (!isConfirming) {
      inputRef?.focus();
    }
  });
</script>

<form class="flex flex-col items-stretch">
  <div class={["illustration self-center max-sm:hidden"]}>
    <ConfirmDeviceIllustration class="text-text-primary mt-4 mb-8 h-32" />
  </div>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">
    {$t`Authorize new device`}
  </h1>
  <p class="text-text-tertiary mb-4 text-base font-medium">
    <Trans>
      You're about to sign in on a <b class="text-text-primary">new device</b>.
    </Trans>
  </p>
  <p class="text-text-tertiary mb-4 text-base font-medium">
    <Trans>
      Only enter the code from that device if you initiated this process
      <b class="text-text-primary">yourself</b>.
    </Trans>
  </p>
  <p class="text-text-tertiary mb-8 text-base font-medium">
    <Trans>
      <b class="text-text-primary">Never</b> enter a code from another source.
    </Trans>
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
  <Button
    onclick={handleSubmit}
    variant="primary"
    size="xl"
    type="submit"
    disabled={confirmationCode.length < CODE_LENGTH || isConfirming}
    class="mb-3"
  >
    {#if isConfirming}
      <ProgressRing />
      <span>{$t`Confirming...`}</span>
    {:else}
      <span>{$t`Confirm sign-in`}</span>
    {/if}
  </Button>
  <Button
    onclick={restart}
    variant="secondary"
    size="xl"
    disabled={isConfirming}
  >
    <RotateCcwIcon class="size-5" />
    <span>{$t`Start over`}</span>
  </Button>
</form>

<style>
  @media (max-height: 700px) {
    /*noinspection CssUnusedSymbol*/
    .illustration {
      display: none !important;
    }
  }
</style>
