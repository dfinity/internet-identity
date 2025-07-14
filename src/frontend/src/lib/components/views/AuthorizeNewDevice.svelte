<script lang="ts">
  import { onMount } from "svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ConfirmDeviceIllustration from "$lib/components/illustrations/ConfirmDeviceIllustration.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    confirm: (confirmationCode: string) => Promise<void>;
    restart: () => void;
  }

  const { confirm, restart }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let confirmationCode = $state("");
  let isConfirming = $state(false);
  let isInvalidCode = $state(true);

  const handleSubmit = async () => {
    isConfirming = true;
    try {
      await confirm(confirmationCode);
    } catch (error) {
      isInvalidCode = true;
    } finally {
      isConfirming = false;
    }
  };

  $effect(() => {
    if (confirmationCode.length > 0) {
      isInvalidCode = false;
    }
  });

  onMount(() => {
    inputRef?.focus();
  });
</script>

<form class="flex flex-col items-stretch">
  <div class={["illustration self-center"]}>
    <ConfirmDeviceIllustration class="text-text-primary mt-4 mb-8 h-32" />
  </div>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">
    Authorize new device
  </h1>
  <p class="text-md text-text-tertiary mb-8 font-medium">
    You're about to sign in on a <b class="text-text-primary">new device</b>.
    <br /><br />
    Only enter the code from that device if you initiated this process
    <b class="text-text-primary">yourself</b>.
    <br /><br />
    <b class="text-text-primary">Never</b> enter a code from another source.
  </p>
  <Input
    bind:value={confirmationCode}
    placeholder="XXXXXX"
    size="md"
    class="mb-8"
    error={isInvalidCode ? "Invalid code, please try again" : undefined}
  />
  <Button
    onclick={handleSubmit}
    variant="primary"
    size="xl"
    type="submit"
    disabled={confirmationCode.length === 0 || isConfirming}
    class="mb-3"
  >
    {#if isConfirming}
      <ProgressRing />
      <span>Confirming...</span>
    {:else}
      <span>Confirm sign-in</span>
    {/if}
  </Button>
  <Button
    onclick={restart}
    variant="secondary"
    size="xl"
    disabled={isConfirming}
  >
    Start over
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
