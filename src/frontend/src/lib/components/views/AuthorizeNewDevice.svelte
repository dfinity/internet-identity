<script lang="ts">
  import { onMount } from "svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ConfirmDeviceIllustration from "$lib/components/illustrations/ConfirmDeviceIllustration.svelte";

  interface Props {
    confirm: (confirmationCode: string) => void;
    restart: () => void;
  }

  const { confirm, restart }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let confirmationCode = $state("");
  let loading = $state(false);

  const handleSubmit = () => {
    loading = true;
    confirm(confirmationCode);
  };

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
  />
  <Button
    onclick={handleSubmit}
    variant="primary"
    size="xl"
    type="submit"
    disabled={confirmationCode.length === 0 || loading}
    class="mb-3"
  >
    Confirm sign-in
  </Button>
  <Button onclick={restart} variant="secondary" size="xl" disabled={loading}>
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
