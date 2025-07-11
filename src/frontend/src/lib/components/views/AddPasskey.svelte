<script lang="ts">
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";

  interface Props {
    createPasskey: () => Promise<void>;
    continueOnAnotherDevice: () => void;
    isUsingPasskeys?: boolean;
  }

  const { createPasskey, continueOnAnotherDevice, isUsingPasskeys }: Props =
    $props();

  let isCreatingPasskey = $state(false);

  const handleCreatePasskey = async () => {
    isCreatingPasskey = true;
    try {
      await createPasskey();
    } finally {
      isCreatingPasskey = false;
    }
  };
</script>

<div class="mt-4 mb-6 flex flex-col">
  <PasskeyIllustration class="text-text-primary mb-8 h-32" />
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    {#if isUsingPasskeys}
      Add another passkey
    {:else}
      Add a passkey
    {/if}
  </h1>
  <p class="text-md text-text-tertiary font-medium text-balance sm:text-center">
    With passkeys, you can now use your fingerprint, face, or screen lock to
    quickly and securely confirm it’s really you.
  </p>
</div>
<div class="flex flex-col gap-3">
  <Button onclick={handleCreatePasskey} size="lg" disabled={isCreatingPasskey}>
    {#if isCreatingPasskey}
      <ProgressRing />
      <span>Creating passkey...</span>
    {:else}
      <span>Create passkey</span>
    {/if}
  </Button>
  <Button
    onclick={continueOnAnotherDevice}
    variant="tertiary"
    size="lg"
    disabled={isCreatingPasskey}
  >
    Continue on another device
  </Button>
</div>
