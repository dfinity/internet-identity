<script lang="ts">
  import { fly } from "svelte/transition";
  import Button from "$lib/components/ui/Button.svelte";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { handleError } from "$lib/components/utils/error";

  interface Props {
    setupNew: () => void;
    useExisting: () => Promise<void>;
    onError?: (error: unknown) => void;
  }

  const { setupNew, useExisting, onError = handleError }: Props = $props();

  let loading = $state(false);

  const handleUseExisting = async () => {
    loading = true;
    try {
      await useExisting();
    } catch (error) {
      onError(error);
    } finally {
      loading = false;
    }
  };
</script>

<div class="mt-4 mb-8 flex flex-col" in:fly={{ duration: 200, x: -10 }}>
  <PasskeyIllustration class="text-text-primary mb-8 h-32" />
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    Simplify your sign-in
  </h1>
  <p class="text-md text-text-tertiary font-medium text-balance sm:text-center">
    With passkeys, you can now use your fingerprint, face, or screen lock to
    quickly and securely confirm it’s really you.
  </p>
</div>
<div class="flex flex-col gap-3">
  <Button onclick={setupNew} size="lg" disabled={loading}
    >Set up a new Passkey</Button
  >
  <Button
    onclick={handleUseExisting}
    variant="secondary"
    size="lg"
    disabled={loading}
  >
    {#if loading}
      <ProgressRing />
      <span>Authenticating...</span>
    {:else}
      <span>Use an existing Passkey</span>
    {/if}
  </Button>
</div>
