<script lang="ts">
  import { fly } from "svelte/transition";
  import SmileyWritingIllustration from "$lib/components/illustrations/SmileyWritingIllustration.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";

  interface Props {
    create: () => Promise<void | "cancelled">;
  }

  const { create }: Props = $props();

  let isCreating = $state(false);
  let isCancelled = $state(false);

  const handleCreate = async () => {
    isCreating = true;
    const result = await create();
    isCreating = false;

    if (result === "cancelled") {
      isCancelled = true;
      await waitFor(4000);
      isCancelled = false;
    }
  };
</script>

<div class="flex flex-1 flex-col" in:fly={{ duration: 200, x: 10 }}>
  <div
    class="text-text-primary mb-8 flex w-full flex-col items-center justify-center"
  >
    <SmileyWritingIllustration class="my-5 h-22" />
    <div>
      <h1 class="mb-3 text-2xl font-medium sm:text-center">
        You are about to create a passkey
      </h1>
      <p
        class="text-md text-text-tertiary font-medium text-balance sm:text-center"
      >
        A new passkey will be created using your biometrics and stored in your
        password manager.
      </p>
    </div>
  </div>
  <div class="flex flex-col items-stretch gap-4">
    <Tooltip
      label="Interaction canceled. Please try again."
      hidden={!isCancelled}
      manual
    >
      <Button
        onclick={handleCreate}
        variant="primary"
        size="lg"
        type="submit"
        disabled={isCreating}
      >
        {#if isCreating}
          <ProgressRing />
          <span>Creating Passkey...</span>
        {:else}
          <span>Create Passkey</span>
        {/if}
      </Button>
    </Tooltip>
  </div>
</div>
