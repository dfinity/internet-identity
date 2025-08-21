<script lang="ts">
  import { fly } from "svelte/transition";
  import { onMount } from "svelte";
  import SmileyWritingIllustration from "$lib/components/illustrations/SmileyWritingIllustration.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import {
    upgradeIdentityFunnel,
    UpgradeIdentityEvents,
  } from "$lib/utils/analytics/upgradeIdentityFunnel";

  interface Props {
    create: (name: string) => Promise<void | "cancelled">;
    buttonLabel?: string;
    loadingLabel?: string;
    identityNumber?: bigint;
  }

  const {
    create,
    buttonLabel = "Create Passkey",
    loadingLabel = "Creating Passkey...",
    identityNumber,
  }: Props = $props();

  onMount(() => {
    upgradeIdentityFunnel.trigger(UpgradeIdentityEvents.CreatePasskeyScreen);
  });

  let inputRef = $state<HTMLInputElement>();
  let name = $state("");
  let isCreating = $state(false);
  let isCancelled = $state(false);

  const handleCreate = async () => {
    isCreating = true;
    const result = await create(name.trim());
    isCreating = false;

    if (result === "cancelled") {
      isCancelled = true;
      await waitFor(4000);
      isCancelled = false;
    }
  };

  onMount(() => {
    inputRef?.focus();
  });
</script>

<form class="flex flex-1 flex-col" in:fly={{ duration: 200, x: 10 }}>
  <div
    class="text-text-primary mb-8 flex w-full flex-col items-center justify-center"
  >
    <SmileyWritingIllustration class="my-5 h-22" />
    <div>
      <h1 class="mb-3 text-2xl font-medium sm:text-center">
        Name your identity
      </h1>
      <p
        class="text-md text-text-tertiary font-medium text-balance sm:text-center"
      >
        This will label your passkey, and you can't rename it later once set.
      </p>
    </div>
  </div>
  <div class="flex flex-col items-stretch gap-4">
    <Input
      bind:element={inputRef}
      bind:value={name}
      inputmode="text"
      placeholder="Identity name"
      hint="Pick something recognizable, like your name."
      type="text"
      size="md"
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      disabled={isCreating}
      error={name.length > 64 ? "Maximum length is 64 characters." : undefined}
      aria-label="Identity name"
    />
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
        disabled={name.length === 0 || name.length > 64 || isCreating}
      >
        {#if isCreating}
          <ProgressRing />
          <span>{loadingLabel}</span>
        {:else}
          <span>{buttonLabel}</span>
        {/if}
      </Button>
    </Tooltip>
    {#if identityNumber !== undefined}
      <p class="text-text-primary text-center text-xs">
        You are upgrading ID &nbsp;
        <Badge size="sm">{identityNumber}</Badge>
      </p>
    {/if}
  </div>
</form>
