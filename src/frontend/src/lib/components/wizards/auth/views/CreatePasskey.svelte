<script lang="ts">
  import { fly } from "svelte/transition";
  import { onMount } from "svelte";
  import SmileyWritingIllustration from "$lib/components/illustrations/SmileyWritingIllustration.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    create: (name: string) => Promise<void>;
    identityNumber?: bigint;
  }

  const { create, identityNumber }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let name = $state("");
  let creating = $state(false);

  const handleCreate = async () => {
    creating = true;
    try {
      await create(name.trim());
    } finally {
      creating = false;
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
      disabled={creating}
      error={name.length > 64 ? "Maximum length is 64 characters." : undefined}
      aria-label="Identity name"
    />
    <Button
      onclick={handleCreate}
      variant="primary"
      size="lg"
      type="submit"
      disabled={name.length === 0 || name.length > 64 || creating}
    >
      {#if creating}
        <ProgressRing />
        <span>Creating Passkey...</span>
      {:else}
        <span>Create Passkey</span>
      {/if}
    </Button>
    {#if identityNumber !== undefined}
      <p class="text-text-primary text-center text-xs">
        You are upgrading ID
        <Badge class="ml-2" size="sm">{identityNumber}</Badge>
      </p>
    {/if}
  </div>
</form>
