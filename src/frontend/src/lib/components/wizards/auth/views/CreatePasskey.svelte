<script lang="ts">
  import { fly } from "svelte/transition";
  import { onMount } from "svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { UserIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    create: (name: string) => Promise<void>;
  }

  const { create }: Props = $props();

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

<form class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col" in:fly={{ duration: 200, x: 10 }}>
    <FeaturedIcon size="lg" class="mb-4 self-start">
      <UserIcon size="1.5rem" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      Name your identity
    </h1>
    <p class="text-md text-text-tertiary mb-6 font-medium">
      This will label your passkey, and you can’t rename it later once set.
    </p>
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
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
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
  </div>
</form>
