<script lang="ts">
  import { onMount } from "svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { UserPlusIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    create: (name: string) => void;
  }

  const { create }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let name = $state("");
  let loading = $state(false);

  const handleSubmit = () => {
    loading = true;
    create(name);
  };

  onMount(() => {
    inputRef?.focus();
  });
</script>

<form class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <FeaturedIcon size="lg" class="mb-4 self-start">
      <UserPlusIcon size="1.5rem" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      Name additional account
    </h1>
    <p class="text-md text-text-tertiary mb-6 font-medium">
      You can rename this account later if needed.
    </p>
    <Input
      bind:element={inputRef}
      bind:value={name}
      inputmode="text"
      placeholder="Account name"
      hint={'Label it by how you\'ll use it — e.g., "Work", "Personal".'}
      type="text"
      size="md"
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      disabled={loading}
      error={name.length > 32 ? "Maximum length is 32 characters." : undefined}
      aria-label="Account name"
    />
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <Button
      onclick={handleSubmit}
      variant="primary"
      size="lg"
      type="submit"
      disabled={name.length === 0 || name.length > 32 || loading}
    >
      {#if loading}
        <ProgressRing />
        <span>Creating account...</span>
      {:else}
        <span>Create account</span>
      {/if}
    </Button>
  </div>
</form>
