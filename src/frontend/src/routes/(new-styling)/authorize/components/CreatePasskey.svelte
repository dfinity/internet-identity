<script lang="ts">
  import { fly } from "svelte/transition";
  import { type State } from "../state";
  import { onMount } from "svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { User } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  type Props = Extract<State, { state: "createPasskey" }>;

  const { create, cancel }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let name = $state("");
  let loading = $state(false);

  const handleSubmit = () => {
    loading = true;
    create(name);
  };

  onMount(() => {
    inputRef?.focus();
    // Make sure mobile keyboard does not overlap the input
    setTimeout(() => inputRef?.scrollIntoView(), 100);
  });
</script>

<form class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col" in:fly={{ duration: 200, x: 10 }}>
    <FeaturedIcon size="lg" class="mb-4 self-start">
      <User size="1.5rem" />
    </FeaturedIcon>
    <h1
      class="text-gray-light-900 dark:text-gray-dark-25 mb-3 text-2xl font-medium"
    >
      Name your identity
    </h1>
    <p
      class="text-md text-gray-light-600 dark:text-gray-dark-50 mb-6 font-medium"
    >
      To spot it faster later.
    </p>
    <Input
      bind:element={inputRef}
      bind:value={name}
      placeholder="Identity name"
      hint="This name is permanent, so make it meaningful."
      type="text"
      size="md"
      autocomplete="off"
      spellcheck="false"
      disabled={loading}
    />
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <Button
      onclick={handleSubmit}
      variant="primary"
      size="lg"
      type="submit"
      disabled={name.length === 0 || loading}
    >
      {#if loading}
        <ProgressRing />
        <span>Creating Passkey...</span>
      {:else}
        <span>Create Passkey</span>
      {/if}
    </Button>
  </div>
</form>
