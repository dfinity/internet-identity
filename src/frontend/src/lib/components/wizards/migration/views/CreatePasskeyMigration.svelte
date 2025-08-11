<script lang="ts">
  import SmileyWritingIllustration from "$lib/components/illustrations/SmileyWritingIllustration.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { nonNullish } from "@dfinity/utils";
  import { onMount } from "svelte";

  interface Props {
    onSubmit: (passkeyName: string) => void;
    isAuthenticating?: boolean;
    identityNumber: bigint;
  }

  let { onSubmit, isAuthenticating, identityNumber }: Props = $props();

  let passkeyName = $state<string | undefined>(undefined);
  let inputElement = $state<HTMLInputElement>();

  onMount(() => {
    inputElement?.focus();
  });

  const handleSubmit = async () => {
    // Button is disabled if identityNumber is null or undefined so no need to manage that case.
    if (nonNullish(passkeyName)) {
      onSubmit(passkeyName);
    }
  };
</script>

<form class="flex flex-1 flex-col">
  <div class="text-text-primary mb-8 flex flex-col items-center justify-center">
    <SmileyWritingIllustration class="my-5 h-32" />
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
      bind:value={passkeyName}
      inputmode="text"
      placeholder="Identity name"
      hint="Pick something recognizable, like your name."
      type="text"
      size="md"
      bind:element={inputElement}
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      aria-label="Identity name"
    />
    <Button
      onclick={handleSubmit}
      variant="primary"
      size="lg"
      type="submit"
      disabled={isAuthenticating}
    >
      {#if isAuthenticating}
        <ProgressRing />
        <span>Creating Passkey...</span>
      {:else}
        <span>Create Passkey</span>
      {/if}
    </Button>
    <p class="text-text-primary flex items-center justify-center gap-2 text-xs">
      <span>You are upgrading ID</span>
      <Badge size="sm">{identityNumber}</Badge>
    </p>
  </div>
</form>
