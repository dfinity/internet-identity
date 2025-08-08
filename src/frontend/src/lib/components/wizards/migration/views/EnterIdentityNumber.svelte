<script lang="ts">
  import MigrationIllustration from "$lib/components/illustrations/MigrationIllustration.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { SUPPORT_URL } from "$lib/config";
  import { nonNullish } from "@dfinity/utils";

  interface Props {
    onSubmit: (identityNumber: bigint) => void;
    isAuthenticating?: boolean;
  }

  let { onSubmit, isAuthenticating }: Props = $props();

  let identityNumber = $state<string | undefined>(undefined);

  const handleKeyDown = (event: KeyboardEvent) => {
    // Allow numeric keys (0-9)
    if (event.key >= "0" && event.key <= "9") {
      return;
    }

    // Allow essential navigation and editing keys
    const allowedKeys = [
      "Backspace",
      "Delete",
      "Tab",
      "ArrowLeft",
      "ArrowRight",
      "ArrowUp",
      "ArrowDown",
      "Home",
      "End",
      "Enter",
    ];

    if (allowedKeys.includes(event.key)) {
      return;
    }

    // Allow copy/paste/select all shortcuts
    if (event.ctrlKey || event.metaKey) {
      const shortcuts = ["a", "c", "v", "x", "z"];
      if (shortcuts.includes(event.key.toLowerCase())) {
        return;
      }
    }

    // Prevent all other keys
    event.preventDefault();
  };

  const handleSubmit = async () => {
    // Button is disabled if identityNumber is null or undefined so no need to manage that case.
    if (nonNullish(identityNumber)) {
      onSubmit(BigInt(identityNumber));
    }
  };
</script>

<form class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <div class="text-text-primary flex h-32 items-center justify-center py-5">
      <MigrationIllustration />
    </div>
    <div>
      <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
        Let's get started
      </h1>
      <p
        class="text-md text-text-tertiary font-medium text-balance sm:text-center"
      >
        Upgrade your existing identity to the new experience in a few steps.
      </p>
    </div>
  </div>
  <div class="flex flex-col items-stretch gap-4">
    <Input
      bind:value={identityNumber}
      onkeydown={handleKeyDown}
      inputmode="numeric"
      placeholder="Internet Identity number"
      size="md"
      autofocus
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      aria-label="Identity number"
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
        <span>Authenticating...</span>
      {:else}
        <span>Continue</span>
      {/if}
    </Button>
    <Button
      href={SUPPORT_URL}
      target="_blank"
      rel="noopener noreferrer"
      variant="tertiary"
      size="lg"
    >
      Help & FAQ
    </Button>
  </div>
</form>
