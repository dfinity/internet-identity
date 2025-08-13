<script lang="ts">
  import MigrationIllustration from "$lib/components/illustrations/MigrationIllustration.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { SUPPORT_URL } from "$lib/config";
  import { onMount } from "svelte";
  import { waitFor } from "$lib/utils/utils";
  import { WrongDomainError } from "$lib/flows/migrationFlow.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { isNullish } from "@dfinity/utils";

  interface Props {
    onSubmit: (
      identityNumber: bigint,
      attachElement?: HTMLElement,
    ) => Promise<void>;
  }

  let { onSubmit }: Props = $props();

  let identityNumber = $state<string>("");
  let inputElement = $state<HTMLInputElement>();
  let attachElement = $state<HTMLElement>();
  let isAuthenticating = $state(false);
  let authenticationError = $state<"cancelled" | "domain">();

  onMount(() => {
    inputElement?.focus();
  });

  const handleSubmit = async () => {
    try {
      isAuthenticating = true;
      await onSubmit(BigInt(identityNumber), attachElement);
    } catch (error) {
      isAuthenticating = false;
      authenticationError =
        error instanceof WrongDomainError ? "domain" : "cancelled";
      await waitFor(1000);
      authenticationError = undefined;
    }
  };
</script>

<form class="flex flex-1 flex-col" bind:this={attachElement}>
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
      bind:value={
        () => identityNumber ?? "",
        (value) => (identityNumber = value.replace(/\D/g, ""))
      }
      inputmode="numeric"
      placeholder="Internet Identity number"
      size="md"
      bind:element={inputElement}
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      aria-label="Identity number"
    />
    <Tooltip
      label={authenticationError === "domain"
        ? "Wrong domain was set. Please try again."
        : "Interaction canceled. Please try again."}
      hidden={isNullish(authenticationError)}
      manual
    >
      <Button
        onclick={handleSubmit}
        variant="primary"
        size="lg"
        type="submit"
        disabled={isAuthenticating || identityNumber.length === 0}
      >
        {#if isAuthenticating}
          <ProgressRing />
          <span>Authenticating...</span>
        {:else}
          <span>Continue</span>
        {/if}
      </Button>
    </Tooltip>
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
