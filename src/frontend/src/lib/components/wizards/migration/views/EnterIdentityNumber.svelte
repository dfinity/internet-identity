<script lang="ts">
  import MigrationIllustration from "$lib/components/illustrations/MigrationIllustration.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { UPGRADE_SUPPORT_URL } from "$lib/config";
  import { onMount } from "svelte";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    onSubmit: (
      identityNumber: bigint,
      attachElement?: HTMLElement,
    ) => Promise<void | "cancelled" | "wrongDomain">;
  }

  let { onSubmit }: Props = $props();

  let identityNumber = $state<string>("");
  let inputElement = $state<HTMLInputElement>();
  let attachElement = $state<HTMLElement>();
  let isSubmitting = $state(false);
  let isCancelled = $state(false);
  let isWrongDomain = $state(false);

  onMount(() => {
    inputElement?.focus();
  });

  const handleSubmit = async () => {
    isSubmitting = true;
    const result = await onSubmit(BigInt(identityNumber), attachElement);
    isSubmitting = false;

    if (result === "cancelled") {
      isCancelled = true;
      await waitFor(4000);
      isCancelled = false;
    }
    if (result === "wrongDomain") {
      isWrongDomain = true;
      await waitFor(4000);
      isWrongDomain = false;
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
        {$t`Let's get started`}
      </h1>
      <p
        class="text-text-tertiary text-base font-medium text-balance sm:text-center"
      >
        <Trans>
          Upgrade your existing identity to the new experience in a few steps.
        </Trans>
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
      placeholder={$t`Internet Identity number`}
      size="md"
      bind:element={inputElement}
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      aria-label={$t`Identity number`}
    />
    <Tooltip
      label={isWrongDomain
        ? $t`Wrong domain was set. Please try again.`
        : $t`Interaction canceled. Please try again.`}
      hidden={!isCancelled && !isWrongDomain}
      manual
    >
      <Button
        onclick={handleSubmit}
        variant="primary"
        size="lg"
        type="submit"
        disabled={isSubmitting || identityNumber.length === 0}
      >
        {#if isSubmitting}
          <ProgressRing />
          <span>{$t`Authenticating...`}</span>
        {:else}
          <span>{$t`Continue`}</span>
        {/if}
      </Button>
    </Tooltip>
    <Button
      href={UPGRADE_SUPPORT_URL}
      target="_blank"
      rel="noopener noreferrer"
      variant="tertiary"
      size="lg"
    >
      {$t`Help & FAQ`}
    </Button>
  </div>
</form>
