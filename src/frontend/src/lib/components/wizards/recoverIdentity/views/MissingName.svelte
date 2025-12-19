<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { UserCheckIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import Input from "$lib/components/ui/Input.svelte";
  import { Trans } from "$lib/components/locale";
  import { onMount } from "svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  const MAX_NAME_LENGTH = 64;

  type Props = {
    identityNumber: bigint;
    onContinue: (newName?: string) => Promise<void>;
    onCancel: () => void;
  };

  const { identityNumber, onContinue, onCancel }: Props = $props();

  let continueInProgress = $state(false);
  let inputRef = $state<HTMLInputElement>();
  let name = $state("");

  const nameTooLong = $derived(name.length > MAX_NAME_LENGTH);

  const handleContinue = async () => {
    try {
      continueInProgress = true;
      await onContinue(name.trim());
    } finally {
      continueInProgress = false;
    }
  };

  onMount(() => {
    inputRef?.focus();
  });
</script>

<form class="flex flex-col">
  <FeaturedIcon variant="success" size="lg" class="mb-4">
    <UserCheckIcon class="size-6" />
  </FeaturedIcon>
  <h2 class="text-text-primary mb-3 text-2xl font-medium">
    {$t`Identity found`}
  </h2>
  <p class="text-text-primary mb-6 text-base font-bold">
    {identityNumber.toString()}
  </p>
  <p class="text-text-tertiary mb-4 text-base font-medium">
    <Trans>
      Enter a name for your identity to complete the upgrade. Once set, it
      cannot be changed later.
    </Trans>
  </p>
  <Input
    bind:element={inputRef}
    bind:value={name}
    inputmode="text"
    placeholder={$t`Identity name`}
    type="text"
    size="md"
    autocomplete="off"
    autocorrect="off"
    spellcheck="false"
    disabled={continueInProgress}
    error={name.length > MAX_NAME_LENGTH
      ? $t`Maximum length is 64 characters.`
      : undefined}
    aria-label={$t`Identity name`}
    class="mb-8"
  >
    {#snippet hint()}
      <Trans>Pick something recognizable, like your name</Trans>
    {/snippet}
  </Input>
  <button
    onclick={handleContinue}
    type="submit"
    disabled={name.trim().length === 0 || nameTooLong || continueInProgress}
    class="btn btn-lg mb-3"
  >
    {#if continueInProgress}
      <ProgressRing />
    {/if}
    <span>{$t`Continue`}</span>
  </button>
  <button
    onclick={onCancel}
    disabled={continueInProgress}
    class="btn btn-tertiary btn-lg"
  >
    {$t`Cancel`}
  </button>
</form>
