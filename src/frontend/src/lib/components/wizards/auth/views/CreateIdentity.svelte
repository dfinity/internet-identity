<script lang="ts">
  import { onMount } from "svelte";
  import SmileyWritingIllustration from "$lib/components/illustrations/SmileyWritingIllustration.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    create: (name: string) => Promise<void>;
  }

  const { create }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let name = $state("");
  let isCreating = $state(false);

  const handleCreate = async () => {
    isCreating = true;
    await create(name.trim());
    isCreating = false;
  };

  onMount(() => {
    inputRef?.focus();
  });
</script>

<form class="flex flex-1 flex-col">
  <div
    class="text-text-primary mb-8 flex w-full flex-col items-center justify-center"
  >
    <div class="illustration self-center">
      <SmileyWritingIllustration class="my-5 h-22" />
    </div>
    <div>
      <h1 class="mb-3 text-2xl font-medium sm:text-center">
        {$t`What's your name?`}
      </h1>
      <p
        class="text-md text-text-tertiary font-medium text-balance sm:text-center"
      >
        <Trans>
          Give your identity a clear, simple, and memorable name you'll easily
          recognize.
        </Trans>
      </p>
    </div>
  </div>
  <div class="flex flex-col items-stretch gap-6">
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
      disabled={isCreating}
      error={name.length > 64
        ? $t`Maximum length is 64 characters.`
        : undefined}
      aria-label={$t`Identity name`}
    >
      {#snippet hint()}
        <Trans>You <b>cannot</b> rename this once it is set.</Trans>
      {/snippet}
    </Input>
    <Button
      onclick={handleCreate}
      variant="primary"
      size="lg"
      type="submit"
      disabled={name.length === 0 || name.length > 64 || isCreating}
    >
      {#if isCreating}
        <ProgressRing />
        <span>{$t`Creating identity...`}</span>
      {:else}
        <span>{$t`Create identity`}</span>
      {/if}
    </Button>
  </div>
</form>

<style>
  @media (max-height: 700px) {
    /*noinspection CssUnusedSymbol*/
    .illustration {
      display: none !important;
    }
  }
</style>
