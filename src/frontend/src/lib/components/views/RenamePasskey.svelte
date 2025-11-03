<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import { PencilIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    name: string;
    onRename: (name: string) => Promise<void>;
    onCancel: () => void;
  }

  const props: Props = $props();

  let name = $state(props.name);
  let isSubmitting = $state(false);
  let inputRef = $state<HTMLInputElement>();

  const hasChanges = $derived(props.name !== name.trim());

  const handleSubmit = async () => {
    try {
      isSubmitting = true;
      await props.onRename(name.trim());
    } finally {
      isSubmitting = false;
    }
  };

  $effect(() => {
    inputRef?.focus();
  });
</script>

<form class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <FeaturedIcon size="lg" class="mb-4 self-start">
      <PencilIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Rename passkey`}
    </h1>
    <p class="text-text-tertiary mb-6 text-base font-medium">
      {$t`Give your passkey a memorable name to help you identify it.`}
    </p>
    <Input
      bind:element={inputRef}
      bind:value={name}
      inputmode="text"
      placeholder={$t`Passkey name`}
      type="text"
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      error={name.length > 32
        ? $t`Maximum length is 32 characters.`
        : undefined}
      disabled={isSubmitting}
      aria-label={$t`Passkey name`}
    />
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <Button
      type="submit"
      size="lg"
      onclick={handleSubmit}
      disabled={name.trim().length === 0 ||
        name.length > 32 ||
        !hasChanges ||
        isSubmitting}
    >
      {#if isSubmitting}
        <ProgressRing />
        <span>{$t`Saving changes...`}</span>
      {:else}
        <span>{$t`Save changes`}</span>
      {/if}
    </Button>
    <Button
      onclick={props.onCancel}
      variant="tertiary"
      size="lg"
      disabled={isSubmitting}
    >
      {$t`Cancel`}
    </Button>
  </div>
</form>
