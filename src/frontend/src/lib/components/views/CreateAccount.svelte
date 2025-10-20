<script lang="ts">
  import { onMount } from "svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { PlusIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import { t } from "$lib/stores/locale.store";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    namesInUse: string[];
    create: (name: string, isDefaultSignIn: boolean) => void;
  }

  const { namesInUse, create }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let name = $state("");
  let isDefaultSignIn = $state(false);
  let isSubmitting = $state(false);
  let isNameInUse = $state(false);

  const handleSubmit = () => {
    isSubmitting = true;
    if (namesInUse.includes(name.trim())) {
      isNameInUse = true;
      isSubmitting = false;
      return;
    }
    create(name.trim(), isDefaultSignIn);
  };

  onMount(() => {
    inputRef?.focus();
  });
</script>

<form class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <FeaturedIcon size="lg" class="mb-4 self-start">
      <PlusIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Name account`}
    </h1>
    <p class="text-md text-text-tertiary mb-6 font-medium">
      {$t`You can edit this account later. Label it by use (e.g. 'Work' or 'Demo').`}
    </p>
    <Input
      bind:element={inputRef}
      bind:value={name}
      inputmode="text"
      placeholder={$t`Account name`}
      type="text"
      size="md"
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      error={name.length > 32
        ? $t`Maximum length is 32 characters.`
        : isNameInUse
          ? $t`In use on another account.`
          : undefined}
      disabled={isSubmitting}
      aria-label={$t`Account name`}
    />
    <div class="border-border-tertiary my-6 border-t"></div>
    <Checkbox
      bind:checked={isDefaultSignIn}
      label={$t`Set as default sign-in`}
      disabled={isSubmitting}
    />
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <Button
      onclick={handleSubmit}
      variant="primary"
      size="lg"
      type="submit"
      disabled={name.length === 0 ||
        name.length > 32 ||
        isNameInUse ||
        isSubmitting}
    >
      {#if isSubmitting}
        <ProgressRing />
        <span>{$t`Creating account...`}</span>
      {:else}
        <span>{$t`Create account`}</span>
      {/if}
    </Button>
  </div>
</form>
