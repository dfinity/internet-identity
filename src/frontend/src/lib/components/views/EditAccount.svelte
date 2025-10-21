<script lang="ts">
  import { onMount } from "svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { PencilIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import { t } from "$lib/stores/locale.store";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { isNullish } from "@dfinity/utils";

  interface Account {
    name: string;
    isDefaultSignIn: boolean;
  }

  interface Props {
    account?: Account;
    existingNames: string[];
    save: (account: Account) => void;
  }

  const { account, existingNames, save }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let name = $state(account?.name ?? "");
  let isDefaultSignIn = $state(account?.isDefaultSignIn ?? false);
  let isSubmitting = $state(false);
  let nameExists = $state(false);

  const showSetDefault = $derived(
    isNullish(account) || !account.isDefaultSignIn,
  );
  const hasChanges = $derived(
    isNullish(account) ||
      account.name !== name.trim() ||
      account.isDefaultSignIn !== isDefaultSignIn,
  );

  const handleSubmit = () => {
    isSubmitting = true;
    if (existingNames.includes(name.trim())) {
      nameExists = true;
      isSubmitting = false;
      return;
    }
    save({
      name: name.trim(),
      isDefaultSignIn,
    });
  };

  onMount(() => {
    inputRef?.focus();
  });
</script>

<form class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <FeaturedIcon size="lg" class="mb-4 self-start">
      <PencilIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {isNullish(account) ? $t`Name account` : $t`Edit account`}
    </h1>
    <p class="text-md text-text-tertiary mb-6 font-medium">
      {isNullish(account)
        ? $t`You can edit this account later. Label it by use (e.g. 'Work' or 'Demo').`
        : $t`Rename or make this your default sign-in`}
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
        : nameExists
          ? $t`In use on another account.`
          : undefined}
      disabled={isSubmitting}
      aria-label={$t`Account name`}
    />
    {#if showSetDefault}
      <div class="border-border-tertiary my-6 border-t"></div>
      <Checkbox
        bind:checked={isDefaultSignIn}
        label={$t`Set as default sign-in`}
        disabled={isSubmitting}
      />
    {/if}
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <Button
      onclick={handleSubmit}
      variant="primary"
      size="lg"
      type="submit"
      disabled={name.length === 0 ||
        name.length > 32 ||
        !hasChanges ||
        nameExists ||
        isSubmitting}
    >
      {#if isSubmitting}
        <ProgressRing />
        <span>
          {isNullish(account) ? $t`Creating account...` : $t`Saving changes...`}
        </span>
      {:else}
        <span>
          {isNullish(account) ? $t`Create account` : $t`Save changes`}
        </span>
      {/if}
    </Button>
  </div>
</form>
