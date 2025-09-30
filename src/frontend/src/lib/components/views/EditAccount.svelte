<script lang="ts">
  import { onMount } from "svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { UserPlusIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { AUTH_FLOW_UPDATES } from "$lib/state/featureFlags";
  import Checkbox from "../ui/Checkbox.svelte";
  import { type AccountInfo } from "$lib/generated/internet_identity_types";

  interface Props {
    handleEdit: (
      account: AccountInfo,
      name: string,
      defaultAccount?: boolean,
    ) => void;
    account: AccountInfo;
    defaultAccount: AccountInfo;
  }

  const { handleEdit, account, defaultAccount }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let name = $state<string>(account.name[0] ?? "");
  let loading = $state(false);
  let isDefault = $state<boolean>(
    defaultAccount.account_number[0] === account.account_number[0],
  );
  let defaultIsDisabled =
    defaultAccount.account_number[0] === account.account_number[0];

  const handleSubmit = () => {
    loading = true;
    handleEdit(account, name, defaultIsDisabled ? undefined : isDefault);
  };

  onMount(() => {
    inputRef?.focus();
  });
</script>

<form class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <FeaturedIcon size="lg" class="mb-4 self-start">
      <UserPlusIcon size="1.5rem" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {#if $AUTH_FLOW_UPDATES}
        Name account
      {:else}
        Name additional account
      {/if}
    </h1>
    <p class="text-md text-text-tertiary mb-6 font-medium">
      {#if $AUTH_FLOW_UPDATES}
        You can edit this account later. Label it by use (e.g. 'Work' or
        'Demo').
      {:else}
        You can rename this account later if needed.
      {/if}
    </p>
    <Input
      bind:element={inputRef}
      bind:value={name}
      inputmode="text"
      placeholder="Account name"
      hint={$AUTH_FLOW_UPDATES
        ? ""
        : 'Label it by how you\'ll use it — e.g., "Work", "Personal".'}
      type="text"
      size="md"
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      disabled={loading}
      error={name.length > 32 ? "Maximum length is 32 characters." : undefined}
      aria-label="Account name"
    />
    {#if $AUTH_FLOW_UPDATES}
      <div class="mt-4.5 flex flex-col gap-6">
        <div class="border-border-tertiary border-t"></div>
        <div class="flex flex-row items-center gap-4">
          <label
            class={`${defaultIsDisabled ? "cursor-not-allowed" : "cursor-pointer"} flex items-center gap-2`}
          >
            <Checkbox
              checked={isDefault}
              disabled={defaultIsDisabled}
              onchange={(e) => (isDefault = e.currentTarget.checked)}
            />
            <span class="text-text-secondary text-sm font-medium">
              Set as default sign-in
            </span>
          </label>
        </div>
      </div>
    {/if}
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <Button
      onclick={handleSubmit}
      variant="primary"
      size="lg"
      type="submit"
      disabled={name.length === 0 || name.length > 32 || loading}
    >
      {#if loading}
        <ProgressRing />
      {/if}
      <span>Save changes</span>
    </Button>
  </div>
</form>
