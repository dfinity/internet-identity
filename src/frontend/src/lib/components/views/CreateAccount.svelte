<script lang="ts">
  import { onMount } from "svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { UserPlusIcon } from "@lucide/svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { AUTH_FLOW_UPDATES } from "$lib/state/featureFlags";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";

  interface Props {
    create: (name: string, isDefault: boolean) => void;
  }

  const { create }: Props = $props();

  let inputRef = $state<HTMLInputElement>();
  let name = $state("");
  let loading = $state(false);
  let isDefault = $state<boolean>(false);

  const handleSubmit = () => {
    loading = true;
    create(name, isDefault);
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
          <label class="flex cursor-pointer items-center gap-2">
            <Checkbox
              checked={isDefault}
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
        <span>Creating account...</span>
      {:else}
        <span>Create account</span>
      {/if}
    </Button>
  </div>
</form>
