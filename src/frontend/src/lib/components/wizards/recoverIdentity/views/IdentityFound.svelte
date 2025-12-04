<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { UserCheckIcon, UserIcon } from "@lucide/svelte";
  import { formatDate, t } from "$lib/stores/locale.store";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import { Trans } from "$lib/components/locale";
  import { onMount } from "svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { nonNullish } from "@dfinity/utils";
  import type { IdentityInfo } from "$lib/generated/internet_identity_types";

  type Props = {
    identityNumber: bigint;
    identityInfo: IdentityInfo;
    onContinue: (newName?: string) => Promise<void>;
    onCancel: () => void;
  };

  const { identityNumber, info, onContinue, onCancel }: Props = $props();

  let continueInProgress = $state(false);

  const hasName = $derived(nonNullish(identityName));
  let inputRef = $state<HTMLInputElement>();
  let name = $state(identityName ?? "");
  const MAX_NAME_LENGTH = 64;
  const nameTooLong = $derived(name.length > MAX_NAME_LENGTH);

  const canSubmit = $derived(
    !continueInProgress && name.length > 0 && !nameTooLong,
  );

  const displayIdentity = $derived(identityName ?? identityNumber.toString());

  onMount(() => {
    inputRef?.focus();
  });

  const handleContinue = async () => {
    if (continueInProgress) {
      return;
    }
    if (!canSubmit) {
      return;
    }
    continueInProgress = true;
    try {
      await onContinue(name.trim());
    } finally {
      continueInProgress = false;
    }
  };
</script>

<form class="flex flex-col gap-8" onsubmit={handleContinue}>
  <div class="mt-6 flex flex-col gap-4">
    <FeaturedIcon size="md" variant="success">
      <UserCheckIcon class="size-6" />
    </FeaturedIcon>
    <div class="flex flex-col gap-3">
      <h2 class="text-text-primary text-xl">
        {$t`We have found your identity`}
      </h2>
      {#if hasName}
        <p class="text-text-tertiary text-md">
          {$t`Here is the identity that is linked to your recovery phrase. Continue to access it and add another access method if required.`}
        </p>
      {:else}
        <p class="text-text-primary text-md font-bold">
          {identityNumber.toString()}
        </p>
      {/if}
    </div>
  </div>
  {#if hasName}
    <ButtonCard onclick={handleContinue}>
      <Avatar size="sm">
        <UserIcon class="size-5" />
      </Avatar>
      <div class="flex flex-col text-left text-sm">
        <div class="text-text-primary font-semibold">
          {displayIdentity}
        </div>
        <div class="text-text-tertiary font-normal" aria-hidden="true">
          {#if nonNullish(createdAt)}
            <span
              >{$t`Created ${$formatDate(createdAt, { dateStyle: "short" })}`}</span
            >
          {/if}
        </div>
      </div>
    </ButtonCard>
  {:else}
    <div class="flex flex-col gap-2">
      <p class="text-text-tertiary text-md">
        <Trans>
          Name your identity to upgrade to II 2.0 and gain access. You can't
          rename it later once set.
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
        error={nameTooLong ? $t`Maximum length is 64 characters.` : undefined}
        aria-label={$t`Identity name`}
      >
        {#snippet hint()}
          <Trans>Pick something recognizable, like your name</Trans>
        {/snippet}
      </Input>
    </div>
  {/if}
  <div class="align-stretch flex flex-col gap-2">
    <Button size="lg" type="submit" disabled={!canSubmit}>
      {#if hasName}
        {$t`Continue`}
      {:else}
        {$t`Update and Continue`}
      {/if}
    </Button>
    <Button size="lg" variant="tertiary" onclick={onCancel}>
      {$t`Cancel`}
    </Button>
  </div>
</form>
