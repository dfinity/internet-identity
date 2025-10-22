<script lang="ts">
  import { onMount } from "svelte";
  import SmileyWritingIllustration from "$lib/components/illustrations/SmileyWritingIllustration.svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import Input from "$lib/components/ui/Input.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import {
    upgradeIdentityFunnel,
    UpgradeIdentityEvents,
  } from "$lib/utils/analytics/upgradeIdentityFunnel";
  import { HelpCircleIcon } from "@lucide/svelte";
  import { II_SUPPORT_PASSKEY_URL } from "$lib/config";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    create: (name: string) => Promise<void | "cancelled">;
  }

  const { create }: Props = $props();

  onMount(() => {
    upgradeIdentityFunnel.trigger(UpgradeIdentityEvents.CreatePasskeyScreen);
  });

  let inputRef = $state<HTMLInputElement>();
  let name = $state("");
  let isCreating = $state(false);
  let isCancelled = $state(false);

  const handleCreate = async () => {
    isCreating = true;
    const result = await create(name.trim());
    isCreating = false;

    if (result === "cancelled") {
      isCancelled = true;
      await waitFor(4000);
      isCancelled = false;
    }
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
        {$t`Name your identity`}
      </h1>
      <p
        class="text-md text-text-tertiary font-medium text-balance sm:text-center"
      >
        <Trans>
          Internet Identity <b>does not</b> store your biometric data. It stays on
          your device. Your identity acts as a secure passkey manager.
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
    <Tooltip
      label="Interaction canceled. Please try again."
      hidden={!isCancelled}
      manual
    >
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
    </Tooltip>
    <div class="flex flex-row items-center">
      <p class="text-text-secondary text-sm">
        <Trans>
          <a
            href={II_SUPPORT_PASSKEY_URL}
            target="_blank"
            class="text-text-primary font-semibold hover:underline"
          >
            Learn more
          </a> about passkeys
        </Trans>
      </p>
      <Tooltip
        label={$t`What are passkeys?`}
        description={$t`Passkeys use cryptographic keys stored on your device for secure, password-free sign-ins with Face ID, Touch ID, or a security key. Your data is never shared.`}
        direction="up"
        align="end"
        offset="0rem"
        class="max-w-80"
      >
        <Button
          variant="tertiary"
          iconOnly
          class="ms-auto !cursor-default !rounded-full"
        >
          <HelpCircleIcon class="size-5" />
        </Button>
      </Tooltip>
    </div>
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
