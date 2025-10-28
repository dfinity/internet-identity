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
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    upgrade: (name: string) => Promise<void | "cancelled">;
    identityNumber: bigint;
  }

  const { upgrade, identityNumber }: Props = $props();

  onMount(() => {
    upgradeIdentityFunnel.trigger(UpgradeIdentityEvents.CreatePasskeyScreen);
  });

  let inputRef = $state<HTMLInputElement>();
  let name = $state("");
  let isUpgrading = $state(false);
  let isCancelled = $state(false);

  const handleCreate = async () => {
    isUpgrading = true;
    const result = await upgrade(name.trim());
    isUpgrading = false;

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
        class="text-text-tertiary text-base font-medium text-balance sm:text-center"
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
      disabled={isUpgrading}
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
        disabled={name.length === 0 || name.length > 64 || isUpgrading}
      >
        {#if isUpgrading}
          <ProgressRing />
          <span>{$t`Upgrading identity...`}</span>
        {:else}
          <span>{$t`Upgrade identity`}</span>
        {/if}
      </Button>
    </Tooltip>
    <p class="text-text-secondary text-center text-xs">
      <Trans>
        You are upgrading ID
        <Badge size="sm" class="ms-1">{identityNumber}</Badge>
      </Trans>
    </p>
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
