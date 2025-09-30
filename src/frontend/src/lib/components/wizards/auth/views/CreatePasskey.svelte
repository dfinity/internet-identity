<script lang="ts">
  import { fly } from "svelte/transition";
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
  import { AUTH_FLOW_UPDATES } from "$lib/state/featureFlags";
  import { HelpCircleIcon } from "@lucide/svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { II_SUPPORT_PASSKEY_URL } from "$lib/config";

  interface Props {
    create: (name: string) => Promise<void | "cancelled">;
    buttonLabel?: string;
    loadingLabel?: string;
    identityNumber?: bigint;
  }

  const {
    create,
    buttonLabel = "Create Passkey",
    loadingLabel = "Creating Passkey...",
    identityNumber,
  }: Props = $props();

  onMount(() => {
    upgradeIdentityFunnel.trigger(UpgradeIdentityEvents.CreatePasskeyScreen);
  });

  let popoverAnchorRef = $state<HTMLDivElement>();
  let inputRef = $state<HTMLInputElement>();
  let name = $state("");
  let isCreating = $state(false);
  let isCancelled = $state(false);
  let showPrivacyPopover = $state(false);

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

<form class="flex flex-1 flex-col" in:fly={{ duration: 200, x: 10 }}>
  <div
    class="text-text-primary mb-8 flex w-full flex-col items-center justify-center"
  >
    <SmileyWritingIllustration class="my-5 h-22" />
    <div>
      <h1 class="mb-3 text-2xl font-medium sm:text-center">
        Name your identity
      </h1>
      <p
        class="text-md text-text-tertiary font-medium text-balance sm:text-center"
      >
        {#if $AUTH_FLOW_UPDATES}
          Internet Identity <b>does not</b> store your biometric data. It stays on
          your device. Your Identity functions as a secure passkey manager for authentication.
        {:else}
          This will label your passkey, and you can't rename it later once set
        {/if}
      </p>
    </div>
  </div>
  <div class="flex flex-col items-stretch gap-4">
    <Input
      bind:element={inputRef}
      bind:value={name}
      inputmode="text"
      placeholder="Identity name"
      hint={$AUTH_FLOW_UPDATES
        ? "You cannot rename this once it is set."
        : "Pick something recognizable, like your name."}
      type="text"
      size="md"
      autocomplete="off"
      autocorrect="off"
      spellcheck="false"
      disabled={isCreating}
      error={name.length > 64 ? "Maximum length is 64 characters." : undefined}
      aria-label="Identity name"
    />
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
          <span>{loadingLabel}</span>
        {:else}
          <span>{buttonLabel}</span>
        {/if}
      </Button>
    </Tooltip>
    {#if identityNumber !== undefined}
      <p class="text-text-primary text-center text-xs">
        You are upgrading ID &nbsp;
        <Badge size="sm">{identityNumber}</Badge>
      </p>
    {/if}

    {#if $AUTH_FLOW_UPDATES}
      <div class="flex flex-row items-center justify-between gap-4">
        <p class="text-text-secondary text-sm">What is a passkey?</p>
        <div bind:this={popoverAnchorRef}>
          <Button
            variant="tertiary"
            onclick={() => (showPrivacyPopover = !showPrivacyPopover)}
          >
            <HelpCircleIcon
              size="20"
              class="text-text-primary stroke-fg-tertiary"
            />
          </Button>
        </div>
      </div>
      {#if showPrivacyPopover}
        <Popover
          anchor={popoverAnchorRef}
          direction="up"
          align="end"
          distance="0.5rem"
          class="gap-0.5"
          onClose={() => (showPrivacyPopover = false)}
        >
          <p class="text-text-primary text-xs font-semibold">
            What is a Passkey
          </p>
          <p class="text-text-secondary text-xs font-medium">
            Passkeys are a secure, password-free way to log in. They use
            cryptographic keys stored safely on your device, letting you sign in
            to dapps with Face ID, Touch ID, or a security key - no personal
            data is ever shared.
          </p>
          <a
            href={II_SUPPORT_PASSKEY_URL}
            target="_blank"
            class="text-text-tertiary mt-2.5 self-end text-xs font-bold underline"
          >
            Learn More
          </a>
        </Popover>
      {/if}
    {/if}
  </div>
</form>
