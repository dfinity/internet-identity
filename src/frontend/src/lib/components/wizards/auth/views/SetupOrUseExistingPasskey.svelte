<script lang="ts">
  import { fly } from "svelte/transition";
  import Button from "$lib/components/ui/Button.svelte";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import {
    AUTH_FLOW_UPDATES,
    CONTINUE_FROM_ANOTHER_DEVICE,
  } from "$lib/state/featureFlags";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { HelpCircleIcon } from "@lucide/svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import { II_SUPPORT_PRIVACY_SECURITY } from "$lib/config";

  interface Props {
    setupNew: () => void;
    useExisting: () => Promise<void | "cancelled">;
    continueFromAnotherDevice: () => void;
  }

  const { setupNew, useExisting, continueFromAnotherDevice }: Props = $props();

  let popoverAnchorRef = $state<HTMLDivElement>();
  let isAuthenticating = $state(false);
  let isCancelled = $state(false);
  let showPrivacyPopover = $state(false);

  const handleUseExisting = async () => {
    isAuthenticating = true;
    const result = await useExisting();
    isAuthenticating = false;

    if (result === "cancelled") {
      isCancelled = true;
      await waitFor(4000);
      isCancelled = false;
    }
  };
</script>

<div class="mt-4 mb-8 flex flex-col" in:fly={{ duration: 200, x: -10 }}>
  <PasskeyIllustration class="text-text-primary mb-8 h-32" />
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    Simplify your sign-in
  </h1>

  <p class="text-md text-text-tertiary font-medium text-balance sm:text-center">
    {#if $AUTH_FLOW_UPDATES}
      Create an identity with a passkey, using biometrics or a security key.
      Your data never leaves your device.
    {:else}
      With passkeys, you can now use your fingerprint, face, or screen lock to
      quickly and securely confirm it’s really you.
    {/if}
  </p>
</div>
<div class="flex flex-col gap-3">
  <Button onclick={setupNew} size="lg" disabled={isAuthenticating}>
    {#if $AUTH_FLOW_UPDATES}
      Create new identity
    {:else}
      Set up a new Passkey
    {/if}
  </Button>
  <Tooltip
    label="Interaction canceled. Please try again."
    hidden={!isCancelled}
    manual
  >
    <Button
      onclick={handleUseExisting}
      variant="secondary"
      size="lg"
      disabled={isAuthenticating}
    >
      {#if isAuthenticating}
        <ProgressRing />
        <span>Authenticating...</span>
      {:else}
        <span>
          {#if $AUTH_FLOW_UPDATES}
            Use existing identity
          {:else}
            Use an existing Passkey
          {/if}
        </span>
      {/if}
    </Button>
  </Tooltip>
  {#if $CONTINUE_FROM_ANOTHER_DEVICE}
    <Button
      onclick={continueFromAnotherDevice}
      variant="tertiary"
      disabled={isAuthenticating}
      size="lg"
    >
      Continue from another device
    </Button>
  {/if}
  {#if $AUTH_FLOW_UPDATES}
    <div class="flex flex-row items-center justify-between gap-4">
      <p class="text-text-secondary text-sm">
        Learn about privacy preservation
      </p>
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
        distance="10px"
        class="gap-0.5"
        onClose={() => (showPrivacyPopover = false)}
      >
        <p class="text-text-primary text-xs font-semibold">
          Internet Identity protects your privacy
        </p>
        <p class="text-text-secondary text-xs font-medium">
          Internet Identity never shares your personal data, such as email or
          name, with apps or websites. Instead, it creates a unique pseudonym
          for each app, so you can't be tracked across apps. All of this happens
          automatically in the background. You don't need to manage anything.
        </p>
        <a
          href={II_SUPPORT_PRIVACY_SECURITY}
          target="_blank"
          class="text-text-tertiary mt-2.5 self-end text-xs font-bold underline"
        >
          Learn More
        </a>
      </Popover>
    {/if}
  {/if}
</div>
