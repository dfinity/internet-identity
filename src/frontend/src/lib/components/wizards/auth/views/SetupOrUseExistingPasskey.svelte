<script lang="ts">
  import { fly } from "svelte/transition";
  import Button from "$lib/components/ui/Button.svelte";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { HelpCircleIcon } from "@lucide/svelte";
  import { II_SUPPORT_PRIVACY_SECURITY } from "$lib/config";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    setupNew: () => void;
    useExisting: () => Promise<void | "cancelled">;
    upgrade: () => void;
  }

  const { setupNew, useExisting, upgrade }: Props = $props();

  let isAuthenticating = $state(false);
  let isCancelled = $state(false);

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
    {$t`Simplify your sign-in`}
  </h1>

  <p
    class="text-text-tertiary text-base font-medium text-balance sm:text-center"
  >
    <Trans>
      Create an identity with a passkey, using biometrics or a security key.
      Your data never leaves your device.
    </Trans>
  </p>
</div>
<div class="flex flex-col gap-3">
  <Button onclick={setupNew} size="lg" disabled={isAuthenticating}>
    {$t`Create new identity`}
  </Button>
  <Tooltip
    label={$t`Interaction canceled. Please try again.`}
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
        <span>{$t`Authenticating...`}</span>
      {:else}
        <span>{$t`Use existing identity`}</span>
      {/if}
    </Button>
  </Tooltip>
  <div class="border-border-tertiary my-5 border-t"></div>
  <div class="flex flex-row items-center justify-between gap-4">
    <p class="text-text-secondary text-sm">
      {$t`Still have an identity number?`}
    </p>
    <button
      onclick={upgrade}
      class="text-text-primary text-sm font-semibold outline-0 hover:underline focus-visible:underline"
    >
      {$t`Upgrade`}
    </button>
  </div>
</div>
