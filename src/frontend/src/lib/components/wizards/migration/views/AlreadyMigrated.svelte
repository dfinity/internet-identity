<script lang="ts">
  import MigrationIllustration from "$lib/components/illustrations/MigrationIllustration.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { SUPPORT_URL } from "$lib/config";
  import { onMount } from "svelte";
  import {
    upgradeIdentityFunnel,
    UpgradeIdentityEvents,
  } from "$lib/utils/analytics/upgradeIdentityFunnel";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  let {
    onUpgradeAgain,
  }: { onUpgradeAgain: (attachElement?: HTMLElement) => void } = $props();

  let attachElement = $state<HTMLElement>();

  onMount(() => {
    upgradeIdentityFunnel.trigger(UpgradeIdentityEvents.AlreadyMigratedScreen);
  });
</script>

<div class="flex flex-1 flex-col" bind:this={attachElement}>
  <div class="mb-8 flex flex-col">
    <div class="text-text-primary flex h-32 items-center justify-center py-5">
      <MigrationIllustration />
    </div>
    <div>
      <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
        {$t`Identity already upgraded`}
      </h1>
      <p
        class="text-text-tertiary mb-4 text-base font-medium text-balance sm:text-center"
      >
        <Trans>
          This identity has already been upgraded to the new experience.
        </Trans>
      </p>
      <p
        class="text-text-tertiary text-base font-medium text-balance sm:text-center"
      >
        <Trans>
          You can continue with
          <b class="text-text-primary whitespace-nowrap">
            Use existing Passkey
          </b> in the login process.
        </Trans>
      </p>
    </div>
  </div>
  <div class="flex flex-col items-stretch gap-4">
    <Button
      href={SUPPORT_URL}
      target="_blank"
      rel="noopener noreferrer"
      variant="secondary"
      size="lg"
    >
      {$t`Help & FAQ`}
    </Button>
    <Button
      onclick={() => onUpgradeAgain(attachElement)}
      variant="tertiary"
      size="lg"
    >
      {$t`Upgrade again`}
    </Button>
  </div>
</div>
