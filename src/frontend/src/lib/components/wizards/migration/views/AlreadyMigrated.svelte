<script lang="ts">
  import MigrationIllustration from "$lib/components/illustrations/MigrationIllustration.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { SUPPORT_URL } from "$lib/config";
  import { onMount } from "svelte";
  import {
    upgradeIdentityFunnel,
    UpgradeIdentityEvents,
  } from "$lib/utils/analytics/upgradeIdentityFunnel";

  let { name }: { name: string } = $props();

  onMount(() => {
    upgradeIdentityFunnel.trigger(UpgradeIdentityEvents.AlreadyMigratedScreen);
  });
</script>

<div class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <div class="text-text-primary flex h-32 items-center justify-center py-5">
      <MigrationIllustration />
    </div>
    <div>
      <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
        Identity already upgraded
      </h1>
      <p
        class="text-md text-text-tertiary font-medium text-balance sm:text-center"
      >
        {`${name} is already upgraded to the new experience.`}
      </p>
    </div>
  </div>
  <div class="flex flex-col items-stretch gap-4">
    <Button
      href={SUPPORT_URL}
      target="_blank"
      rel="noopener noreferrer"
      variant="tertiary"
      size="lg"
    >
      Help & FAQ
    </Button>
  </div>
</div>
