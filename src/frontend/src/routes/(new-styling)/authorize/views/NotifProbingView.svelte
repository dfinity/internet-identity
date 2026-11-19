<script lang="ts">
  import { BellRingIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
</script>

<div class="flex min-w-0 flex-col items-stretch" aria-busy="true">
  <FeaturedIcon size="lg" class="mb-4 self-start">
    <BellRingIcon class="size-6" aria-hidden="true" />
  </FeaturedIcon>
  <h1 class="text-text-primary mb-3 text-2xl font-medium">
    {$t`Checking notifications`}
  </h1>
  <p class="text-text-tertiary text-base">
    <Trans>Making sure they still reach this device.</Trans>
  </p>
  <div
    role="status"
    class="text-text-tertiary mt-7 flex items-center justify-center gap-2 text-sm"
  >
    <ProgressRing class="size-4" />
    <span><Trans>This takes a few seconds</Trans></span>
  </div>
</div>
