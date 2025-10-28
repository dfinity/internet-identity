<script lang="ts">
  import SwitchDeviceIllustration from "$lib/components/illustrations/SwitchDeviceIllustration.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
</script>

<div class="flex flex-col">
  <SwitchDeviceIllustration class="text-text-primary mt-4 mb-8 h-32" />
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    {$t`Continue on your new device`}
  </h1>
  <p class="text-text-tertiary text-base font-medium sm:text-center">
    <Trans>
      To finish setting up your passkey, follow the instructions shown on your
      <b class="text-text-primary">new device</b>.
    </Trans>
  </p>
</div>
