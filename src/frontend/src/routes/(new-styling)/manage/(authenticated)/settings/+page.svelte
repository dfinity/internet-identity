<script lang="ts">
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import CliAccessSection from "./CliAccessSection.svelte";
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">
    {$t`Settings`}
  </h1>
  <p class="text-text-tertiary text-base">
    <Trans>Manage how other tools connect to your identity.</Trans>
  </p>
</header>

<div class="mt-10 flex max-w-3xl flex-col gap-5">
  <CliAccessSection identityNumber={$authenticatedStore.identityNumber} />
</div>
