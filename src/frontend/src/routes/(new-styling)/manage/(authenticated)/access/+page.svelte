<script lang="ts">
  import AccessMethods from "$lib/components/views/AccessMethods.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">{$t`Access methods`}</h1>
  <p class="text-text-tertiary text-base">
    <Trans>
      Overview of access methods (dates created, recent usage). Add a new method
      or remove them.
    </Trans>
  </p>
</header>

<AccessMethods />
