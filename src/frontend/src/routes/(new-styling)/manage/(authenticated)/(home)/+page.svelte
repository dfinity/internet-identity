<script lang="ts">
  import IdentityInfo from "./components/IdentityInfo.svelte";
  import type { PageProps } from "./$types";
  import AccessMethods from "./components/AccessMethods.svelte";
  import { toAccessMethods } from "../access/utils";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Panel from "./components/Panel.svelte";

  const { data }: PageProps = $props();

  const name = $derived(
    data.identityInfo.name[0] ?? data.identityNumber.toString(),
  );
  const totalAccessMethods = $derived(
    toAccessMethods(data.identityInfo).length,
  );
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">
    {$t`Welcome, ${name}!`}
  </h1>
  <p class="text-text-tertiary text-base">
    <Trans>Your identity and sign-in methods at a glance.</Trans>
  </p>
</header>

<div
  class="mt-10 grid grid-cols-[repeat(auto-fill,minmax(min(100%,24rem),1fr))] gap-6"
>
  <Panel>
    <IdentityInfo {name} />
  </Panel>
  <Panel>
    <AccessMethods {totalAccessMethods} />
  </Panel>
</div>
