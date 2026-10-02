<script lang="ts">
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { fromCanisterMcpConfig } from "$lib/utils/mcpConfig";
  import { currentBrowserId } from "$lib/stores/browser-key.store";
  import { saidAlreadyInstalled } from "$lib/utils/notifications/alreadyInstalled";
  import CliAccessSection from "./components/CliAccessSection.svelte";
  import NotificationsSection from "./components/NotificationsSection.svelte";
  import McpTrustedServersSection from "./components/McpTrustedServersSection.svelte";
  import type { PageProps } from "./$types";

  const { data }: PageProps = $props();

  // The MCP config comes from `identity_info`, an update call, so what the
  // section renders — and what it writes back — rests on a certified value
  // rather than on the forgeable `mcp_get_config` query.
  const mcpConfig = $derived(
    fromCanisterMcpConfig(data.identityInfo.mcp_config),
  );

  // Which entry this browser is, and what the user told it about their own device.
  // Both are this browser's own storage rather than the identity's, so they are read
  // here and handed down: the section renders from `identity_info` like the others.
  const browsers = $derived(data.identityInfo.browsers[0] ?? []);
  let browserId = $state<number | undefined>(undefined);
  let saidInstalled = $state(false);
  $effect(() => {
    const identityNumber = $authenticatedStore.identityNumber;
    void currentBrowserId(identityNumber).then((id) => {
      browserId = id;
    });
    void saidAlreadyInstalled(identityNumber).then((said) => {
      saidInstalled = said;
    });
  });
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
  <NotificationsSection
    identityNumber={$authenticatedStore.identityNumber}
    {browsers}
    {browserId}
    saidAlreadyInstalled={saidInstalled}
  />
  <CliAccessSection identityNumber={$authenticatedStore.identityNumber} />
  <McpTrustedServersSection
    identityNumber={$authenticatedStore.identityNumber}
    {mcpConfig}
  />
</div>
