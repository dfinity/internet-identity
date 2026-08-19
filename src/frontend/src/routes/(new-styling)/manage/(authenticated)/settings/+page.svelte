<script lang="ts">
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { fromCanisterMcpConfig } from "$lib/utils/mcpConfig";
  import CliAccessSection from "./components/CliAccessSection.svelte";
  import McpTrustedServersSection from "./components/McpTrustedServersSection.svelte";
  import SessionDevicesSection from "./components/SessionDevicesSection.svelte";
  import { fromCanisterSessionDevices } from "./sessionDevices";
  import { currentDeviceId } from "$lib/stores/browser-key.store";
  import type { PageProps } from "./$types";

  const { data }: PageProps = $props();

  // The MCP config comes from `identity_info`, an update call, so what the
  // section renders — and what it writes back — rests on a certified value
  // rather than on the forgeable `mcp_get_config` query.
  const mcpConfig = $derived(
    fromCanisterMcpConfig(data.identityInfo.mcp_config),
  );

  // Read from this browser's own key record rather than from the canister, which has no
  // way to tell which browser is asking: `identity_info` is signed by an access method.
  let thisBrowser = $state<number | undefined>(undefined);
  $effect(() => {
    void currentDeviceId($authenticatedStore.identityNumber).then(
      (id) => (thisBrowser = id),
    );
  });

  const sessionDevices = $derived(
    fromCanisterSessionDevices(data.identityInfo.session_devices, thisBrowser),
  );
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
  <McpTrustedServersSection
    identityNumber={$authenticatedStore.identityNumber}
    {mcpConfig}
  />
  <SessionDevicesSection
    identityNumber={$authenticatedStore.identityNumber}
    devices={sessionDevices}
  />
</div>
