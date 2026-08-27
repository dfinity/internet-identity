<script lang="ts">
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { fromCanisterMcpConfig } from "$lib/utils/mcpConfig";
  import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
  import CliAccessSection from "./components/CliAccessSection.svelte";
  import McpTrustedServersSection from "./components/McpTrustedServersSection.svelte";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import { BellIcon, ChevronRightIcon } from "@lucide/svelte";
  import type { PageProps } from "./$types";

  const { data }: PageProps = $props();

  // The MCP config comes from `identity_info`, an update call, so what the
  // section renders — and what it writes back — rests on a certified value
  // rather than on the forgeable `mcp_get_config` query.
  const mcpConfig = $derived(
    fromCanisterMcpConfig(data.identityInfo.mcp_config),
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
  {#if $PUSH_NOTIFICATIONS}
    <ButtonCard href="/manage/notifications" class="group !p-4 sm:!p-5">
      <span
        class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
        aria-hidden="true"
      >
        <BellIcon class="size-5" />
      </span>
      <span class="flex min-w-0 flex-1 flex-col gap-1 text-start">
        <span class="text-text-primary text-base font-semibold">
          {$t`Notifications`}
        </span>
        <span class="text-text-tertiary text-sm font-normal">
          {$t`Manage which apps can send push notifications to this device.`}
        </span>
      </span>
      <ChevronRightIcon class="text-fg-tertiary size-5 shrink-0" />
    </ButtonCard>
  {/if}
</div>
