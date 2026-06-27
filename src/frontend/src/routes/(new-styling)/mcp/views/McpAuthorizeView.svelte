<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import McpHero from "../components/McpHero.svelte";
  import Select from "$lib/components/ui/Select.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { ChevronDownIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { sessionStore } from "$lib/stores/session.store";
  import { handleError } from "$lib/components/utils/error";

  interface Props {
    /** Hostname of the MCP server (display, e.g. mcp.id.ai). */
    mcpServerHost: string;
    /** Session duration the request asked for (minutes); used as the initial
     *  selection when it matches a preset, otherwise we default to 1 hour. */
    requestedTtlMinutes: number;
    /** Called once the selected identity is authenticated, with the chosen
     *  session duration, to connect. */
    onAuthorize: (ttlMinutes: number) => void;
  }

  const { mcpServerHost, requestedTtlMinutes, onAuthorize }: Props = $props();

  // Connecting authorizes this agent for the user's identity — no account is
  // chosen here (accounts are app-specific; the MCP server is the connector, not
  // an app). The identity switcher (shown by the layout) is how the user picks
  // which identity, and it only *selects* (it doesn't sign in). So "Allow access"
  // authenticates the selected identity unless it's already the live session.
  const authLastUsedFlow = new AuthLastUsedFlow();
  const selectedIdentityNumber = $derived(
    $lastUsedIdentitiesStore.selected?.identityNumber,
  );
  $effect(() => {
    if (selectedIdentityNumber !== undefined) {
      authLastUsedFlow.init([selectedIdentityNumber]);
    }
  });
  let isAuthorizing = $state(false);

  const HOUR = 60;
  const DAY = 24 * HOUR;
  // Session-duration presets (minutes). The backend caps a standing delegation
  // at 30 days, so that's the longest offered.
  const PRESETS = [HOUR, 8 * HOUR, DAY, 7 * DAY, 30 * DAY];

  const labelFor = (minutes: number): string => {
    switch (minutes) {
      case HOUR:
        return $t`1 hour`;
      case 8 * HOUR:
        return $t`8 hours`;
      case DAY:
        return $t`1 day`;
      case 7 * DAY:
        return $t`1 week`;
      case 30 * DAY:
        return $t`30 days`;
      default:
        return $t`1 hour`;
    }
  };

  // Start from the requested duration when it's one of the presets; otherwise
  // default to 1 hour. The user can change it before connecting.
  let selectedTtlMinutes = $state(
    PRESETS.includes(requestedTtlMinutes) ? requestedTtlMinutes : HOUR,
  );
  const options = $derived(
    PRESETS.map((minutes) => ({
      value: minutes,
      label: labelFor(minutes),
      selected: minutes === selectedTtlMinutes,
    })),
  );

  const handleAllowAccess = async (): Promise<void> => {
    const selected = $lastUsedIdentitiesStore.selected;
    if (selected === undefined) {
      return;
    }
    isAuthorizing = true;
    try {
      // Authenticate unless the live session is already the selected identity.
      // The switcher only selects, so a session for a *different* identity must
      // not be reused — that would connect as the wrong anchor.
      if ($authenticationStore?.identityNumber !== selected.identityNumber) {
        sessionStore.reset();
        await authLastUsedFlow.authenticate(selected);
      }
      onAuthorize(selectedTtlMinutes);
    } catch (error) {
      handleError(error);
    } finally {
      isAuthorizing = false;
    }
  };
</script>

<!--
  The MCP connect screen is a consent step: it authorizes the agent for the
  user's identity and lets the user choose the session duration. There is no
  account picker — accounts are per-app and the MCP server origin is just the
  connector; the server selects an app account per call later.
-->
<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <McpHero mcpServer={mcpServerHost} />
    <h1 class="text-text-primary mt-2 text-2xl font-medium">
      {$t`Connect ${mcpServerHost}`}
    </h1>
    <p class="text-text-tertiary mt-1 text-base text-pretty">
      {$t`Let ${mcpServerHost} act on your behalf across your apps. You'll need to reconnect when this access expires.`}
    </p>
    <div
      class="border-border-tertiary mt-4 mb-6 flex flex-row items-center justify-between gap-3 border-t pt-4"
    >
      <span class="text-text-secondary text-sm font-medium">
        {$t`Access expires after`}
      </span>
      <Select
        {options}
        onChange={(value) => (selectedTtlMinutes = value)}
        align="end"
      >
        <button type="button" class="btn btn-secondary btn-sm gap-2">
          <span>{labelFor(selectedTtlMinutes)}</span>
          <ChevronDownIcon class="size-4" />
        </button>
      </Select>
    </div>
    <button
      class="btn btn-primary w-full gap-2"
      onclick={handleAllowAccess}
      disabled={isAuthorizing || selectedIdentityNumber === undefined}
    >
      {#if isAuthorizing}
        <ProgressRing class="size-5" />
      {/if}
      <span>{$t`Allow access`}</span>
    </button>
  </AuthPanel>
</div>
