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
    /** Session duration the request asked for (seconds, already clamped to the
     *  30-day cap); the initial selection, which the user can change. */
    requestedTtlSeconds: number;
    /** Called once the selected identity is authenticated, with the chosen
     *  session duration (seconds), to connect. */
    onAuthorize: (ttlSeconds: number) => void;
  }

  const { mcpServerHost, requestedTtlSeconds, onAuthorize }: Props = $props();

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

  const MINUTE = 60;
  const HOUR = 60 * MINUTE;
  const DAY = 24 * HOUR;
  const WEEK = 7 * DAY;
  const MONTH = 30 * DAY;
  // Session-duration presets (seconds). The backend caps a standing delegation
  // at 30 days, so that's the longest offered.
  const PRESETS = [HOUR, 8 * HOUR, DAY, WEEK, MONTH];

  // Compact label for an arbitrary duration: the request can ask for any number
  // of seconds (within the 30-day cap), so the selected value isn't always one
  // of the presets.
  const formatTtl = (seconds: number): string => {
    const parts: string[] = [];
    const d = Math.floor(seconds / DAY);
    const h = Math.floor((seconds % DAY) / HOUR);
    const m = Math.floor((seconds % HOUR) / MINUTE);
    const s = seconds % MINUTE;
    if (d > 0) parts.push(`${d}d`);
    if (h > 0) parts.push(`${h}h`);
    if (m > 0) parts.push(`${m}m`);
    if (s > 0) parts.push(`${s}s`);
    return parts.length > 0 ? parts.join(" ") : "0s";
  };

  const labelFor = (seconds: number): string => {
    switch (seconds) {
      case HOUR:
        return $t`1 hour`;
      case 8 * HOUR:
        return $t`8 hours`;
      case DAY:
        return $t`1 day`;
      case WEEK:
        return $t`1 week`;
      case MONTH:
        return $t`30 days`;
      default:
        return formatTtl(seconds);
    }
  };

  // Honor the exact requested duration (already clamped to the cap); the user can
  // still override it with one of the presets before connecting.
  let selectedTtlSeconds = $state(requestedTtlSeconds);
  const options = $derived(
    PRESETS.map((seconds) => ({
      value: seconds,
      label: labelFor(seconds),
      selected: seconds === selectedTtlSeconds,
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
      onAuthorize(selectedTtlSeconds);
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
        onChange={(value) => (selectedTtlSeconds = value)}
        align="end"
      >
        <button type="button" class="btn btn-secondary btn-sm gap-2">
          <span>{labelFor(selectedTtlSeconds)}</span>
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
