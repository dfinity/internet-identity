<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import McpHero from "../components/McpHero.svelte";
  import Select from "$lib/components/ui/Select.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import AccessLevelToggle from "$lib/components/ui/AccessLevelToggle.svelte";
  import type { AccessLevel } from "$lib/utils/accessLevel";
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
    /** Session duration the request asked for (seconds, already clamped to
     *  [10 min, 30 days]); the initial selection, which the user can change. */
    requestedTtlSeconds: number;
    /** Called once the selected identity is authenticated, with the chosen
     *  session duration (seconds) and the access level the per-app delegations
     *  the server obtains should grant, to connect. */
    onAuthorize: (ttlSeconds: number, accessLevel: AccessLevel) => void;
  }

  const { mcpServerHost, requestedTtlSeconds, onAuthorize }: Props = $props();

  // MCP connections default to read-only (opt-out): the server can read on the
  // user's behalf across their apps, but its per-app delegations are query-only
  // unless the user opts into full access by unchecking.
  let accessLevel: AccessLevel = $state("read-only");

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
  // The session durations the picker offers, each with its label, spanning the
  // allowed range: 10 minutes (the shortest, also the request's floor) to 30
  // days (the longest, the backend's grant cap). `$derived` so the labels
  // re-translate when the locale changes.
  const presets = $derived([
    { value: 10 * MINUTE, label: $t`10 minutes` },
    { value: HOUR, label: $t`1 hour` },
    { value: 8 * HOUR, label: $t`8 hours` },
    { value: DAY, label: $t`1 day` },
    { value: WEEK, label: $t`1 week` },
    { value: 30 * DAY, label: $t`30 days` },
  ]);

  // Compact label for an off-preset duration: the request can ask for any number
  // of seconds within [10 min, 30 days], so the selected value isn't always one
  // of the presets (e.g. a 2-hour request shows "2h").
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

  // Honor the exact requested duration (already clamped to the cap); the user can
  // still override it with one of the presets before connecting.
  let selectedTtlSeconds = $state(requestedTtlSeconds);
  const options = $derived(
    presets.map((preset) => ({
      ...preset,
      selected: preset.value === selectedTtlSeconds,
    })),
  );
  // The trigger shows the selected preset's label, or the compact format when the
  // selection is an off-preset (honoured) duration.
  const selectedLabel = $derived(
    presets.find((preset) => preset.value === selectedTtlSeconds)?.label ??
      formatTtl(selectedTtlSeconds),
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
      onAuthorize(selectedTtlSeconds, accessLevel);
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
        <!-- Disabled while connecting: a disabled button dispatches no click, so
             the Select can't open once the user has committed to "Allow access". -->
        <button
          type="button"
          class="btn btn-secondary btn-sm gap-2"
          disabled={isAuthorizing}
        >
          <span>{selectedLabel}</span>
          <ChevronDownIcon class="size-4" />
        </button>
      </Select>
    </div>
    <AccessLevelToggle
      bind:accessLevel
      prompt="read-only"
      disabled={isAuthorizing}
      class="mb-6"
    />
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
