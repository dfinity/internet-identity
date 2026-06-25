<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import ContinueView from "../../authorize/views/ContinueView.svelte";
  import McpHero from "../components/McpHero.svelte";
  import Select from "$lib/components/ui/Select.svelte";
  import { ChevronDownIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Hostname of the MCP server (display, e.g. mcp.id.ai). */
    mcpServerHost: string;
    /** Origin used for canister account calls (gateway origins remapped). */
    effectiveOrigin: string;
    /** The MCP server origin, shown to the user and used for the app lookup. */
    displayOrigin: string;
    /** Session duration the request asked for (minutes); used as the initial
     *  selection when it matches a preset, otherwise we default to 1 hour. */
    requestedTtlMinutes: number;
    /** Receives the chosen account (resolved after sign-in) and the chosen
     *  session duration, then connects. */
    onAuthorize: (
      accountNumber: Promise<bigint | undefined>,
      ttlMinutes: number,
    ) => void;
  }

  const {
    mcpServerHost,
    effectiveOrigin,
    displayOrigin,
    requestedTtlMinutes,
    onAuthorize,
  }: Props = $props();

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
</script>

<!--
  The MCP connect screen reuses the regular continue-with-ii account picker
  (multi-account toggle, add/edit account, account list) so the user binds a
  specific, stable account — the same principal that enable and disable derive.
  The MCP-specific consent (which server, what it can do, and for how long, with
  a user-chosen session duration) replaces the picker's app header via `header`.
-->
<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <ContinueView
      {effectiveOrigin}
      {displayOrigin}
      onAuthorize={(account) => onAuthorize(account, selectedTtlMinutes)}
      continueLabel={$t`Allow access`}
    >
      {#snippet header()}
        <McpHero mcpServer={mcpServerHost} />
        <h1 class="text-text-primary mt-2 text-2xl font-medium">
          {$t`Connect ${mcpServerHost}`}
        </h1>
        <p class="text-text-tertiary mt-1 text-base text-pretty">
          {$t`Let ${mcpServerHost} act as you across your apps. You'll need to reconnect when this access expires.`}
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
      {/snippet}
    </ContinueView>
  </AuthPanel>
</div>
