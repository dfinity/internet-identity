<script lang="ts">
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import McpHero from "../components/McpHero.svelte";
  import SessionDurationSelect from "$lib/components/ui/SessionDurationSelect.svelte";
  import { MAX_SESSION_DURATION_SECONDS } from "$lib/utils/sessionDuration";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import AccessLevelSelector from "$lib/components/ui/AccessLevelSelector.svelte";
  import type { AccessLevel } from "$lib/utils/accessLevel";
  import { accessLevelStore } from "$lib/stores/access-level.store";
  import { InfoIcon } from "@lucide/svelte";
  import { Trans } from "$lib/components/locale";
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

  // The MCP connect flow always offers the access-level choice (ungated by
  // READ_ONLY_MODE): the choice is recorded with the grant and applies to the
  // per-app delegations the server later obtains, while its standing delegation
  // stays full access. The chosen level maps to the per-app delegations'
  // `permissions`: "read-only" = queries-only, "full-access" = update-capable.
  // Derived per-anchor (the browser may be shared) from the selected anchor's
  // stored MCP choice: unselected on a first-time connect so the user picks
  // explicitly (the "Allow access" button stays disabled until then), and
  // re-hydrated when they switch identity. The user's own radio pick overrides
  // this until the identity changes again.
  let accessLevel: AccessLevel | undefined = $derived(
    selectedIdentityNumber === undefined
      ? undefined
      : accessLevelStore.getPreference("mcp", selectedIdentityNumber),
  );
  let isAuthorizing = $state(false);

  // Honor the exact requested duration (already clamped to the cap); the user can
  // still override it — up to the 30-day maximum — before connecting.
  let selectedTtlSeconds = $state(requestedTtlSeconds);

  const handleAllowAccess = async (): Promise<void> => {
    const selected = $lastUsedIdentitiesStore.selected;
    // Capture the chosen level in a const so its non-undefined type survives the
    // `await` below (a reactive `let` would widen back to `AccessLevel | undefined`).
    const chosenAccessLevel = accessLevel;
    if (selected === undefined || chosenAccessLevel === undefined) {
      return;
    }
    // Remember this anchor's choice so it pre-fills its next MCP connect.
    accessLevelStore.setPreference(
      "mcp",
      selected.identityNumber,
      chosenAccessLevel,
    );
    isAuthorizing = true;
    try {
      // Authenticate unless the live session is already the selected identity.
      // The switcher only selects, so a session for a *different* identity must
      // not be reused — that would connect as the wrong anchor.
      if ($authenticationStore?.identityNumber !== selected.identityNumber) {
        sessionStore.reset();
        await authLastUsedFlow.authenticate(selected);
      }
      onAuthorize(selectedTtlSeconds, chosenAccessLevel);
    } catch (error) {
      handleError(error);
    } finally {
      isAuthorizing = false;
    }
  };
</script>

<!--
  The MCP connect screen is a consent step: it authorizes the agent for the
  user's identity and lets the user choose the session duration and access
  level. There is no account picker — accounts are per-app and the MCP server
  origin is just the connector; the server selects an app account per call later.
-->
<div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
  <AuthPanel>
    <McpHero mcpServer={mcpServerHost} />
    <h1 class="text-text-primary mt-2 text-2xl font-medium">
      {$t`Connect ${mcpServerHost}`}
    </h1>
    <p class="text-text-tertiary mt-1 text-base text-pretty">
      {$t`Acts on your behalf across your apps.`}
    </p>

    <!-- Session: how long the connection lasts before the user must reconnect. -->
    <div class="border-border-tertiary mt-4 mb-6 flex flex-col border-t pt-4">
      <span class="text-text-primary mb-0.5 text-base font-medium">
        {$t`Session`}
      </span>
      <div class="flex flex-row items-center justify-between gap-2">
        <span class="text-text-tertiary text-base">
          {$t`Time until you have to reconnect:`}
        </span>
        <!-- The connect flow lets the user pick any duration up to the 30-day
             maximum, defaulting to the requested value. -->
        <SessionDurationSelect
          maxSeconds={MAX_SESSION_DURATION_SECONDS}
          bind:value={selectedTtlSeconds}
          disabled={isAuthorizing}
        />
      </div>
    </div>

    <AccessLevelSelector
      bind:accessLevel
      disabled={isAuthorizing}
      class="mb-6"
    />

    <!-- Fine print: the connection can be revoked from Settings at any time.
         Opens in a new tab so the connect request in this one isn't lost. -->
    <div
      class="border-border-tertiary text-text-tertiary mb-6 flex items-start gap-2 border-t pt-5 text-sm"
    >
      <InfoIcon class="mt-0.5 size-4 shrink-0" />
      <span>
        <Trans>
          Revoke access anytime in your <a
            href="/manage/settings"
            target="_blank"
            rel="noopener noreferrer"
            class="text-text-primary font-medium underline-offset-2 hover:underline"
            >settings</a
          >.
        </Trans>
      </span>
    </div>

    <button
      class="btn btn-primary btn-xl w-full"
      onclick={handleAllowAccess}
      disabled={isAuthorizing ||
        selectedIdentityNumber === undefined ||
        accessLevel === undefined}
    >
      {#if isAuthorizing}
        <ProgressRing class="size-5" />
      {/if}
      <span>{$t`Allow access`}</span>
    </button>
  </AuthPanel>
</div>
