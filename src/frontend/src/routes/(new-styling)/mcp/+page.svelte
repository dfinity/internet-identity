<script lang="ts">
  import type { PageProps } from "./$types";
  import {
    isAuthenticatedStore,
    authenticationStore,
  } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import { t } from "$lib/stores/locale.store";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { remapToLegacyDomain } from "$lib/utils/iiConnection";
  import { parseMcpServerUrl } from "$lib/utils/mcpServer";
  import { readMcpConfig, isOriginTrusted } from "$lib/utils/mcpConfig";
  import { get } from "svelte/store";
  import { onMount } from "svelte";
  import McpHero from "./components/McpHero.svelte";
  import McpAuthorizeView from "./views/McpAuthorizeView.svelte";
  import McpCloseWindowView from "./views/McpCloseWindowView.svelte";
  import McpErrorView from "./views/McpErrorView.svelte";
  import McpInvalidView from "./views/McpInvalidView.svelte";
  import McpUntrustedView from "./views/McpUntrustedView.svelte";
  import McpConnectingView from "./views/McpConnectingView.svelte";
  import ManageHandoff from "$lib/components/ui/ManageHandoff.svelte";
  import { ManageHandoffFlow } from "$lib/flows/manageHandoffFlow.svelte";
  import { mcpAuthorize } from "./utils";
  import { showIdentitySwitcher } from "./mcp-switcher.store";
  import {
    mcpAuthorizeFunnel,
    McpAuthorizeEvents,
  } from "$lib/utils/analytics/mcpAuthorizeFunnel";

  const { data }: PageProps = $props();
  const params = $derived(data.params);
  const status = $derived(data.status);

  // The MCP server the user is connecting is identified by the origin of the
  // request's callback: each user trusts whichever (remote) server they connect.
  // The standing delegation is delivered there via a top-level form-POST, so we
  // only accept callbacks the /mcp `form-action` CSP allows — MCP is remote-only,
  // so any https origin (a plain-http or loopback callback is rejected). A
  // disallowed (or unparsable) callback yields `undefined` → the invalid screen,
  // rather than a silent CSP block at submit time.
  const mcpServer = $derived(
    params.kind === "valid" ? parseMcpServerUrl(params.callback) : undefined,
  );

  // Origin used for canister account calls: remap a gateway origin
  // (*.icp0.io / *.icp.net) to *.ic0.app so the derived principal matches the
  // one /authorize derives for that origin.
  const effectiveOrigin = $derived(
    mcpServer !== undefined ? remapToLegacyDomain(mcpServer.origin) : undefined,
  );

  const requestValid = $derived(
    params.kind === "valid" && mcpServer !== undefined,
  );

  onMount(() => {
    // A redirect back from the MCP server carries an outcome `status`; a fresh
    // entry carries the request itself.
    if (status === "success") {
      mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.Success);
    } else if (status === "error") {
      mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.Error);
    } else {
      mcpAuthorizeFunnel.init();
      if (!requestValid) {
        mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.RequestInvalid);
        mcpAuthorizeFunnel.close();
      } else {
        mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.RequestReceived);
      }
    }

    // Drop the URL fragment once parsed so the public_key, callback, and status
    // don't sit in the address bar after the user lands here.
    if (window.location.hash !== "") {
      window.history.replaceState(
        null,
        "",
        window.location.pathname + window.location.search,
      );
    }
  });

  type Phase =
    | { kind: "wizard" }
    | { kind: "authorize" }
    | { kind: "untrusted" }
    | { kind: "connecting" }
    | { kind: "close" }
    | { kind: "invalid" }
    | { kind: "error" };

  // The phase the page opens on. Outcomes the MCP server redirects back with
  // take priority; otherwise a returning user with a previously-used identity
  // opens on the connect screen, and a user with no last-used identity starts in
  // the sign-in method wizard. Whether the server is actually trusted is the
  // identity's synced (on-chain) config, which can only be read once
  // authenticated — so we show the connect screen optimistically and verify it
  // against the canister at connect time (`handleAuthorize`), moving to the
  // untrusted screen if it isn't.
  const initialPhase = (): Phase => {
    if (status === "success") {
      return { kind: "close" };
    }
    if (status === "error") {
      return { kind: "error" };
    }
    if (!requestValid) {
      return { kind: "invalid" };
    }
    const selected = get(lastUsedIdentitiesStore).selected;
    if (selected === undefined) {
      return { kind: "wizard" };
    }
    return { kind: "authorize" };
  };

  let phase = $state<Phase>(initialPhase());

  // The identity the current live phase was last derived for. A change means the
  // user switched identity, so we re-open the connect screen optimistically
  // (re-verifying at connect for the new identity).
  let phaseIdentity = $state<bigint | undefined>(
    get(lastUsedIdentitiesStore).selected?.identityNumber,
  );

  // The switcher is meaningful while the user is choosing/confirming an identity
  // — including on the untrusted screen, where switching to an identity that
  // does trust this server moves straight to the connect screen.
  $effect(() => {
    showIdentitySwitcher.set(
      phase.kind === "wizard" ||
        phase.kind === "authorize" ||
        phase.kind === "untrusted",
    );
  });

  // Manage the live sign-in phases (wizard → connect). Terminal and
  // redirect-outcome phases (close, error, invalid) are owned by the initial
  // outcome and never re-evaluated here. Switching identity only *selects* (it
  // doesn't authenticate), so we leave the wizard only once the chosen identity
  // has actually authenticated — and only once `selected` is populated, since
  // sign-up authenticates and *then* selects, and the reused picker reads
  // `selected` at mount. Once an identity is selected we show the connect screen
  // optimistically; trust is verified against the canister at connect time. A
  // change of selected identity re-opens the connect screen for the new one (so
  // an untrusted result for the previous identity doesn't stick).
  $effect(() => {
    if (
      phase.kind !== "wizard" &&
      phase.kind !== "authorize" &&
      phase.kind !== "untrusted"
    ) {
      return;
    }
    const selected = $lastUsedIdentitiesStore.selected;
    if (selected === undefined) {
      phaseIdentity = undefined;
      if (phase.kind !== "wizard") {
        phase = { kind: "wizard" };
      }
      return;
    }
    if (phase.kind === "wizard" && !$isAuthenticatedStore) {
      return;
    }
    if (selected.identityNumber !== phaseIdentity) {
      phaseIdentity = selected.identityNumber;
      phase = { kind: "authorize" };
    }
  });

  // Invoked by the reused account picker once it has authenticated the selected
  // identity and resolved the chosen account. Connecting performs the opt-in
  // (`mcp_set_access`) and delivers the standing delegation to the MCP server.
  const handleAuthorize = (
    accountNumberPromise: Promise<bigint | undefined>,
    ttlMinutes: number,
  ): void => {
    const server = mcpServer;
    if (params.kind !== "valid" || server === undefined) {
      return;
    }
    const request = params;
    mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.Confirmed);
    // Show a loading screen while we verify trust and (if trusted) prepare and
    // form-POST the standing delegation: the picker's own button spinner stops
    // once it hands off here, and the verify + `mcpAuthorize` calls run several
    // canister calls before navigating away.
    phase = { kind: "connecting" };
    void (async () => {
      try {
        const authenticated = get(authenticationStore);
        if (authenticated === undefined) {
          phase = { kind: "authorize" };
          return;
        }
        // The identity's synced trusted-server config is the source of truth:
        // connect only when this identity has MCP enabled and trusts this
        // server's origin. Verifying here (post-authentication) means the result
        // is the same on every device, regardless of any local state.
        const config = await readMcpConfig(
          authenticated.actor,
          authenticated.identityNumber,
        );
        if (!isOriginTrusted(config, server.origin)) {
          mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.ServerUntrusted);
          phase = { kind: "untrusted" };
          return;
        }
        const accountNumber = await accountNumberPromise;
        // On success `mcpAuthorize` navigates the browser to the MCP server,
        // which redirects back here with a status — so a resolved promise means
        // the chain was built and submitted, not that we stay on this page.
        await mcpAuthorize({
          authenticated,
          publicKey: request.publicKey,
          mcpServerOrigin: server.origin,
          accountNumber,
          ttlMinutes,
          callback: request.callback,
          state: request.state,
        });
      } catch (error) {
        // Return to the connect screen so the user can retry.
        phase = { kind: "authorize" };
        handleError(error);
      }
    })();
  };

  // The untrusted screen sends the user to Settings to set this server as their
  // trusted one. Rather than open Settings cold (which would force a fresh
  // sign-in in the new tab), authenticate the selected identity here and hand
  // the session to the opened tab — same handoff as the header's "Manage
  // identity". The returning-user untrusted screen often isn't authenticated
  // yet, so this runs the full ceremony when needed.
  const manageHandoff = new ManageHandoffFlow();
  const handleManageTrustedServer = (): void => {
    const selected = $lastUsedIdentitiesStore.selected;
    if (selected === undefined) {
      return;
    }
    void (async () => {
      try {
        await manageHandoff.start("/manage/settings", selected);
      } catch (error) {
        handleError(error);
      }
    })();
  };

  const wizardSignInHandlers = {
    onSignIn: (identityNumber: bigint): Promise<void> => {
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
      return Promise.resolve();
    },
    onSignUp: (identityNumber: bigint): Promise<void> => {
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
      toaster.success({
        title: $t`You're all set. Your identity has been created.`,
        duration: 4000,
      });
      return Promise.resolve();
    },
    onError: handleError,
  };
</script>

{#if phase.kind === "invalid"}
  <McpInvalidView />
{:else if phase.kind === "error"}
  <McpErrorView />
{:else if phase.kind === "wizard" && mcpServer !== undefined}
  <div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
    <AuthPanel>
      <McpHero mcpServer={mcpServer.host} />
      <AuthWizard {...wizardSignInHandlers}>
        <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
          {$t`Choose method`}
        </h1>
        <p class="text-text-secondary mb-6 self-start text-sm">
          {$t`to connect ${mcpServer.host}`}
        </p>
      </AuthWizard>
    </AuthPanel>
  </div>
{:else if phase.kind === "authorize" && mcpServer !== undefined && effectiveOrigin !== undefined && $lastUsedIdentitiesStore.selected !== undefined}
  <McpAuthorizeView
    mcpServerHost={mcpServer.host}
    {effectiveOrigin}
    displayOrigin={mcpServer.origin}
    requestedTtlMinutes={params.kind === "valid" ? params.ttlMinutes : 60}
    onAuthorize={handleAuthorize}
  />
{:else if phase.kind === "untrusted" && mcpServer !== undefined}
  <McpUntrustedView
    mcpServerHost={mcpServer.host}
    onManageTrustedServer={handleManageTrustedServer}
    busy={manageHandoff.isAuthenticating}
  />
{:else if phase.kind === "connecting" && mcpServer !== undefined}
  <McpConnectingView mcpServer={mcpServer.host} />
{:else if phase.kind === "close"}
  <McpCloseWindowView />
{/if}

<ManageHandoff
  flow={manageHandoff}
  description={$t`Open Settings in a new tab to set your trusted MCP server.`}
  buttonLabel={$t`Open Settings`}
/>
