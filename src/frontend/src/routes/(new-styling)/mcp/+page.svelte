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
  import { mcpTrustedServersStore } from "$lib/stores/mcp-trusted-servers.store";
  import { get } from "svelte/store";
  import { onMount } from "svelte";
  import McpHero from "./components/McpHero.svelte";
  import McpAuthorizeView from "./views/McpAuthorizeView.svelte";
  import McpCloseWindowView from "./views/McpCloseWindowView.svelte";
  import McpErrorView from "./views/McpErrorView.svelte";
  import McpInvalidView from "./views/McpInvalidView.svelte";
  import McpUntrustedView from "./views/McpUntrustedView.svelte";
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
  // request's callback: each user trusts whichever server they connect (a public
  // https server, or one they run locally on http://127.0.0.1). The standing
  // delegation is delivered there via a top-level form-POST, so we only accept
  // callbacks the /mcp `form-action` CSP allows — any https origin, or http
  // loopback on 127.0.0.1 (not `localhost`, which can resolve off-loopback).
  // A disallowed (or unparsable) callback yields `undefined` → the invalid
  // screen, rather than a silent CSP block at submit time.
  const mcpServer = $derived(
    params.kind === "valid" ? parseMcpServerUrl(params.callback) : undefined,
  );

  // The chosen identity must have added this server to its trusted list (via
  // Settings) before we connect it. The actual authority is the backend binding
  // `mcp_set_access` creates on connect; this device-local allowlist is the
  // user-controlled pre-gate that decides which servers reach that step.
  const isServerTrusted = (identityNumber: bigint): boolean =>
    mcpServer !== undefined &&
    mcpTrustedServersStore.isTrusted(identityNumber, mcpServer.origin);

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
        // A returning user whose selected identity hasn't trusted this server
        // opens straight on the untrusted screen, so record that here (the
        // post-sign-in path records it from the phase effect instead).
        if (phase.kind === "untrusted") {
          mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.ServerUntrusted);
        }
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
    | { kind: "close" }
    | { kind: "invalid" }
    | { kind: "error" };

  // The phase the page opens on. Outcomes the MCP server redirects back with
  // take priority; otherwise a returning user with a previously-used identity
  // opens on the connect screen (or the untrusted screen if that identity
  // hasn't trusted this server), and a user with no last-used identity starts
  // in the sign-in method wizard.
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
    return isServerTrusted(selected.identityNumber)
      ? { kind: "authorize" }
      : { kind: "untrusted" };
  };

  let phase = $state<Phase>(initialPhase());

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

  // Manage the live sign-in phases (wizard → connect / untrusted). Terminal and
  // redirect-outcome phases (close, error, invalid) are owned by the initial
  // outcome and never re-evaluated here. Switching identity only *selects* (it
  // doesn't authenticate), so we leave the wizard only once the chosen identity
  // has actually authenticated — and only once `selected` is populated, since
  // sign-up authenticates and *then* selects, and the reused picker reads
  // `selected` at mount. On the connect/untrusted screens we re-derive trust
  // whenever the selection changes.
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
      if (phase.kind !== "wizard") {
        phase = { kind: "wizard" };
      }
      return;
    }
    if (phase.kind === "wizard" && !$isAuthenticatedStore) {
      return;
    }
    if (isServerTrusted(selected.identityNumber)) {
      if (phase.kind !== "authorize") {
        phase = { kind: "authorize" };
      }
    } else if (phase.kind !== "untrusted") {
      mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.ServerUntrusted);
      phase = { kind: "untrusted" };
    }
  });

  // Invoked by the reused account picker once it has authenticated the selected
  // identity and resolved the chosen account. Connecting performs the opt-in
  // (`mcp_set_access`) and delivers the standing delegation to the MCP server.
  const handleAuthorize = (
    accountNumberPromise: Promise<bigint | undefined>,
  ): void => {
    const server = mcpServer;
    if (params.kind !== "valid" || server === undefined) {
      return;
    }
    const request = params;
    mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.Confirmed);
    void (async () => {
      try {
        const accountNumber = await accountNumberPromise;
        const authenticated = get(authenticationStore);
        if (authenticated === undefined) {
          return;
        }
        // On success `mcpAuthorize` navigates the browser to the MCP server,
        // which redirects back here with a status — so a resolved promise means
        // the chain was built and submitted, not that we stay on this page.
        await mcpAuthorize({
          authenticated,
          publicKey: request.publicKey,
          mcpServerOrigin: server.origin,
          accountNumber,
          ttlMinutes: request.ttlMinutes,
          callback: request.callback,
          state: request.state,
        });
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
    onAuthorize={handleAuthorize}
  />
{:else if phase.kind === "untrusted" && mcpServer !== undefined}
  <McpUntrustedView mcpServerHost={mcpServer.host} />
{:else if phase.kind === "close"}
  <McpCloseWindowView />
{/if}
