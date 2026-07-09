<script lang="ts">
  import type { AccessLevel } from "$lib/utils/accessLevel";
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
  import { parseMcpServerUrl } from "$lib/utils/mcpServer";
  import { fromBase64URL } from "$lib/utils/utils";
  import { readMcpConfig, isOriginTrusted } from "$lib/utils/mcpConfig";
  import { matchDeclaredCallback } from "$lib/utils/authCallbacks";
  import { get } from "svelte/store";
  import { onMount } from "svelte";
  import McpHero from "./components/McpHero.svelte";
  import McpAuthorizeView from "./views/McpAuthorizeView.svelte";
  import McpCloseWindowView from "./views/McpCloseWindowView.svelte";
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

  // The MCP server the user is connecting is identified by the origin of the
  // request's callback: each user trusts whichever (remote) server they connect.
  // The connect flow delivers the registration delegation to that callback —
  // once it has been matched against the allow-list the server declares on its
  // origin (see `matchDeclaredCallback` in `handleAuthorize`). MCP is
  // remote-only, so only https callbacks are accepted (a plain-http or loopback
  // callback is rejected). A disallowed (or unparsable) callback yields
  // `undefined` → the invalid screen.
  const mcpServer = $derived(
    params.kind === "valid" ? parseMcpServerUrl(params.callback) : undefined,
  );

  const requestValid = $derived(
    params.kind === "valid" && mcpServer !== undefined,
  );

  onMount(() => {
    mcpAuthorizeFunnel.init();
    if (!requestValid) {
      mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.RequestInvalid);
      mcpAuthorizeFunnel.close();
    } else {
      mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.RequestReceived);
    }

    // Drop the URL fragment once parsed so the callback and state don't sit in
    // the address bar after the user lands here.
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
    | { kind: "close"; redirecting: boolean }
    | { kind: "invalid" };

  // The phase the page opens on: a returning user with a previously-used
  // identity opens on the connect screen, and a user with no last-used identity
  // starts in the sign-in method wizard. Whether the server is actually trusted
  // is the identity's synced (on-chain) config, which can only be read once
  // authenticated — so we show the connect screen optimistically and verify it
  // against the canister at connect time (`handleAuthorize`), moving to the
  // untrusted screen if it isn't. Once the registration delegation is minted,
  // `handleAuthorize` hands the tab to the server's declared callback
  // (carrying the delegation in the fragment): the server redeems it and
  // finishes its own flow (e.g. hands an OAuth code to an MCP client). The
  // terminal `close` phase is set first so the page is truthful if that
  // navigation never replaces the document or the user comes Back to it.
  const initialPhase = (): Phase => {
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

  // Manage the live sign-in phases (wizard → connect). Terminal phases (close,
  // invalid) are owned by `handleAuthorize` / the initial request check and
  // never re-evaluated here. Switching identity only *selects* (it
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
  // identity and resolved the chosen account. Connecting mints a single-use
  // registration delegation for the server's per-connect key (recording the
  // anchor and chosen access level on the backend) and hands the tab to the
  // server's declared callback, which redeems it (`mcp_register_v2`).
  const handleAuthorize = (
    ttlSeconds: number,
    accessLevel: AccessLevel,
  ): void => {
    const server = mcpServer;
    if (params.kind !== "valid" || server === undefined) {
      return;
    }
    const request = params;
    mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.Confirmed);
    // Show a loading screen while we verify trust and (if trusted) mint the
    // registration delegation: the picker's own button spinner stops once it
    // hands off here, and those steps span several canister round trips.
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
        // is the same on every device, regardless of any local state — and the
        // backend re-checks it when registering.
        const config = await readMcpConfig(
          authenticated.actor,
          authenticated.identityNumber,
        );
        if (!isOriginTrusted(config, server.origin)) {
          mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.ServerUntrusted);
          phase = { kind: "untrusted" };
          return;
        }
        // Deliver to the trusted server only at a callback it *declares*: the
        // server hosts an allow-list at a fixed well-known path on its origin
        // (/.well-known/ii-auth-callbacks), and the link's callback must
        // exact-match a declared entry. The (attacker-craftable) link only
        // ever selects among the server-declared set — a crafted link can't
        // point II at an arbitrary path on the trusted origin (a planted
        // file, a reflecting or redirecting route). An undeclared callback,
        // or an unreachable/invalid allow-list, throws: the connect fails
        // closed into the catch below, before anything is minted.
        const callback = await matchDeclaredCallback(
          server.origin,
          request.callback,
        );
        const deliveryUrl = await mcpAuthorize({
          authenticated,
          ttlSeconds,
          accessLevel,
          callback,
          state: request.state,
          // The server's public per-connect key `X` from the link (validated
          // base64url in `load`); II mints a `P_reg -> X` delegation for it.
          registrationKey: fromBase64URL(request.registrationKey),
        });
        mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.Success);
        // The registration delegation is minted: reach the terminal close
        // screen first, so the page is in the truthful state even when the
        // navigation below never replaces the document or the user comes Back
        // to a bfcache-restored page — rather than stranding them on the
        // connecting spinner. Then hand the tab to the server's declared
        // callback with the delegation in the fragment; the server redeems it
        // (mcp_register_v2) and finishes its own flow.
        phase = { kind: "close", redirecting: true };
        window.location.assign(deliveryUrl);
      } catch (error) {
        // Return to the connect screen so the user can retry.
        mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.Error);
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
{:else if phase.kind === "authorize" && mcpServer !== undefined && $lastUsedIdentitiesStore.selected !== undefined}
  <McpAuthorizeView
    mcpServerHost={mcpServer.host}
    requestedTtlSeconds={params.kind === "valid" ? params.ttlSeconds : 3600}
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
  <McpCloseWindowView redirecting={phase.redirecting} />
{/if}

<ManageHandoff
  flow={manageHandoff}
  description={$t`Open Settings in a new tab to set your trusted MCP server.`}
  buttonLabel={$t`Open Settings`}
/>
