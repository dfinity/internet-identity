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
  import { readMcpConfig } from "$lib/utils/mcpConfig";
  import { backendCanisterConfig } from "$lib/globals";
  import { get } from "svelte/store";
  import { onMount } from "svelte";
  import McpHero from "./components/McpHero.svelte";
  import McpAuthorizeView from "./views/McpAuthorizeView.svelte";
  import McpCloseWindowView from "./views/McpCloseWindowView.svelte";
  import McpInvalidView from "./views/McpInvalidView.svelte";
  import McpUntrustedView from "./views/McpUntrustedView.svelte";
  import McpLocalServerNoticeView from "./views/McpLocalServerNoticeView.svelte";
  import McpConnectingView from "./views/McpConnectingView.svelte";
  import { localServerNoticeStore } from "./local-server-notice.store";
  import ManageHandoff from "$lib/components/ui/ManageHandoff.svelte";
  import { ManageHandoffFlow } from "$lib/flows/manageHandoffFlow.svelte";
  import {
    isOriginTrusted,
    mcpAuthorize,
    McpUntrustedServerError,
  } from "./utils";
  import { showIdentitySwitcher } from "./mcp-switcher.store";
  import {
    mcpAuthorizeFunnel,
    McpAuthorizeEvents,
  } from "$lib/utils/analytics/mcpAuthorizeFunnel";

  const { data }: PageProps = $props();
  const params = $derived(data.params);

  // The MCP server the user is connecting is identified by the origin of the
  // request's callback: each user trusts whichever server they connect. The
  // connect flow delivers the registration delegation to that callback — once
  // `mcpAuthorize` has confirmed the origin is the trusted one and, for a
  // remote server, matched the callback against the allow-list that server
  // declares. Two shapes are accepted: an https callback for a remote server,
  // and an `http://127.0.0.1[:port]` one for a local server on this computer.
  // Anything else (or an unparsable callback) yields `undefined` → the invalid
  // screen.
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
    | { kind: "local-notice" }
    | { kind: "untrusted" }
    | { kind: "connecting" }
    | { kind: "close"; redirecting: boolean }
    | { kind: "invalid" };

  // The identity that accepted the local-server notice in this tab. Held here
  // rather than written straight to the device store, so a connect that goes on
  // to be refused (a crafted link naming a loopback callback the identity
  // doesn't actually permit) can't leave an "already told" behind that would
  // make the next attempt one screen quieter. It is committed on success.
  let noticeAcceptedFor = $state<bigint | undefined>(undefined);

  // The connect screen, or the local-server notice that precedes it the first
  // time this identity signs in to a local server on this computer. Answerable
  // without the canister — the callback comes from the fragment and the record
  // is device-local — so it can run before the identity has authenticated,
  // which is what lets the notice come *before* the consent screen rather than
  // after the user has already approved the connect.
  const connectPhase = (identityNumber: bigint): Phase =>
    mcpServer?.isLoopback === true &&
    noticeAcceptedFor !== identityNumber &&
    !localServerNoticeStore.isAcknowledged(identityNumber)
      ? { kind: "local-notice" }
      : { kind: "authorize" };

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
    return connectPhase(selected.identityNumber);
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
        phase.kind === "local-notice" ||
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
      phase.kind !== "local-notice" &&
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
      phase = connectPhase(selected.identityNumber);
    }
  });

  // Auto-advance the untrusted screen once the user trusts this server. The
  // "Manage trusted server" button hands the session to a Settings tab where the
  // user sets the trusted server; when they come back to this tab, re-read the
  // synced config and — if this server's origin is now trusted — move straight
  // to the connect screen, so they don't have to restart the connect ("then try
  // again" happens for them). Same unblock as switching to an identity that
  // already trusts the server. Re-checking when the tab regains focus/visibility
  // (rather than polling) is enough: the trusted server is set in another tab, so
  // regaining focus here is exactly the moment the synced config may have
  // changed. The read reuses the actor the untrusted screen already holds
  // (reaching it authenticated the identity); minting still waits for an explicit
  // "Allow access" on the connect screen.
  $effect(() => {
    if (phase.kind !== "untrusted" || mcpServer === undefined) {
      return;
    }
    const server = mcpServer;
    let checking = false;
    const recheck = (): void => {
      if (checking || document.visibilityState !== "visible") {
        return;
      }
      const authenticated = get(authenticationStore);
      if (authenticated === undefined) {
        return;
      }
      const identityNumber = authenticated.identityNumber;
      checking = true;
      void (async () => {
        try {
          const config = await readMcpConfig(
            authenticated.actor,
            identityNumber,
          );
          // Only act if the user is still on the untrusted screen for the same
          // identity this config was read (and signed) as: a late resolve mustn't
          // yank them off a screen they've since moved to (navigated away), and —
          // since the untrusted screen is reachable again for a different identity
          // after a switch — applying a stale read must not unblock the wrong one.
          if (
            phase.kind === "untrusted" &&
            get(authenticationStore)?.identityNumber === identityNumber &&
            isOriginTrusted(config, server.origin, backendCanisterConfig)
          ) {
            phase = { kind: "authorize" };
          }
        } catch {
          // Couldn't read the config (e.g. a transient error or an expired
          // session): leave the user on the untrusted screen to retry manually.
        } finally {
          checking = false;
        }
      })();
    };
    document.addEventListener("visibilitychange", recheck);
    window.addEventListener("focus", recheck);
    return () => {
      document.removeEventListener("visibilitychange", recheck);
      window.removeEventListener("focus", recheck);
    };
  });

  // Invoked by the reused account picker once it has authenticated the selected
  // identity and resolved the chosen account. Connecting mints a short-lived
  // registration delegation for the server's per-connect key (rooted at a
  // principal `P_reg` seeded from a fresh random nonce; the whole consent —
  // anchor, access level, grant lifetime — is recorded canister-side, so the
  // server can't alter it) and hands the tab to the server's declared callback,
  // which redeems it (`mcp_register_v2`).
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
        // Fast pre-filter, NOT the security gate: if this identity plainly
        // doesn't trust the link's origin, go to the untrusted screen now —
        // before `mcpAuthorize`, whose `prepare` step would otherwise mint a
        // registration and flip the synced config to enabled for a connect we
        // are about to reject. This `mcp_get_config` query is forgeable by a
        // malicious node, so it does NOT gate delivery: delivery is gated
        // authoritatively inside `mcpAuthorize` on `prepare`'s *certified*
        // `trusted_url` (an update call), which a forged query cannot defeat.
        const config = await readMcpConfig(
          authenticated.actor,
          authenticated.identityNumber,
        );
        if (!isOriginTrusted(config, server.origin, backendCanisterConfig)) {
          mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.ServerUntrusted);
          phase = { kind: "untrusted" };
          return;
        }
        // Trust looks right; mint and deliver. `mcpAuthorize` re-checks the
        // origin against `prepare`'s certified `trusted_url` (the real gate) and
        // matches the link's callback against the server's declared allow-list
        // before delivering. A certified-gate mismatch throws
        // `McpUntrustedServerError` (handled below); the backend re-checks the
        // trust again when the server registers.
        const deliveryUrl = await mcpAuthorize({
          authenticated,
          ttlSeconds,
          accessLevel,
          serverOrigin: server.origin,
          // The raw callback from the (attacker-craftable) link; matched inside
          // `mcpAuthorize` only once its origin is trust-confirmed.
          requestedCallback: request.callback,
          state: request.state,
          // The server's public per-connect key `X` from the link (validated
          // base64url in `load`); the browser-signed final hop of the
          // registration chain targets it.
          registrationKey: fromBase64URL(request.registrationKey),
        });
        mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.Success);
        // Trust held and the delegation is minted, so the local-server notice
        // has been earned: record it for this identity on this computer, and
        // don't show it again here. Committed only now — never at the moment
        // the user accepted it — so an attempt that ended at the untrusted
        // screen leaves the next one just as loud.
        if (server.isLoopback) {
          localServerNoticeStore.acknowledge(authenticated.identityNumber);
        }
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
        // An untrusted server is an expected outcome, not an error: route to
        // the untrusted screen (where setting the trusted server auto-advances
        // the connect). Nothing has been delivered — the minted registration
        // stays inert and expires.
        if (error instanceof McpUntrustedServerError) {
          mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.ServerUntrusted);
          phase = { kind: "untrusted" };
          return;
        }
        // Anything else returns to the connect screen so the user can retry.
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
  // The local-server notice: continuing goes on to the consent screen, which
  // still gates this connect and every later one. Cancelling drops the connect
  // rather than proceeding silently, so declining actually declines.
  const handleLocalNoticeContinue = (): void => {
    const selected = $lastUsedIdentitiesStore.selected;
    if (selected === undefined) {
      return;
    }
    noticeAcceptedFor = selected.identityNumber;
    phase = { kind: "authorize" };
  };

  const handleLocalNoticeCancel = (): void => {
    mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.Error);
    phase = { kind: "close", redirecting: false };
  };

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
      <McpHero />
      <AuthWizard {...wizardSignInHandlers} mode="signin">
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
    mcpServerOrigin={mcpServer.origin}
    requestedTtlSeconds={params.kind === "valid" ? params.ttlSeconds : 3600}
    onAuthorize={handleAuthorize}
  />
{:else if phase.kind === "local-notice" && mcpServer !== undefined}
  <McpLocalServerNoticeView
    mcpServerHost={mcpServer.host}
    onContinue={handleLocalNoticeContinue}
    onCancel={handleLocalNoticeCancel}
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
