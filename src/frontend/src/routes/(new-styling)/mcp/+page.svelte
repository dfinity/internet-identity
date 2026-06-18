<script lang="ts">
  import type { PageProps } from "./$types";
  import {
    isAuthenticatedStore,
    authenticationStore,
  } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { sessionStore } from "$lib/stores/session.store";
  import { mcpAccessStore } from "$lib/stores/mcp-access.store";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import { t } from "$lib/stores/locale.store";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { getMcpServerOrigin } from "$lib/globals";
  import { get } from "svelte/store";
  import { onMount } from "svelte";
  import McpHero from "./components/McpHero.svelte";
  import McpAuthorizeView from "./views/McpAuthorizeView.svelte";
  import McpCloseWindowView from "./views/McpCloseWindowView.svelte";
  import McpDisabledView from "./views/McpDisabledView.svelte";
  import McpErrorView from "./views/McpErrorView.svelte";
  import McpInvalidView from "./views/McpInvalidView.svelte";
  import { mcpAuthorize } from "./utils";
  import { showIdentitySwitcher } from "./mcp-switcher.store";
  import {
    mcpAuthorizeFunnel,
    McpAuthorizeEvents,
  } from "$lib/utils/analytics/mcpAuthorizeFunnel";

  const { data }: PageProps = $props();
  const params = $derived(data.params);
  const status = $derived(data.status);

  // The MCP server is configured as a deploy arg; when unset (or misconfigured)
  // the flow is disabled. The delegation is delivered to this origin only.
  // Parse it once, defensively: a malformed value is treated as unset so the
  // route shows the invalid screen rather than throwing during render.
  const mcpServer = ((): { origin: string; host: string } | undefined => {
    const raw = getMcpServerOrigin();
    if (raw === undefined) {
      return undefined;
    }
    try {
      const url = new URL(raw);
      return { origin: url.origin, host: url.host };
    } catch {
      return undefined;
    }
  })();
  const mcpServerHost = mcpServer?.host;

  // The request's callback must point at the configured MCP server origin. This
  // mirrors the `form-action` CSP, which only allows that origin — checking it
  // here lets us show a clean invalid screen instead of a silent CSP block.
  const callbackMatchesMcpServer = (callback: string): boolean => {
    if (mcpServer === undefined) {
      return false;
    }
    try {
      return new URL(callback).origin === mcpServer.origin;
    } catch {
      return false;
    }
  };
  const requestValid = $derived(
    params.kind === "valid" && callbackMatchesMcpServer(params.callback),
  );

  const authFlow = new AuthLastUsedFlow();
  $effect(() =>
    authFlow.init(
      Object.values($lastUsedIdentitiesStore.identities).map(
        ({ identityNumber }) => identityNumber,
      ),
    ),
  );

  // MCP is always app-scoped, so the chosen identity must have MCP access
  // enabled on this device.
  const isMcpAccessGated = (identityNumber: bigint | undefined): boolean =>
    identityNumber !== undefined && !mcpAccessStore.isEnabled(identityNumber);

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
        // A returning user without device MCP access opens straight on the
        // gated screen, so record that terminal outcome here.
        if (phase.kind === "mcp-disabled") {
          mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.AccessDisabled);
          mcpAuthorizeFunnel.close();
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
    | { kind: "close" }
    | { kind: "mcp-disabled" }
    | { kind: "invalid" }
    | { kind: "error" };

  // The phase the page opens on. Outcomes the MCP server redirects back with
  // take priority; otherwise a returning user with a previously-used identity
  // opens on the authorize screen, and a user with no last-used identity starts
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
    if (selected !== undefined) {
      // MCP access is a device-local flag we can read up front, so when it's
      // off for this identity show the gated screen immediately rather than the
      // Allow access button followed by a gate after sign-in.
      return isMcpAccessGated(selected.identityNumber)
        ? { kind: "mcp-disabled" }
        : { kind: "authorize" };
    }
    return { kind: "wizard" };
  };

  let phase = $state<Phase>(initialPhase());

  // The switcher is only meaningful while the user is choosing how to sign in.
  $effect(() => {
    showIdentitySwitcher.set(
      phase.kind === "wizard" || phase.kind === "authorize",
    );
  });

  // Once an identity has actually signed in (via the wizard here or the "use
  // another identity" dialog in the layout), advance from the wizard to the
  // authorize step. Switching identity only *selects* and never authenticates,
  // so it never triggers this.
  $effect(() => {
    if (phase.kind === "wizard" && $isAuthenticatedStore) {
      phase = isMcpAccessGated($authenticationStore?.identityNumber)
        ? { kind: "mcp-disabled" }
        : { kind: "authorize" };
    }
  });

  const handleAuthorize = async (): Promise<void> => {
    if (params.kind !== "valid" || mcpServer === undefined) {
      return;
    }
    const selected = $lastUsedIdentitiesStore.selected;
    if (selected === undefined) {
      return;
    }
    mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.Confirmed);
    try {
      // The Allow access button is the single sign-in point: switching identity
      // only selects, so authenticate the selected identity now if it isn't
      // already the active session.
      if ($authenticationStore?.identityNumber !== selected.identityNumber) {
        sessionStore.reset();
        await authFlow.authenticate(
          $lastUsedIdentitiesStore.identities[`${selected.identityNumber}`],
        );
      }
      const authenticated = get(authenticationStore);
      if (authenticated === undefined) {
        return;
      }
      if (isMcpAccessGated(authenticated.identityNumber)) {
        mcpAuthorizeFunnel.trigger(McpAuthorizeEvents.AccessDisabled);
        mcpAuthorizeFunnel.close();
        phase = { kind: "mcp-disabled" };
        return;
      }
      // On success `mcpAuthorize` navigates the browser to the MCP server,
      // which redirects back here with a status — so a resolved promise means
      // the chain was built and submitted, not that we stay on this page.
      await mcpAuthorize({
        authenticated,
        publicKey: params.publicKey,
        mcpServerOrigin: mcpServer.origin,
        ttlMinutes: params.ttlMinutes,
        callback: params.callback,
        state: params.state,
      });
    } catch (error) {
      handleError(error);
    }
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
{:else if phase.kind === "wizard" && params.kind === "valid" && mcpServerHost !== undefined}
  <div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
    <AuthPanel>
      <McpHero mcpServer={mcpServerHost} />
      <AuthWizard {...wizardSignInHandlers}>
        <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
          {$t`Choose method`}
        </h1>
        <p class="text-text-secondary mb-6 self-start text-sm">
          {$t`to connect ${mcpServerHost}`}
        </p>
      </AuthWizard>
    </AuthPanel>
  </div>
{:else if phase.kind === "authorize" && params.kind === "valid" && mcpServerHost !== undefined}
  <McpAuthorizeView mcpServer={mcpServerHost} onAuthorize={handleAuthorize} />
{:else if phase.kind === "mcp-disabled"}
  <McpDisabledView />
{:else if phase.kind === "close"}
  <McpCloseWindowView />
{/if}
