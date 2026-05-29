<script lang="ts">
  import type { PageProps } from "./$types";
  import { isAuthenticatedStore } from "$lib/stores/authentication.store";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { sessionStore } from "$lib/stores/session.store";
  import { cliAccessStore } from "$lib/stores/cli-access.store";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { CircleAlertIcon } from "@lucide/svelte";
  import { get } from "svelte/store";
  import { onMount } from "svelte";
  import CliAuthorizeView from "./views/CliAuthorizeView.svelte";
  import CliCloseWindowView from "./views/CliCloseWindowView.svelte";
  import CliErrorView from "./views/CliErrorView.svelte";
  import { cliAuthorize } from "./utils";
  import { showIdentitySwitcher } from "./cli-switcher.store";

  const { data }: PageProps = $props();
  const params = $derived(data.params);
  const status = $derived(data.status);

  // Authenticates a selected identity from the last-used list. The identity
  // switcher here only *selects*; the big Continue button is the single point
  // that actually signs in (see `handleAuthorize`).
  const authFlow = new AuthLastUsedFlow();
  $effect(() =>
    authFlow.init(
      Object.values($lastUsedIdentitiesStore.identities).map(
        ({ identityNumber }) => identityNumber,
      ),
    ),
  );

  // After a mismatch the loopback server redirects back here so the user can
  // retry with the right identity; surface why before they try again.
  onMount(() => {
    if (status === "identity-mismatch") {
      toaster.error({
        title: $t`That identity doesn't match`,
        description: $t`Choose the identity you originally linked, then try again.`,
        duration: 6000,
      });
    }
    // Drop the URL fragment once parsed so the public_key, callback, and
    // status don't sit in the address bar after the user lands here.
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
    | { kind: "cli-disabled" }
    | { kind: "invalid" }
    | { kind: "error" };

  // In app mode the chosen identity must have CLI access enabled on this
  // device; otherwise it's a generic CLI sign-in with no gate.
  const isCliAccessGated = (identityNumber: bigint | undefined): boolean =>
    params.kind === "valid" &&
    params.domain !== undefined &&
    identityNumber !== undefined &&
    !cliAccessStore.isEnabled(identityNumber);

  // The phase the page opens on. The success/error outcomes the loopback
  // server redirects back with take priority; a mismatch keeps the request
  // params, so it falls through to the authorize screen (plus the onMount
  // toast) for an in-place retry.
  const initialPhase = (): Phase => {
    if (status === "success") {
      return { kind: "close" };
    }
    if (status === "error") {
      return { kind: "error" };
    }
    if (params.kind !== "valid") {
      return { kind: "invalid" };
    }
    // Mirror /authorize: a returning user with a previously-used identity opens
    // on the Continue/authorize screen for it, and only a user with no last-used
    // identity starts in the sign-in method wizard. The Continue button
    // (`handleAuthorize`) authenticates the selected identity if it isn't the
    // active session yet, so opening here works even before authentication.
    //
    // This holds on an identity-mismatch redirect too: the user stays on the
    // Continue screen and picks the right identity from the header switcher
    // (plus the onMount toast explaining the mismatch), rather than being
    // dropped back into the method wizard.
    if (get(lastUsedIdentitiesStore).selected !== undefined) {
      return { kind: "authorize" };
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

  // The only transition driven by auth state: once an identity has actually
  // signed in (via the wizard here or the "use another identity" dialog in the
  // layout), advance from the wizard to the authorize step. Switching identity
  // only *selects* and never authenticates, so it never triggers this.
  $effect(() => {
    if (phase.kind === "wizard" && $isAuthenticatedStore) {
      phase = isCliAccessGated($authenticationStore?.identityNumber)
        ? { kind: "cli-disabled" }
        : { kind: "authorize" };
    }
  });

  const handleAuthorize = async (): Promise<void> => {
    if (params.kind !== "valid") {
      return;
    }
    const selected = $lastUsedIdentitiesStore.selected;
    if (selected === undefined) {
      return;
    }
    try {
      // The Continue button is the single sign-in point: switching identity
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
      if (isCliAccessGated(authenticated.identityNumber)) {
        phase = { kind: "cli-disabled" };
        return;
      }
      // On success `cliAuthorize` navigates the browser to the loopback server,
      // which redirects back here with a status — so a resolved promise means
      // the chain was built and submitted, not that we stay on this page.
      await cliAuthorize({
        authenticated,
        publicKey: params.publicKey,
        domain: params.domain,
        ttlMinutes: params.ttlMinutes,
        callback: params.callback,
        nonce: params.nonce,
      });
    } catch (error) {
      // Authentication and delegation errors are surfaced the same way as the
      // rest of the app — a toast (with a graceful notice for a cancelled
      // prompt) — and the user stays here to click Continue again. The error
      // screen is reserved for CLI-reported failures (status=error).
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
    onUpgrade: (identityNumber: bigint): Promise<void> => {
      lastUsedIdentitiesStore.selectIdentity(identityNumber);
      return Promise.resolve();
    },
    onError: handleError,
  };
</script>

{#if phase.kind === "invalid"}
  <AuthPanel>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Invalid request`}
    </h1>
    <p class="text-text-tertiary mb-2 text-base">
      <Trans>
        It seems like an invalid CLI authentication request was received.
      </Trans>
    </p>
    <p class="text-text-tertiary text-base">
      {$t`You can close this window.`}
    </p>
  </AuthPanel>
{:else if phase.kind === "error"}
  <AuthPanel>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Something went wrong`}
    </h1>
    <p class="text-text-tertiary mb-2 text-base">
      <Trans>
        The CLI sign-in couldn't be completed. Check the CLI for details.
      </Trans>
    </p>
    <p class="text-text-tertiary text-base">
      {$t`You can close this window.`}
    </p>
  </AuthPanel>
{:else if phase.kind === "wizard"}
  <div class="flex w-full justify-center max-sm:flex-1 sm:max-w-110">
    <AuthPanel>
      <AuthWizard {...wizardSignInHandlers}>
        <h1 class="text-text-primary my-2 self-start text-2xl font-medium">
          {$t`Sign in`}
        </h1>
        <p class="text-text-secondary mb-6 self-start text-sm">
          {$t`Choose method to continue`}
        </p>
      </AuthWizard>
    </AuthPanel>
  </div>
{:else if phase.kind === "authorize" && params.kind === "valid"}
  <CliAuthorizeView domain={params.domain} onAuthorize={handleAuthorize} />
{:else if phase.kind === "cli-disabled"}
  <CliErrorView />
{:else if phase.kind === "close"}
  <CliCloseWindowView />
{/if}
