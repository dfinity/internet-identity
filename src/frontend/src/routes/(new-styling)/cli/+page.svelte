<script lang="ts">
  import type { PageProps } from "./$types";
  import { isAuthenticatedStore } from "$lib/stores/authentication.store";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { cliAccessStore } from "$lib/stores/cli-access.store";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import { CircleAlertIcon, RotateCcwIcon } from "@lucide/svelte";
  import { onMount } from "svelte";
  import CliAuthorizeView from "./views/CliAuthorizeView.svelte";
  import CliCloseWindowView from "./views/CliCloseWindowView.svelte";
  import CliErrorView from "./views/CliErrorView.svelte";
  import { cliAuthorize } from "./utils";

  const { data }: PageProps = $props();
  const params = $derived(data.params);
  const status = $derived(data.status);

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
    | { kind: "error" }
    | { kind: "authorize-failed"; message: string };

  // Selecting the initial phase happens whenever the user signs in / out
  // mid-flow (e.g. switching identities via the header switcher).
  const initialPhase = (): Phase => {
    // Outcomes the loopback server redirects back with take priority. A
    // mismatch keeps the request params, so it falls through to the normal
    // sign-in flow (plus the toast) for an in-place retry.
    if (status === "success") {
      return { kind: "close" };
    }
    if (status === "error") {
      return { kind: "error" };
    }
    if (params.kind !== "valid") {
      return { kind: "invalid" };
    }
    if (!$isAuthenticatedStore) {
      return { kind: "wizard" };
    }
    if (params.appHost !== undefined) {
      const identityNumber = $authenticationStore?.identityNumber;
      if (
        identityNumber !== undefined &&
        !cliAccessStore.isEnabled(identityNumber)
      ) {
        return { kind: "cli-disabled" };
      }
    }
    return { kind: "authorize" };
  };

  let phase = $state<Phase>(initialPhase());

  // Keep phase in sync with auth state changes (e.g. user switches identity
  // from the header switcher). Only auto-resyncs into the authorize/wizard
  // state; we never bounce the user off a terminal phase.
  $effect(() => {
    const id = $authenticationStore?.identityNumber;
    if (
      phase.kind === "close" ||
      phase.kind === "error" ||
      phase.kind === "invalid" ||
      phase.kind === "authorize-failed"
    ) {
      return;
    }
    if (params.kind !== "valid") {
      return;
    }
    if (!$isAuthenticatedStore) {
      phase = { kind: "wizard" };
      return;
    }
    if (
      params.appHost !== undefined &&
      id !== undefined &&
      !cliAccessStore.isEnabled(id)
    ) {
      phase = { kind: "cli-disabled" };
      return;
    }
    // Authenticated user with access (or generic mode) — move forward unless
    // we're still in the wizard, in which case the wizard's handlers will
    // advance us themselves.
    if (phase.kind === "wizard" && $isAuthenticatedStore) {
      phase = { kind: "authorize" };
    }
  });

  const handleAuthorize = async (): Promise<void> => {
    if (params.kind !== "valid") {
      return;
    }
    const authenticated = $authenticationStore;
    if (authenticated === undefined) {
      return;
    }
    // On success `cliAuthorize` navigates the browser to the loopback server,
    // which redirects back here with a status — so a resolved promise means
    // the chain was built and submitted, not that we stay on this page. Only
    // failures building the chain land in the catch.
    try {
      await cliAuthorize({
        authenticated,
        publicKey: params.publicKey,
        appHost: params.appHost,
        ttlMinutes: params.ttlMinutes,
        callback: params.callback,
      });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      phase = { kind: "authorize-failed", message };
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
  <div class="flex w-full justify-center max-sm:flex-1 sm:max-w-100">
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
  <CliAuthorizeView appHost={params.appHost} onAuthorize={handleAuthorize} />
{:else if phase.kind === "cli-disabled"}
  <CliErrorView />
{:else if phase.kind === "close"}
  <CliCloseWindowView />
{:else if phase.kind === "authorize-failed"}
  <AuthPanel>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Something went wrong`}
    </h1>
    <p class="text-text-tertiary mb-6 text-base">
      <Trans>Couldn't authorize the CLI. Please try again.</Trans>
    </p>
    <p class="text-text-tertiary mb-6 font-mono text-xs">
      {phase.message}
    </p>
    <button
      class="btn btn-secondary"
      onclick={() => (phase = { kind: "authorize" })}
    >
      <RotateCcwIcon class="size-4" />
      <span>{$t`Try again`}</span>
    </button>
  </AuthPanel>
{/if}
