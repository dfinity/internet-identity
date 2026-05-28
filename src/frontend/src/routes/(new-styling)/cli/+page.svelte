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
  import CliAuthorizeView from "./views/CliAuthorizeView.svelte";
  import CliCloseWindowView from "./views/CliCloseWindowView.svelte";
  import CliErrorView from "./views/CliErrorView.svelte";
  import { cliAuthorize } from "./cliAuthorize";

  const { data }: PageProps = $props();
  const params = $derived(data.params);

  type Phase =
    | { kind: "wizard" }
    | { kind: "authorize" }
    | { kind: "close" }
    | { kind: "cli-disabled" }
    | { kind: "callback-failed"; message: string };

  // Selecting the initial phase happens whenever the user signs in / out
  // mid-flow (e.g. switching identities via the header switcher).
  const initialPhase = (): Phase => {
    if (params.kind !== "valid") {
      return { kind: "wizard" };
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
  // state; we never bounce the user off a terminal phase (close, error).
  $effect(() => {
    const id = $authenticationStore?.identityNumber;
    if (phase.kind === "close" || phase.kind === "callback-failed") {
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
    try {
      await cliAuthorize({
        authenticated,
        publicKeyHex: params.publicKey,
        appHost: params.appHost,
        ttlMinutes: params.ttlMinutes,
        callback: params.callback,
      });
      phase = { kind: "close" };
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      phase = { kind: "callback-failed", message };
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

{#if params.kind === "invalid"}
  <AuthPanel>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Invalid request`}
    </h1>
    <p class="text-text-tertiary mb-6 text-base">
      <Trans>
        It seems like an invalid CLI authentication request was received.
      </Trans>
    </p>
    <button class="btn btn-secondary" onclick={() => window.close()}>
      <RotateCcwIcon class="size-4" />
      <span>{$t`Return to terminal`}</span>
    </button>
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
{:else if phase.kind === "authorize"}
  <CliAuthorizeView appHost={params.appHost} onAuthorize={handleAuthorize} />
{:else if phase.kind === "cli-disabled"}
  <CliErrorView />
{:else if phase.kind === "close"}
  <CliCloseWindowView />
{:else if phase.kind === "callback-failed"}
  <AuthPanel>
    <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
      <CircleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Couldn't reach the CLI`}
    </h1>
    <p class="text-text-tertiary mb-6 text-base">
      <Trans>Make sure the CLI is still running and try again.</Trans>
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
