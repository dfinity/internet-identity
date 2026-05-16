<script lang="ts">
  import Trans from "../../../lib/components/locale/Trans.svelte";
  import { t } from "$lib/stores/locale.store";
  import { RefreshCcw } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Header from "$lib/components/layout/Header.svelte";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import Footer from "$lib/components/layout/Footer.svelte";
  import {
    type FoundIdentity,
    RecoverIdentityWizard,
  } from "$lib/components/wizards/recoverIdentity";
  import {
    RecoverWithEmailWizard,
    type RecoverySuccess,
  } from "$lib/components/wizards/recoverWithEmail";
  import type {
    EmailRecoveryDnsInput,
    EmailRecoveryGetDelegationArgs,
  } from "$lib/generated/internet_identity_types";
  import { EMAIL_RECOVERY } from "$lib/state/featureFlags";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import {
    fromMnemonicWithoutValidation,
    IC_DERIVATION_PATH,
  } from "$lib/utils/recoveryPhrase";
  import { HttpAgent } from "@icp-sdk/core/agent";
  import { anonymousActor, anonymousAgent } from "$lib/globals";
  import { throwCanisterError } from "$lib/utils/utils";
  import { handleError } from "$lib/components/utils/error";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import { authenticateWithSession } from "$lib/utils/authentication";
  import { goto, preloadData } from "$app/navigation";
  import { toaster } from "$lib/components/utils/toaster";
  import { DelegationChain, DelegationIdentity } from "@icp-sdk/core/identity";
  import { transformSignedDelegation } from "$lib/utils/utils";

  let showRecoveryDialog = $state(false);
  let showEmailRecoveryDialog = $state(false);

  const handleSubmit = async (
    recoveryPhrase: string[],
  ): Promise<FoundIdentity | undefined> => {
    try {
      const identity = await fromMnemonicWithoutValidation(
        recoveryPhrase.join(" "),
        IC_DERIVATION_PATH,
      );
      const agent = await HttpAgent.from(anonymousAgent);
      agent.replaceIdentity(identity);
      const [identityNumber] =
        await anonymousActor.lookup_caller_identity_by_recovery_phrase.withOptions(
          { agent },
        )();
      if (identityNumber === undefined) {
        // Identity not found
        return;
      }
      const identityInfo = await anonymousActor.identity_info
        .withOptions({ agent })(identityNumber)
        .then(throwCanisterError);
      return { identityNumber, identityInfo };
    } catch (error) {
      showRecoveryDialog = false;
      handleError(error);
    }
  };

  // -------------------------------------------------------------
  // Email-recovery wizard plumbing
  // -------------------------------------------------------------

  const prepareEmailDelegation = (
    input: EmailRecoveryDnsInput,
    sessionPublicKey: Uint8Array,
  ) =>
    anonymousActor
      .email_recovery_prepare_delegation(input, sessionPublicKey)
      .then(throwCanisterError);

  const emailRecoveryStatus = (nonce: string) =>
    anonymousActor.email_recovery_status(nonce);

  const submitEmailDkimLeaf = (
    arg: import("$lib/generated/internet_identity_types").EmailRecoverySubmitDkimLeafArg,
  ) =>
    anonymousActor
      .email_recovery_submit_dkim_leaf(arg)
      .then(throwCanisterError);

  const getEmailDelegation = (args: EmailRecoveryGetDelegationArgs) =>
    anonymousActor.email_recovery_get_delegation(args).then(throwCanisterError);

  /**
   * Bridge the wizard's "I have a SignedDelegation" output back into
   * the manage-page session: build a DelegationIdentity, seed the
   * auth store with the anchor the canister already resolved at
   * smtp time (carried back via `RecoverySuccess.identityNumber`),
   * and navigate.
   */
  const handleEmailRecoverySignIn = async (success: RecoverySuccess) => {
    try {
      const delegationChain = DelegationChain.fromDelegations(
        [transformSignedDelegation(success.delegation)],
        new Uint8Array(success.userKey),
      );
      const delegationIdentity = DelegationIdentity.fromDelegation(
        success.sessionIdentity,
        delegationChain,
      );
      await authenticationStore.set({
        identity: delegationIdentity,
        identityNumber: success.identityNumber,
        authMethod: {
          emailRecovery: {
            principal: delegationIdentity.getPrincipal(),
          },
        },
      });
      await preloadData("/manage/access");
      await goto("/manage/access");
      toaster.success({
        title: $t`Successfully recovered your identity`,
        description: $t`You can manage your access methods on this page.`,
        duration: 5000,
      });
    } catch (error) {
      showEmailRecoveryDialog = false;
      authenticationStore.reset();
      handleError(error);
    }
  };

  const handleSignIn = async (
    identityNumber: bigint,
    recoveryPhrase: string[],
    newName?: string,
  ): Promise<void> => {
    try {
      // Authenticate with recovery phrase
      const identity = await fromMnemonicWithoutValidation(
        recoveryPhrase.join(" "),
        IC_DERIVATION_PATH,
      );
      await authenticationStore.set({
        identity: await authenticateWithSession({ session: { identity } }),
        identityNumber,
        authMethod: {
          recoveryPhrase: { principal: identity.getPrincipal() },
        },
      });
      // Set name if identity didn't have a name yet (upgrading from II 1.0)
      if (newName !== undefined) {
        await $authenticationStore?.actor
          .identity_properties_replace(identityNumber, {
            name: [newName],
          })
          .then(throwCanisterError);
      }
      // Preload and then redirect to dashboard with success message
      await preloadData("/manage/access");
      await goto("/manage/access");
      toaster.success({
        title: $t`Successfully restored access to your identity`,
        description: $t`Make sure to add a new access method so that you can sign in next time or reset your recovery phrase.`,
        duration: 5000,
      });
    } catch (error) {
      showRecoveryDialog = false;
      authenticationStore.reset();
      handleError(error);
    }
  };
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="h-[env(safe-area-inset-top)]"></div>
  <Header />
  <div class="flex flex-1 flex-col items-center justify-center">
    <AuthPanel class="sm:max-w-100">
      <div class="mt-auto flex flex-col sm:my-auto">
        <FeaturedIcon size="lg" class="mb-4">
          <RefreshCcw class="size-5" />
        </FeaturedIcon>
        <h1 class="text-text-primary mb-3 text-2xl font-medium">
          {$t`Recover your identity`}
        </h1>
        <p class="text-text-tertiary mb-6 text-base text-pretty">
          <Trans>
            Before getting started, find a private place and have your recovery
            phrase ready. Keep it confidential to protect your identity.
          </Trans>
        </p>
        <button
          onclick={() => (showRecoveryDialog = true)}
          class="btn btn-xl mb-3"
        >
          {$t`Recover with phrase`}
        </button>
        {#if $EMAIL_RECOVERY}
          <button
            onclick={() => (showEmailRecoveryDialog = true)}
            class="btn btn-secondary btn-xl mb-3"
          >
            {$t`Recover with email`}
          </button>
        {/if}
        <a href="/" class="btn btn-secondary btn-xl">
          {$t`Cancel`}
        </a>
      </div>
    </AuthPanel>
  </div>
  <Footer />
  <div class="h-[env(safe-area-inset-bottom)]"></div>
</div>

{#if showRecoveryDialog}
  <Dialog onClose={() => (showRecoveryDialog = false)}>
    <RecoverIdentityWizard
      onSubmit={handleSubmit}
      onSignIn={handleSignIn}
      onCancel={() => (showRecoveryDialog = false)}
    />
  </Dialog>
{/if}

{#if showEmailRecoveryDialog}
  <Dialog
    onClose={() => (showEmailRecoveryDialog = false)}
    closeOnOutsideClick={false}
  >
    <RecoverWithEmailWizard
      prepareDelegation={prepareEmailDelegation}
      status={emailRecoveryStatus}
      submitDkimLeaf={submitEmailDkimLeaf}
      getDelegation={getEmailDelegation}
      onSignedIn={handleEmailRecoverySignIn}
      onCancel={() => (showEmailRecoveryDialog = false)}
    />
  </Dialog>
{/if}
