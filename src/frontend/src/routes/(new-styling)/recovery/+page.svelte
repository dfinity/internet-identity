<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
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
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import {
    fromMnemonicWithoutValidation,
    IC_DERIVATION_PATH,
  } from "$lib/utils/recoveryPhrase";
  import { HttpAgent } from "@icp-sdk/core/agent";
  import { anonymousActor, anonymousAgent } from "$lib/globals";
  import { throwCanisterError } from "$lib/utils/utils";
  import { handleError } from "$lib/components/utils/error";
  import {
    authenticatedStore,
    authenticationStore,
  } from "$lib/stores/authentication.store";
  import { authenticateWithSession } from "$lib/utils/authentication";
  import { goto } from "$app/navigation";
  import { toaster } from "$lib/components/utils/toaster";

  let showRecoveryDialog = $state(false);

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
          recoveryPhrase: true,
        },
      });
      // Set name if identity didn't have a name yet (upgrading from II 1.0)
      if (newName !== undefined) {
        await $authenticatedStore.actor
          .identity_properties_replace(identityNumber, {
            name: [newName],
          })
          .then(throwCanisterError);
      }
      // Redirect to dashboard with success message
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
        <p class="text-text-tertiary mb-2 text-sm">
          <Trans>
            Ensure you are in a private place before entering your recovery
            phrase. It grants full access to your identity and must remain
            confidential.
          </Trans>
        </p>
        <p class="text-text-tertiary mb-6 text-sm">
          <Trans>
            Have your phrase ready so the next step is quick and accurate. If
            you need a moment to prepare, feel free to pause here before moving
            forward.
          </Trans>
        </p>
        <Button
          onclick={() => (showRecoveryDialog = true)}
          size="xl"
          class="mb-3"
        >
          {$t`Get started`}
        </Button>
        <Button href="/login" variant="secondary" size="xl">
          {$t`Cancel`}
        </Button>
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
