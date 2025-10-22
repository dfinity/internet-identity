<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { goto } from "$app/navigation";
  import { authorizationContextStore } from "$lib/stores/authorization.store";
  import { toaster } from "$lib/components/utils/toaster";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { handleError } from "$lib/components/utils/error";
  import { AuthWizard } from "$lib/components/wizards/auth";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    ),
  );

  const onSignIn = async (identityNumber: bigint) => {
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    await goto("/authorize/continue");
  };
  const onSignUp = async (identityNumber: bigint) => {
    toaster.success({
      title: $t`You're all set. Your identity has been created.`,
      duration: 4000,
    });
    lastUsedIdentitiesStore.selectIdentity(identityNumber);
    await goto("/authorize/continue");
  };
  const onMigration = async () => {
    await goto("/authorize/upgrade-success");
  };
</script>

<AuthWizard {onSignIn} {onSignUp} {onMigration} onError={handleError}>
  <AuthorizeHeader origin={$authorizationContextStore.requestOrigin} />
  <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
    {$t`Choose method`}
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">
    {#if nonNullish(dapp?.name)}
      {@const application = dapp.name}
      <Trans>
        to continue with <b>{application}</b>
      </Trans>
    {:else}
      <Trans>to continue with this app</Trans>
    {/if}
  </p>
</AuthWizard>
