<script lang="ts">
  import { goto } from "$app/navigation";
  import {
    authenticateWithJWT,
    authenticateWithPasskey,
  } from "$lib/utils/authentication";
  import {
    lastUsedIdentitiesStore,
    type LastUsedAccount,
  } from "$lib/stores/last-used-identities.store";
  import { canisterConfig, canisterId } from "$lib/globals";
  import { sessionStore } from "$lib/stores/session.store";
  import { createGoogleRequestConfig, requestJWT } from "$lib/utils/openID";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import {
    authorizationStore,
    authorizationContextStore,
  } from "$lib/stores/authorization.store";
  import { PlusIcon } from "@lucide/svelte";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { handleError } from "$lib/components/utils/error";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import { nonNullish } from "@dfinity/utils";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    ),
  );
  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected!);
  const lastUsedAccounts = $derived.by<LastUsedAccount[]>(() => {
    const accounts = Object.values(
      selectedIdentity.accounts?.[$authorizationContextStore.effectiveOrigin] ??
        {},
    ).sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis);
    if (accounts.length === 0) {
      return [
        {
          identityNumber: selectedIdentity.identityNumber,
          origin: $authorizationContextStore.effectiveOrigin,
          accountNumber: undefined,
          lastUsedTimestampMillis: selectedIdentity.lastUsedTimestampMillis,
        },
      ];
    }
    return accounts;
  });
  const authLastUsedFlow = new AuthLastUsedFlow();
  let loading = $state(false);

  const handleContinueAs = async (account: LastUsedAccount) => {
    try {
      loading = true;
      await authLastUsedFlow.authenticate(selectedIdentity);
      if ("passkey" in selectedIdentity.authMethod) {
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.ContinueAsPasskey,
        );
      } else if ("openid" in selectedIdentity.authMethod) {
        authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsGoogle);
      }
      lastUsedIdentitiesStore.addLastUsedAccount(account);
      await authorizationStore.authorize(account.accountNumber);
    } catch (error) {
      loading = false;
      handleError(error);
    }
  };

  const handleUseAnother = async () => {
    try {
      loading = true;
      await authLastUsedFlow.authenticate(selectedIdentity);
      authenticationV2Funnel.trigger(AuthenticationV2Events.UseAnother);
      await goto("/authorize/account");
    } catch (error) {
      loading = false;
      handleError(error);
    }
  };

  $effect(() => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsScreen);
  });
</script>

<div class="flex flex-1 flex-col">
  <AuthorizeHeader origin={$authorizationContextStore.requestOrigin} />
  <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
    Choose account
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">
    <span>for</span>
    {#if nonNullish(dapp?.name)}
      <span><b>{dapp.name}</b></span>
    {:else}
      <span>this app</span>
    {/if}
  </p>
  <div class="flex flex-col items-stretch gap-1.5 self-stretch">
    <ul class="contents">
      {#each lastUsedAccounts as account}
        <li class="contents">
          <ButtonCard
            onclick={() => handleContinueAs(account)}
            disabled={loading}
          >
            <Avatar size="sm" aria-hidden>
              {(account.name ?? "Primary account").slice(0, 1).toUpperCase()}
            </Avatar>
            <span class="overflow-hidden overflow-ellipsis whitespace-nowrap">
              {account.name ?? "Primary account"}
            </span>
          </ButtonCard>
        </li>
      {/each}
    </ul>
    <ButtonCard onclick={handleUseAnother} disabled={loading}>
      <FeaturedIcon size="sm">
        <PlusIcon size="1.25rem" />
      </FeaturedIcon>
      <span>Create or use another account</span>
    </ButtonCard>
  </div>
</div>
{#if authLastUsedFlow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}
