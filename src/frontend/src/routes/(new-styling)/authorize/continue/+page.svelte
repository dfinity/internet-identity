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
  import { untrack } from "svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import { nonNullish } from "@dfinity/utils";
  import { getDapps } from "$lib/flows/dappsExplorer/dapps";
  import { currentIdentityNumberStore } from "$lib/stores/current-identity.store";

  const dapps = getDapps();
  const dapp = $derived(
    dapps.find((dapp) =>
      dapp.hasOrigin($authorizationContextStore.requestOrigin),
    ),
  );
  const currentIdentity = $derived(
    $lastUsedIdentitiesStore[`${$currentIdentityNumberStore!}`],
  );
  const lastUsedAccounts = $derived.by<LastUsedAccount[]>(() => {
    const accounts = Object.values(
      currentIdentity.accounts?.[$authorizationContextStore.effectiveOrigin] ??
        {},
    ).sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis);
    if (accounts.length === 0) {
      return [
        {
          identityNumber: currentIdentity.identityNumber,
          origin: $authorizationContextStore.effectiveOrigin,
          accountNumber: undefined,
          lastUsedTimestampMillis: currentIdentity.lastUsedTimestampMillis,
        },
      ];
    }
    return accounts;
  });
  let loading = $state(false);
  let systemOverlay = $state(false);

  const authenticateCurrentIdentity = async () => {
    if ("passkey" in currentIdentity.authMethod) {
      const { identity, identityNumber } = await authenticateWithPasskey({
        canisterId,
        session: $sessionStore,
        credentialId: currentIdentity.authMethod.passkey.credentialId,
      });
      authenticationStore.set({ identity, identityNumber });
      lastUsedIdentitiesStore.addLastUsedIdentity(currentIdentity);
    } else if (
      "openid" in currentIdentity.authMethod &&
      currentIdentity.authMethod.openid.iss === "https://accounts.google.com"
    ) {
      systemOverlay = true;
      const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id!;
      const requestConfig = createGoogleRequestConfig(clientId);
      const jwt = await requestJWT(requestConfig, {
        nonce: $sessionStore.nonce,
        mediation: "required",
        loginHint: currentIdentity.authMethod.openid.sub,
      });
      systemOverlay = false;
      const { identity, identityNumber } = await authenticateWithJWT({
        canisterId,
        session: $sessionStore,
        jwt,
      });
      authenticationStore.set({ identity, identityNumber });
      lastUsedIdentitiesStore.addLastUsedIdentity(currentIdentity);
    } else {
      throw new Error("Unrecognized authentication method");
    }
  };

  const continueAs = async (account: LastUsedAccount) => {
    try {
      loading = true;
      await authenticateCurrentIdentity();
      if ("passkey" in currentIdentity.authMethod) {
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.ContinueAsPasskey,
        );
      } else if ("openid" in currentIdentity.authMethod) {
        authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsGoogle);
      }
      lastUsedIdentitiesStore.addLastUsedAccount(account);
      await authorizationStore.authorize(account.accountNumber);
    } catch (error) {
      systemOverlay = false;
      loading = false;
      handleError(error);
    }
  };

  const useAnother = async () => {
    try {
      loading = true;
      await authenticateCurrentIdentity();
      authenticationV2Funnel.trigger(AuthenticationV2Events.UseAnother);
      await goto("/authorize/account");
    } catch (error) {
      systemOverlay = false;
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
    Sign in
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">
    <span>Choose</span>
    {#if nonNullish(dapp?.name)}
      <span>your <b>{dapp.name}</b> account</span>
    {:else}
      <span>from your accounts with this app</span>
    {/if}
  </p>
  <div class="flex flex-col items-stretch gap-1.5 self-stretch">
    <ul class="contents">
      {#each lastUsedAccounts as account}
        <li class="contents">
          <ButtonCard onclick={() => continueAs(account)} disabled={loading}>
            <Avatar size="sm" aria-hidden>
              {account.name?.slice(0, 1).toUpperCase() ?? "A"}
            </Avatar>
            <span class="overflow-hidden overflow-ellipsis whitespace-nowrap">
              {account.name ?? "Primary account"}
            </span>
          </ButtonCard>
        </li>
      {/each}
    </ul>
    <ButtonCard onclick={useAnother} disabled={loading}>
      <FeaturedIcon size="sm">
        <PlusIcon size="1.25rem" />
      </FeaturedIcon>
      <span>Create or use another account</span>
    </ButtonCard>
  </div>
</div>
{#if systemOverlay}
  <SystemOverlayBackdrop />
{/if}
