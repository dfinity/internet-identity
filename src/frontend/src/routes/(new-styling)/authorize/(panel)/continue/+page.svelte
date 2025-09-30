<script lang="ts">
  import { goto } from "$app/navigation";
  import {
    lastUsedIdentitiesStore,
    type LastUsedAccount,
  } from "$lib/stores/last-used-identities.store";
  import {
    authorizationStore,
    authorizationContextStore,
  } from "$lib/stores/authorization.store";
  import { HelpCircleIcon, PlusIcon } from "@lucide/svelte";
  import ButtonCard from "$lib/components/ui/ButtonCard.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { handleError } from "$lib/components/utils/error";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { get } from "svelte/store";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";
  import { nonNullish } from "@dfinity/utils";
  import { getDapps } from "$lib/legacy/flows/dappsExplorer/dapps";
  import { AuthLastUsedFlow } from "$lib/flows/authLastUsedFlow.svelte";
  import { findConfig, isOpenIdConfig } from "$lib/utils/openID";
  import { AUTH_FLOW_UPDATES } from "$lib/state/featureFlags";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { throwCanisterError } from "$lib/utils/utils";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import Popover from "$lib/components/ui/Popover.svelte";
  import Button from "$lib/components/ui/Button.svelte";

  let toggled = $state<boolean>(false);
  let showPrivacyPopover = $state<boolean>(false);
  let popoverAnchorRef = $state<HTMLDivElement>();

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
  // Initialize the flow every time the last used identities change
  $effect(() =>
    authLastUsedFlow.init(
      lastUsedAccounts.map(({ identityNumber }) => identityNumber),
    ),
  );
  let loading = $state(false);

  const handleContinueAs = async (account?: LastUsedAccount) => {
    try {
      loading = true;
      await authLastUsedFlow.authenticate(selectedIdentity);
      if ("passkey" in selectedIdentity.authMethod) {
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.ContinueAsPasskey,
        );
      } else if ("openid" in selectedIdentity.authMethod) {
        const config = findConfig(
          selectedIdentity.authMethod.openid.iss,
          selectedIdentity.authMethod.openid.metadata ?? [],
        );
        if (nonNullish(config) && isOpenIdConfig(config)) {
          authenticationV2Funnel.addProperties({
            provider: config.name,
          });
        }
        authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsOpenID);
      }
      if (account) {
        lastUsedIdentitiesStore.addLastUsedAccount(account);
        await authorizationStore.authorize(account.accountNumber);
      } else {
        const { identityNumber, actor } = get(authenticatedStore);
        const { effectiveOrigin } = get(authorizationContextStore);

        const defaultAccount = await actor
          .get_default_account(identityNumber, effectiveOrigin)
          .then(throwCanisterError);
        lastUsedIdentitiesStore.addLastUsedAccount({
          origin: effectiveOrigin,
          identityNumber,
          name: defaultAccount.name[0],
          accountNumber: defaultAccount.account_number[0],
        });
        await authorizationStore.authorize(defaultAccount.account_number[0]);
      }
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

  const handleEnableMultipleAccounts = async () => {
    try {
      toggled = true;
      await handleUseAnother();
    } catch {
      toggled = false;
    }
  };

  $effect(() => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsScreen);
  });
</script>

<div class="flex flex-1 flex-col">
  <AuthorizeHeader origin={$authorizationContextStore.requestOrigin} />
  <h1 class="text-text-primary mb-2 self-start text-2xl font-medium">
    {#if $AUTH_FLOW_UPDATES}
      Sign in
    {:else}
      Choose account
    {/if}
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">
    {#if $AUTH_FLOW_UPDATES}
      with your Internet Identity
    {:else}
      <span>for</span>
      {#if nonNullish(dapp?.name)}
        <span><b>{dapp.name}</b></span>
      {:else}
        <span>this app</span>
      {/if}
    {/if}
  </p>
  <div class="flex flex-col items-stretch gap-1.5 self-stretch">
    <ul class="contents">
      {#if $AUTH_FLOW_UPDATES}
        <li class="contents">
          <ButtonCard
            onclick={() => handleContinueAs()}
            disabled={loading}
            class="!justify-center"
          >
            <span class="overflow-hidden overflow-ellipsis whitespace-nowrap">
              Continue
            </span>
          </ButtonCard>
        </li>
      {:else}
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
      {/if}
    </ul>
    {#if $AUTH_FLOW_UPDATES}
      <div class="mt-4.5 flex flex-col gap-6">
        <div class="border-border-tertiary border-t"></div>
        <div class="flex flex-row items-center justify-between gap-4">
          <Toggle
            {toggled}
            onClick={handleEnableMultipleAccounts}
            label="Enable multiple accounts"
          ></Toggle>
          <div bind:this={popoverAnchorRef}>
            <Button
              variant="tertiary"
              onclick={() => (showPrivacyPopover = !showPrivacyPopover)}
            >
              <HelpCircleIcon
                size="20"
                class="text-text-primary stroke-fg-tertiary"
              />
            </Button>
          </div>
          {#if showPrivacyPopover}
            <Popover
              anchor={popoverAnchorRef}
              direction="up"
              align="end"
              distance="10px"
              class="gap-0.5"
              onClose={() => (showPrivacyPopover = false)}
            >
              <p class="text-text-primary text-xs font-semibold">
                Enable Multiple accounts
              </p>
              <p class="text-text-secondary text-xs font-medium">
                By enabling this feature, you can create more than one account
                for a single app. Easily switch between accounts (e.g. work,
                personal, or demo).
              </p>
            </Popover>
          {/if}
        </div>
      </div>
    {:else}
      <ButtonCard onclick={handleUseAnother} disabled={loading}>
        <FeaturedIcon size="sm">
          <PlusIcon size="1.25rem" />
        </FeaturedIcon>
        <span>Create or use another account</span>
      </ButtonCard>
    {/if}
  </div>
</div>
{#if authLastUsedFlow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}
