<script lang="ts">
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { goto } from "$app/navigation";
  import {
    authenticateWithJWT,
    authenticateWithPasskey,
  } from "$lib/utils/authentication";
  import {
    lastUsedIdentitiesStore,
    type LastUsedIdentity,
  } from "$lib/stores/last-used-identities.store";
  import { canisterConfig, canisterId } from "$lib/globals";
  import { sessionStore } from "$lib/stores/session.store";
  import { createGoogleRequestConfig, requestJWT } from "$lib/utils/openID";
  import {
    authenticatedStore,
    authenticationStore,
  } from "$lib/stores/authentication.store";
  import {
    authorizationStore,
    authorizationContextStore,
  } from "$lib/stores/authorization.store";
  import Button from "$lib/components/ui/Button.svelte";
  import {
    ChevronDownIcon,
    UserIcon,
    PlusIcon,
    ArrowRightLeftIcon,
  } from "@lucide/svelte";
  import RadioCard from "$lib/components/ui/RadioCard.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { handleError } from "$lib/components/utils/error";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import AuthorizeHeader from "$lib/components/ui/AuthorizeHeader.svelte";
  import { untrack } from "svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import {
    AuthenticationV2Events,
    authenticationV2Funnel,
  } from "$lib/utils/analytics/authenticationV2Funnel";

  let continueButtonRef = $state<HTMLElement>();
  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
  );
  let selectedIdentity = $state.raw(untrack(() => lastUsedIdentities[0]));
  const lastUsedAccount = $derived(
    selectedIdentity.accounts?.[$authorizationContextStore.effectiveOrigin],
  );
  let continueWith = $state<"lastUsedAccount" | "anotherAccount">(
    "lastUsedAccount",
  );
  let identitySwitcherVisible = $state(false);
  let loading = $state(false);
  let systemOverlay = $state(false);

  const authenticateLastUsed = async () => {
    if ("passkey" in selectedIdentity.authMethod) {
      const { identity, identityNumber, credentialId } =
        await authenticateWithPasskey({
          canisterId,
          session: $sessionStore,
          credentialId: selectedIdentity.authMethod.passkey.credentialId,
        });
      authenticationStore.set({ identity, identityNumber });
      const info =
        await $authenticatedStore.actor.get_anchor_info(identityNumber);
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod: { passkey: { credentialId } },
      });
    } else if (
      "openid" in selectedIdentity.authMethod &&
      selectedIdentity.authMethod.openid.iss === "https://accounts.google.com"
    ) {
      systemOverlay = true;
      const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id!;
      const requestConfig = createGoogleRequestConfig(clientId);
      const jwt = await requestJWT(requestConfig, {
        nonce: $sessionStore.nonce,
        mediation: "required",
        loginHint: selectedIdentity.authMethod.openid.sub,
      });
      systemOverlay = false;
      const { identity, identityNumber, iss, sub } = await authenticateWithJWT({
        canisterId,
        session: $sessionStore,
        jwt,
      });
      authenticationStore.set({ identity, identityNumber });
      const info =
        await $authenticatedStore.actor.get_anchor_info(identityNumber);
      lastUsedIdentitiesStore.addLastUsedIdentity({
        identityNumber,
        name: info.name[0],
        authMethod: { openid: { iss, sub } },
      });
    } else {
      throw new Error("Unrecognized authentication method");
    }
  };

  const handleContinue = async () => {
    try {
      loading = true;
      await authenticateLastUsed();

      switch (continueWith) {
        case "lastUsedAccount":
          if ("passkey" in selectedIdentity.authMethod) {
            authenticationV2Funnel.trigger(
              AuthenticationV2Events.ContinueAsPasskey,
            );
          } else if ("openid" in selectedIdentity.authMethod) {
            authenticationV2Funnel.trigger(
              AuthenticationV2Events.ContinueAsGoogle,
            );
          }
          lastUsedIdentitiesStore.addLastUsedAccount(
            lastUsedAccount ?? {
              identityNumber: selectedIdentity.identityNumber,
              accountNumber: undefined,
              origin: $authorizationContextStore.effectiveOrigin,
            },
          );
          return authorizationStore.authorize(lastUsedAccount?.accountNumber);
        case "anotherAccount":
          authenticationV2Funnel.trigger(AuthenticationV2Events.UseAnother);
          return goto("/authorize/account");
        default:
          void (continueWith satisfies never);
      }
    } catch (error) {
      loading = false;
      systemOverlay = false;
      handleError(error);
    }
  };

  const selectOption = (option: typeof continueWith) => {
    if (loading) {
      return;
    }
    continueWith = option;
  };

  const showIdentitySwitcher = (show: boolean) => {
    if (loading) {
      return;
    }
    identitySwitcherVisible = show;
  };

  const switchIdentity = (identity: LastUsedIdentity) => {
    identitySwitcherVisible = false;
    selectedIdentity = identity;
    continueWith = "lastUsedAccount";
  };

  $effect(() => {
    authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsScreen);
    continueButtonRef?.focus();
  });
</script>

<div class="flex flex-1 flex-col">
  <AuthorizeHeader
    origin={$authorizationContextStore.requestOrigin}
    class="mb-6"
  />
  <Button
    onclick={() => showIdentitySwitcher(true)}
    variant="tertiary"
    size="sm"
    class="mb-3 -ml-2 max-w-full self-start !px-2 not-disabled:hover:bg-transparent dark:not-disabled:hover:bg-transparent"
  >
    <Ellipsis
      text={`${selectedIdentity.name ?? String(selectedIdentity.identityNumber)}'s Identity`}
      position="middle"
    />
    <ChevronDownIcon size="1rem" class="shrink-0" />
  </Button>
  <div
    class="mb-6 flex flex-col items-stretch gap-1.5 self-stretch"
    role="radiogroup"
  >
    <RadioCard
      onclick={() => selectOption("lastUsedAccount")}
      checked={continueWith === "lastUsedAccount"}
      disabled={loading}
    >
      <Avatar size="sm">
        {lastUsedAccount?.name?.slice(0, 1).toUpperCase() ?? "A"}
      </Avatar>
      <span class="overflow-hidden overflow-ellipsis whitespace-nowrap">
        {lastUsedAccount?.name ?? "Primary Account"}
      </span>
    </RadioCard>
    <RadioCard
      onclick={() => selectOption("anotherAccount")}
      checked={continueWith === "anotherAccount"}
      disabled={loading}
    >
      <FeaturedIcon size="sm">
        <ArrowRightLeftIcon size="1.25rem" />
      </FeaturedIcon>
      <span>Use another account</span>
    </RadioCard>
  </div>
  <Button
    bind:element={continueButtonRef}
    onclick={handleContinue}
    size="xl"
    disabled={loading}
  >
    {#if loading}
      <ProgressRing />
      <span
        >{continueWith === "lastUsedAccount"
          ? "Signing in..."
          : "Authenticating..."}</span
      >
    {:else}
      <span>Continue</span>
    {/if}
  </Button>
</div>
{#if systemOverlay}
  <SystemOverlayBackdrop />
{/if}
{#if identitySwitcherVisible}
  <Dialog onClose={() => showIdentitySwitcher(false)}>
    <h1 class="text-text-primary mb-8 text-2xl font-medium">Switch Identity</h1>
    <div class="flex flex-col gap-1.5">
      {#each lastUsedIdentities as lastUsedIdentity}
        <RadioCard
          onclick={() => switchIdentity(lastUsedIdentity)}
          checked={lastUsedIdentity === selectedIdentity}
          checkIcon
        >
          <Avatar size="sm">
            <UserIcon size="1.25rem" />
          </Avatar>
          <span
            class="flex-1 overflow-hidden text-start text-ellipsis whitespace-nowrap"
          >
            {lastUsedIdentity.name ?? lastUsedIdentity.identityNumber}
          </span>
        </RadioCard>
      {/each}
      <RadioCard href="/authorize">
        <FeaturedIcon size="sm">
          <PlusIcon size="1.25rem" />
        </FeaturedIcon>
        <span
          class="flex-1 overflow-hidden text-start text-ellipsis whitespace-nowrap"
        >
          Use another Internet Identity
        </span>
      </RadioCard>
    </div>
  </Dialog>
{/if}
