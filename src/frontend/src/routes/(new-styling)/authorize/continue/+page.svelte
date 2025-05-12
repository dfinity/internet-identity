<script lang="ts">
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { goto } from "$app/navigation";
  import {
    authenticateWithJWT,
    authenticateWithPasskey,
  } from "$lib/utils/authentication";
  import { isNullish } from "@dfinity/utils";
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
  import Badge from "$lib/components/ui/Badge.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import {
    ChevronDownIcon,
    UserIcon,
    PlusIcon,
    ArrowRightLeftIcon,
  } from "@lucide/svelte";
  import RadioCard from "$lib/components/ui/RadioCard.svelte";
  import Avatar from "$lib/components/ui/Avatar.svelte";
  import { handleError } from "../error";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Ellipsis from "$lib/components/utils/Ellipsis.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import DappLogo from "$lib/components/ui/DappLogo.svelte";

  const origin =
    "https://nns.ic0.app" || $authorizationContextStore.requestOrigin;

  let continueButtonRef = $state<HTMLElement>();
  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore)
      .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
      .slice(0, 3),
  );
  let selectedIdentity = $state.raw(lastUsedIdentities[0]);
  const lastUsedAccount = $derived(
    selectedIdentity.accounts?.[
      $authorizationContextStore.authRequest.derivationOrigin ??
        $authorizationContextStore.requestOrigin
    ],
  );
  let continueWith = $state<"lastUsedAccount" | "anotherAccount">(
    "lastUsedAccount",
  );
  let identitySwitcherVisible = $state(false);
  let loading = $state(false);

  const handleContinue = async () => {
    try {
      loading = true;
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
        const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id!;
        const requestConfig = createGoogleRequestConfig(clientId);
        const jwt = await requestJWT(requestConfig, {
          nonce: $sessionStore.nonce,
          mediation: "required",
          loginHint: selectedIdentity.authMethod.openid.sub,
        });
        const { identity, identityNumber, iss, sub } =
          await authenticateWithJWT({
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
        return handleError(new Error("Unrecognized authentication method"));
      }

      switch (continueWith) {
        case "lastUsedAccount":
          if (isNullish(lastUsedAccount)) {
            return handleError(new Error("Unreachable"));
          }
          lastUsedIdentitiesStore.addLastUsedAccount(lastUsedAccount);
          return authorizationStore.authorize(lastUsedAccount.accountNumber);
        case "anotherAccount":
          return goto("/authorize/account");
        default:
          void (continueWith satisfies never);
      }
    } catch (error) {
      loading = false;
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
    continueButtonRef?.focus();
  });
</script>

<div class="flex flex-col">
  <div class="flex flex-col items-center gap-6 py-8">
    <DappLogo origin={$authorizationContextStore.requestOrigin} />
    <Badge size="sm" class="max-w-full">
      <Ellipsis
        text={$authorizationContextStore.requestOrigin}
        position="middle"
      />
    </Badge>
  </div>
  <div class="mb-6 flex flex-col gap-2">
    <h1 class="text-gray-light-900 dark:text-gray-dark-25 text-2xl font-medium">
      Sign in
    </h1>
    <p class="text-gray-light-700 dark:text-gray-dark-50 text-sm">
      with Internet Identity
    </p>
  </div>
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
    <button
      onclick={() => selectOption("lastUsedAccount")}
      class="outline-none"
    >
      <RadioCard checked={continueWith === "lastUsedAccount"}>
        <Avatar size="sm">
          {lastUsedAccount?.name?.slice(0, 1).toUpperCase() ?? "A"}
        </Avatar>

        <span class="overflow-hidden overflow-ellipsis whitespace-nowrap">
          {lastUsedAccount?.name ?? "Primary Account"}
        </span>
      </RadioCard>
    </button>
    <button onclick={() => selectOption("anotherAccount")} class="outline-none">
      <RadioCard checked={continueWith === "anotherAccount"}>
        <FeaturedIcon size="sm">
          <ArrowRightLeftIcon size="1.25rem" />
        </FeaturedIcon>
        <span>Use another account</span>
      </RadioCard>
    </button>
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
{#if identitySwitcherVisible}
  <Dialog onClose={() => showIdentitySwitcher(false)}>
    <h1
      class="text-gray-light-900 dark:text-gray-dark-25 mb-8 text-2xl font-medium"
    >
      Switch Identity
    </h1>
    <div class="flex flex-col gap-1.5">
      {#each lastUsedIdentities as lastUsedIdentity}
        <button
          onclick={() => switchIdentity(lastUsedIdentity)}
          class="outline-none"
        >
          <RadioCard checked={lastUsedIdentity === selectedIdentity} checkIcon>
            <Avatar size="sm">
              <UserIcon size="1.25rem" />
            </Avatar>
            <span
              class="flex-1 overflow-hidden text-start text-ellipsis whitespace-nowrap"
            >
              {lastUsedIdentity.name ?? lastUsedIdentity.identityNumber}
            </span>
          </RadioCard>
        </button>
      {/each}
      <a href="/authorize" class="outline-none">
        <RadioCard>
          <FeaturedIcon size="sm">
            <PlusIcon size="1.25rem" />
          </FeaturedIcon>
          <span
            class="flex-1 overflow-hidden text-start text-ellipsis whitespace-nowrap"
          >
            Use another Internet Identity
          </span>
        </RadioCard>
      </a>
    </div>
  </Dialog>
{/if}
