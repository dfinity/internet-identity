<script lang="ts">
  import Dialog from "$lib/components/UI/Dialog.svelte";
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
  import { authorizationStore } from "$lib/stores/authorization.store";

  let continueButtonRef = $state<HTMLButtonElement>();
  const lastUsedIdentities = $derived(
    Object.values($lastUsedIdentitiesStore).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    ),
  );
  let selectedIdentity = $state.raw(lastUsedIdentities[0]);
  const lastUsedAccount = $derived(
    selectedIdentity.accounts?.[
      $authorizationStore.authRequest.derivationOrigin ??
        $authorizationStore.requestOrigin
    ],
  );
  let continueWith = $state<"lastUsedAccount" | "anotherAccount">(
    "lastUsedAccount",
  );
  let identitySwitcherVisible = $state(false);

  const handleContinue = async () => {
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

    switch (continueWith) {
      case "lastUsedAccount":
        if (isNullish(lastUsedAccount)) {
          // Unreachable, user shouldn't have been redirected to this page
          return;
        }
        lastUsedIdentitiesStore.addLastUsedAccount(lastUsedAccount);
        return authorizationStore.authorize(lastUsedAccount.accountNumber);
      case "anotherAccount":
        return goto("/authorize/account");
      default:
        void (continueWith satisfies never);
    }
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

<div class="flex flex-col items-start">
  <button
    onclick={() => (identitySwitcherVisible = !identitySwitcherVisible)}
    class="btn mb-3 self-start px-0 py-1 font-medium"
  >
    <span
      >{selectedIdentity.name ?? selectedIdentity.identityNumber}'s Internet
      Identity</span
    >
    <span class="-mt-2.5">⌄</span>
  </button>
  <div
    class="mb-6 flex flex-col items-stretch gap-3 self-stretch"
    role="radiogroup"
  >
    <button
      onclick={() => (continueWith = "lastUsedAccount")}
      class={[
        "btn box-border h-15 justify-start rounded-lg p-4 px-4 text-left transition-none",
        continueWith === "lastUsedAccount"
          ? "bg-surface-200-800 border-surface-0 border-2 font-semibold"
          : "preset-outlined-surface-300-700",
      ]}
      role="radio"
      aria-checked={continueWith === "lastUsedAccount"}
    >
      {lastUsedAccount?.name ?? "Primary account"}
    </button>
    <button
      onclick={() => (continueWith = "anotherAccount")}
      class={[
        "btn box-border h-15 justify-start rounded-lg p-4 px-4 text-left transition-none",
        continueWith === "anotherAccount"
          ? "bg-surface-200-800 border-surface-0 border-2 font-semibold"
          : "preset-outlined-surface-300-700",
      ]}
      role="radio"
      aria-checked={continueWith === "lastUsedAccount"}
    >
      Use another account
    </button>
  </div>
  <button
    bind:this={continueButtonRef}
    onclick={handleContinue}
    class="btn preset-filled self-stretch py-2">Continue</button
  >
</div>
{#if identitySwitcherVisible}
  <Dialog
    title={"Switch Internet Identity"}
    onClose={() => (identitySwitcherVisible = false)}
  >
    <div class="h-4"></div>
    {#each lastUsedIdentities as lastUsedIdentity}
      <button
        onclick={() => switchIdentity(lastUsedIdentity)}
        class="border-t-surface-100-900 text-surface-contrast-50-950/80 flex items-center border-t p-2 text-start"
      >
        <span class="flex-1"
          >{lastUsedIdentity.name ?? lastUsedIdentity.identityNumber}'s Internet
          Identity</span
        >
        {#if lastUsedIdentity === selectedIdentity}
          <span
            class="preset-filled size-5 rounded-full text-center text-xs leading-5 font-bold"
            >✓</span
          >
        {/if}
      </button>
    {/each}
    <a
      href="/authorize"
      class="border-y-surface-100-900 text-surface-contrast-50-950/80 border-y p-2 text-start"
      >Use another Internet Identity</a
    >
  </Dialog>
{/if}
