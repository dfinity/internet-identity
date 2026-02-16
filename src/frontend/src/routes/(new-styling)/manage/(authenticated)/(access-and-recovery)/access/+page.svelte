<script lang="ts">
  import type { PageProps } from "./$types";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { PlusIcon } from "@lucide/svelte";
  import { openIdName } from "$lib/utils/openID";
  import type {
    AuthnMethodData,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";
  import RenamePasskey from "./components/RenamePasskey.svelte";
  import RemovePasskey from "./components/RemovePasskey.svelte";
  import RemoveOpenIdCredential from "./components/RemoveOpenIdCredential.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { throwCanisterError } from "$lib/utils/utils";
  import {
    authnMethodToPublicKey,
    getAuthnMethodAlias,
  } from "$lib/utils/webAuthn";
  import { nanosToMillis } from "$lib/utils/time";
  import { goto, invalidateAll } from "$app/navigation";
  import { AddAccessMethodWizard } from "$lib/components/wizards/addAccessMethod";
  import { flip } from "svelte/animate";
  import { scale } from "svelte/transition";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import OpenIdItem from "./components/OpenIdItem.svelte";
  import PasskeyItem from "./components/PasskeyItem.svelte";
  import {
    compareAccessMethods,
    toAccessMethods,
    toKey,
    isCurrentAccessMethod,
  } from "./utils";
  import { sessionStore } from "$lib/stores/session.store";
  import { page } from "$app/state";
  import { nonNullish } from "@dfinity/utils";
  import { ConfirmAccessMethodWizard } from "$lib/components/wizards/confirmAccessMethod";
  import { toaster } from "$lib/components/utils/toaster";

  const MAX_PASSKEYS = 16;

  const { data }: PageProps = $props();

  // State
  let isAddingAccessMethod = $state(false);
  let renamingAccessMethodKey = $state<string>();
  let removingAccessMethodKey = $state<string>();
  let accessMethods = $derived(toAccessMethods(data.identityInfo));
  let pendingRegistrationId = $state(data.pendingRegistrationId);

  // Derived
  const renamingAccessMethod = $derived(
    accessMethods.find((m) => renamingAccessMethodKey === toKey(m)),
  );
  const removingAccessMethod = $derived(
    accessMethods.find((m) => removingAccessMethodKey === toKey(m)),
  );
  const isUsingPasskeys = $derived(accessMethods.some((m) => "passkey" in m));
  const maxPasskeysReached = $derived(
    accessMethods.filter((m) => "passkey" in m).length >= MAX_PASSKEYS,
  );
  const openIdCredentials = $derived(
    accessMethods.filter((m) => "openid" in m).map(({ openid }) => openid),
  );

  // Handlers
  const handleOpenIdLinked = async (openid: OpenIdCredential) => {
    isAddingAccessMethod = false;
    accessMethods = [{ openid } as const, ...accessMethods].sort(
      compareAccessMethods,
    );
    lastUsedIdentitiesStore.addLastUsedIdentityIfMissing({
      identityNumber: data.identityNumber,
      name: data.identityInfo.name[0],
      createdAtMillis: nonNullish(data.identityInfo.created_at[0])
        ? nanosToMillis(data.identityInfo.created_at[0])
        : undefined,
      authMethod: {
        openid: {
          iss: openid.iss,
          sub: openid.sub,
          metadata: openid.metadata,
        },
      },
    });
    void invalidateAll();
  };
  const handlePasskeyRegistered = async (passkey: AuthnMethodData) => {
    isAddingAccessMethod = false;
    accessMethods = [{ passkey } as const, ...accessMethods].sort(
      compareAccessMethods,
    );

    if ("WebAuthn" in passkey.authn_method) {
      lastUsedIdentitiesStore.addLastUsedIdentityIfMissing({
        identityNumber: data.identityNumber,
        name: data.identityInfo.name[0],
        createdAtMillis:
          data.identityInfo.created_at.length > 0 &&
          nonNullish(data.identityInfo.created_at[0])
            ? nanosToMillis(data.identityInfo.created_at[0])
            : undefined,
        authMethod: {
          passkey: {
            credentialId: new Uint8Array(
              passkey.authn_method.WebAuthn.credential_id,
            ),
          },
        },
      });
    }

    void invalidateAll();
  };
  const handleOtherDeviceRegistered = async () => {
    isAddingAccessMethod = false;
    void invalidateAll();
  };
  const handleOtherDeviceConfirmed = async () => {
    toaster.success({
      title: $t`Passkey has been registered from another device.`,
    });
    pendingRegistrationId = null;
    // Remove searchParam and update state
    void goto(page.url.pathname, { replaceState: true, invalidateAll: true });
  };

  const handleNameChanged = async (name: string) => {
    if (
      renamingAccessMethod === undefined ||
      !("passkey" in renamingAccessMethod)
    ) {
      return;
    }
    try {
      // Replace passkey metadata
      const metadata = Object.fromEntries(
        renamingAccessMethod.passkey.metadata,
      );
      metadata["alias"] = { String: name };
      await $authenticatedStore.actor
        .authn_method_metadata_replace(
          $authenticatedStore.identityNumber,
          authnMethodToPublicKey(renamingAccessMethod.passkey),
          Object.entries(metadata),
        )
        .then(throwCanisterError);
      // Optimistic update
      renamingAccessMethod.passkey.metadata = Object.entries(metadata);
      accessMethods = [...accessMethods];
      // Close dialog and update
      renamingAccessMethodKey = undefined;
      void invalidateAll();
    } catch (error) {
      handleError(error);
    }
  };
  const handleRemoveConfirmed = async () => {
    if (removingAccessMethod === undefined) {
      return;
    }
    try {
      if ("passkey" in removingAccessMethod) {
        await $authenticatedStore.actor
          .authn_method_remove(
            $authenticatedStore.identityNumber,
            authnMethodToPublicKey(removingAccessMethod.passkey),
          )
          .then(throwCanisterError);
      }
      if ("openid" in removingAccessMethod) {
        await $authenticatedStore.actor
          .openid_credential_remove($authenticatedStore.identityNumber, [
            removingAccessMethod.openid.iss,
            removingAccessMethod.openid.sub,
          ])
          .then(throwCanisterError);
      }
      // Logout and forget identity if it's the current access method
      if (isCurrentAccessMethod($authenticatedStore, removingAccessMethod)) {
        lastUsedIdentitiesStore.removeIdentity(
          $authenticatedStore.identityNumber,
        );
        await sessionStore.reset();
        location.replace("/login");
        return;
      }
      // Optimistic update
      accessMethods = accessMethods
        .filter((m) => toKey(m) !== removingAccessMethodKey)
        .sort(compareAccessMethods);
      // Close dialog and fetch update
      removingAccessMethodKey = undefined;
      void invalidateAll();
    } catch (error) {
      handleError(error);
    }
  };
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">{$t`Access methods`}</h1>
  <p class="text-text-tertiary text-base">
    <Trans>Add or remove the ways you can sign in with your identity.</Trans>
  </p>
</header>
<div
  class="mt-10 grid grid-cols-[repeat(auto-fill,minmax(min(100%,16rem),1fr))] gap-5"
>
  <button
    onclick={() => (isAddingAccessMethod = true)}
    class={[
      "flex flex-col items-center justify-center gap-2",
      "bg-bg-primary border-border-tertiary rounded-sm border transition-colors duration-200 outline-none",
      "hover:border-border-secondary hover:bg-bg-primary_hover",
      "min-h-27 max-sm:h-27",
    ]}
  >
    <PlusIcon class="text-fg-secondary size-5" />
    <span class="text-text-primary text-sm font-semibold">{$t`Add new`}</span>
  </button>
  <ul class="contents">
    {#key data.identityNumber}
      {#each accessMethods as accessMethod (toKey(accessMethod))}
        <li
          animate:flip={{ duration: 300, delay: 300 }}
          in:scale={{ duration: 300, delay: 500, start: 0.95 }}
          out:scale={{ duration: 300, delay: 300, start: 0.95 }}
          class={[
            "flex flex-col",
            "bg-bg-primary border-border-secondary rounded-sm p-6 not-dark:shadow-sm dark:border",
          ]}
        >
          {#if "passkey" in accessMethod}
            <PasskeyItem
              passkey={accessMethod.passkey}
              onRename={() => (renamingAccessMethodKey = toKey(accessMethod))}
              onRemove={accessMethods.length > 1
                ? () => (removingAccessMethodKey = toKey(accessMethod))
                : undefined}
              isCurrentAccessMethod={isCurrentAccessMethod(
                $authenticatedStore,
                accessMethod,
              )}
            />
          {:else if "openid" in accessMethod}
            <OpenIdItem
              openid={accessMethod.openid}
              onUnlink={accessMethods.length > 1
                ? () => (removingAccessMethodKey = toKey(accessMethod))
                : undefined}
              isCurrentAccessMethod={isCurrentAccessMethod(
                $authenticatedStore,
                accessMethod,
              )}
            />
          {/if}
        </li>
      {/each}
    {/key}
  </ul>
</div>

{#if isAddingAccessMethod}
  <Dialog onClose={() => (isAddingAccessMethod = false)}>
    <AddAccessMethodWizard
      onOpenIdLinked={handleOpenIdLinked}
      onPasskeyRegistered={handlePasskeyRegistered}
      onOtherDeviceRegistered={handleOtherDeviceRegistered}
      onError={(error) => {
        isAddingAccessMethod = false;
        handleError(error);
      }}
      {maxPasskeysReached}
      {isUsingPasskeys}
      {openIdCredentials}
      identityName={data.identityInfo.name[0]}
    />
  </Dialog>
{/if}

{#if renamingAccessMethod !== undefined && "passkey" in renamingAccessMethod}
  <Dialog onClose={() => (renamingAccessMethodKey = undefined)}>
    <RenamePasskey
      passkey={renamingAccessMethod.passkey}
      onRename={handleNameChanged}
      onCancel={() => (renamingAccessMethodKey = undefined)}
    />
  </Dialog>
{/if}

{#if removingAccessMethod !== undefined}
  <Dialog onClose={() => (removingAccessMethodKey = undefined)}>
    {#if "passkey" in removingAccessMethod}
      <RemovePasskey
        onRemove={handleRemoveConfirmed}
        onCancel={() => (removingAccessMethodKey = undefined)}
        isCurrentAccessMethod={isCurrentAccessMethod(
          $authenticatedStore,
          removingAccessMethod,
        )}
      />
    {/if}
    {#if "openid" in removingAccessMethod}
      <RemoveOpenIdCredential
        onRemove={handleRemoveConfirmed}
        onCancel={() => (removingAccessMethodKey = undefined)}
        providerName={openIdName(
          removingAccessMethod.openid.iss,
          removingAccessMethod.openid.metadata,
        ) ?? $t`Unknown`}
        isCurrentAccessMethod={isCurrentAccessMethod(
          $authenticatedStore,
          removingAccessMethod,
        )}
      />
    {/if}
  </Dialog>
{/if}

{#if nonNullish(pendingRegistrationId)}
  <Dialog onClose={() => (pendingRegistrationId = null)}>
    <ConfirmAccessMethodWizard
      registrationId={pendingRegistrationId}
      onConfirm={handleOtherDeviceConfirmed}
      onError={(error) => {
        pendingRegistrationId = null;
        handleError(error);
        goto(page.url.pathname, { replaceState: true }); // Remove searchParam
      }}
    />
  </Dialog>
{/if}
