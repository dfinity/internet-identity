<script lang="ts">
  import type { PageProps } from "./$types";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import { PlusIcon } from "@lucide/svelte";
  import { openIdName } from "$lib/utils/openID";
  import type {
    AuthnMethodData,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";
  import RenamePasskey from "$lib/components/views/RenamePasskey.svelte";
  import RemovePasskey from "$lib/components/views/RemovePasskey.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { throwCanisterError } from "$lib/utils/utils";
  import {
    authnMethodEqual,
    authnMethodToCredentialId,
    authnMethodToPublicKey,
    getAuthnMethodAlias,
  } from "$lib/utils/webAuthn";
  import { invalidateAll } from "$app/navigation";
  import { AddAccessMethodWizard } from "$lib/components/wizards/addAccessMethod";
  import { flip } from "svelte/animate";
  import { scale } from "svelte/transition";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { bytesToHex } from "@noble/hashes/utils";
  import UnlinkOpenIdCredential from "$lib/components/views/UnlinkOpenIdCredential.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import OpenIdItem from "$lib/components/ui/OpenIdItem.svelte";
  import PasskeyItem from "$lib/components/ui/PasskeyItem.svelte";
  import { ConfirmAccessMethodWizard } from "$lib/components/wizards/confirmAccessMethod";
  import { toaster } from "$lib/components/utils/toaster";

  const MAX_PASSKEYS = 8;

  type AccessMethod =
    | { passkey: AuthnMethodData }
    | { openid: OpenIdCredential };

  const { data }: PageProps = $props();

  let isAddAccessMethodWizardOpen = $state(false);
  let renamablePasskey = $state<AuthnMethodData>();
  let removablePasskey = $state<AuthnMethodData>();
  let unlinkableOpenIdCredential = $state<OpenIdCredential>();
  let pendingRegistrationId = $state(data.pendingRegistrationId);

  const sortAccessMethods = (a: AccessMethod, b: AccessMethod) => {
    const aVal =
      "passkey" in a
        ? a.passkey.last_authentication[0]
        : a.openid.last_usage_timestamp[0];
    const bVal =
      "passkey" in b
        ? b.passkey.last_authentication[0]
        : b.openid.last_usage_timestamp[0];
    // If items are equal (either undefined or same timestamp),
    // the OpenID items should come first before the passkeys.
    if (aVal === bVal) {
      return "openid" in a ? -1 : 1;
    }
    // Undefined should come first (new/unused access methods at the top),
    // defined should sort descending (most recently used first).
    if (isNullish(bVal)) return 1;
    if (isNullish(aVal)) return -1;
    return bVal > aVal ? 1 : bVal < aVal ? -1 : 0;
  };
  const accessMethodKey = (accessMethod: AccessMethod) =>
    "passkey" in accessMethod
      ? bytesToHex(authnMethodToPublicKey(accessMethod.passkey))
      : accessMethod.openid.iss + accessMethod.openid.sub;
  const passkeyInUse = (data: AuthnMethodData) =>
    "passkey" in $authenticatedStore.authMethod &&
    bytesToHex($authenticatedStore.authMethod.passkey.credentialId) ===
      bytesToHex(authnMethodToCredentialId(data));
  const openIdInUse = (credential: OpenIdCredential) =>
    "openid" in $authenticatedStore.authMethod &&
    $authenticatedStore.authMethod.openid.iss === credential.iss &&
    $authenticatedStore.authMethod.openid.sub === credential.sub;

  let accessMethods = $derived(
    [
      // Reverse to put the latest unused first
      ...(data.identityInfo.openid_credentials[0] ?? [])
        .map((openid) => ({ openid }) as const)
        .reverse(),
      ...data.identityInfo.authn_methods
        // Filter out anything that isn't a passkey e.g. recovery phrase
        .filter((passkey) => "WebAuthn" in passkey.authn_method)
        .map((passkey) => ({ passkey }) as const)
        .reverse(),
    ].sort(sortAccessMethods),
  );
  const isUsingPasskeys = $derived(
    accessMethods.some((accessMethod) => "passkey" in accessMethod),
  );
  const maxPasskeysReached = $derived(
    accessMethods.filter((accessMethod) => "passkey" in accessMethod).length >=
      MAX_PASSKEYS,
  );
  const openIdCredentials = $derived(
    accessMethods
      .filter((accessMethod) => "openid" in accessMethod)
      .map(({ openid }) => openid),
  );

  const handleOpenIdLinked = async (openid: OpenIdCredential) => {
    isAddAccessMethodWizardOpen = false;
    accessMethods = [{ openid } as const, ...accessMethods].sort(
      sortAccessMethods,
    );
    void invalidateAll();
  };
  const handlePasskeyRegistered = async (passkey: AuthnMethodData) => {
    isAddAccessMethodWizardOpen = false;
    accessMethods = [{ passkey } as const, ...accessMethods].sort(
      sortAccessMethods,
    );
    void invalidateAll();
  };
  const handleOtherDeviceRegistered = async () => {
    isAddAccessMethodWizardOpen = false;
    void invalidateAll();
  };
  const handleRenamePasskey = async (name: string) => {
    if (isNullish(renamablePasskey)) {
      return;
    }
    try {
      const passkey = renamablePasskey;
      const metadata = Object.fromEntries(passkey.metadata);
      metadata["alias"] = { String: name };
      passkey.metadata = Object.entries(metadata);
      await $authenticatedStore.actor
        .authn_method_metadata_replace(
          $authenticatedStore.identityNumber,
          authnMethodToPublicKey(passkey),
          passkey.metadata,
        )
        .then(throwCanisterError);
      renamablePasskey = undefined;
      accessMethods = accessMethods
        .map((accessMethod) =>
          "passkey" in accessMethod &&
          authnMethodEqual(accessMethod.passkey, passkey)
            ? ({ passkey } as const)
            : accessMethod,
        )
        .sort(sortAccessMethods);
      void invalidateAll();
    } catch (error) {
      handleError(error);
    }
  };
  const handleRemovePasskey = async () => {
    if (isNullish(removablePasskey)) {
      return;
    }
    try {
      const passkey = removablePasskey;
      await $authenticatedStore.actor
        .authn_method_remove(
          $authenticatedStore.identityNumber,
          authnMethodToPublicKey(passkey),
        )
        .then(throwCanisterError);
      if (passkeyInUse(passkey)) {
        logoutAndForgetIdentity();
        return;
      }
      removablePasskey = undefined;
      accessMethods = accessMethods
        .filter(
          (accessMethod) =>
            !("passkey" in accessMethod) ||
            !authnMethodEqual(accessMethod.passkey, passkey),
        )
        .sort(sortAccessMethods);
      void invalidateAll();
    } catch (error) {
      handleError(error);
    }
  };
  const handleUnlinkOpenIdCredential = async () => {
    if (isNullish(unlinkableOpenIdCredential)) {
      return;
    }
    try {
      const credential = unlinkableOpenIdCredential;
      await $authenticatedStore.actor
        .openid_credential_remove($authenticatedStore.identityNumber, [
          credential.iss,
          credential.sub,
        ])
        .then(throwCanisterError);
      if (openIdInUse(credential)) {
        logoutAndForgetIdentity();
        return;
      }
      unlinkableOpenIdCredential = undefined;
      accessMethods = accessMethods
        .filter(
          (accessMethod) =>
            !("openid" in accessMethod) ||
            accessMethod.openid.iss !== credential.iss ||
            accessMethod.openid.sub !== credential.sub,
        )
        .sort(sortAccessMethods);
      void invalidateAll();
    } catch (error) {
      handleError(error);
    }
  };
  const handleConfirmAccessMethod = async () => {
    pendingRegistrationId = null;
    toaster.success({
      title: $t`Passkey has been registered from another device.`,
    });
    await invalidateAll();
  };
  const logoutAndForgetIdentity = () => {
    lastUsedIdentitiesStore.removeIdentity($authenticatedStore.identityNumber);
    location.replace("/login");
  };
</script>

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">{$t`Access methods`}</h1>
  <p class="text-text-tertiary text-base">
    <Trans>
      Overview of access methods (dates created, recent usage). Add a new method
      or remove them.
    </Trans>
  </p>
</header>
<div class="mt-10 grid grid-cols-[repeat(auto-fill,minmax(260px,1fr))] gap-5">
  <button
    onclick={() => (isAddAccessMethodWizardOpen = true)}
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
      {#each accessMethods as accessMethod (accessMethodKey(accessMethod))}
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
              onRename={() => (renamablePasskey = accessMethod.passkey)}
              onRemove={accessMethods.length > 1
                ? () => (removablePasskey = accessMethod.passkey)
                : undefined}
              inUse={passkeyInUse(accessMethod.passkey)}
            />
          {:else if "openid" in accessMethod}
            <OpenIdItem
              openid={accessMethod.openid}
              onUnlink={accessMethods.length > 1
                ? () => (unlinkableOpenIdCredential = accessMethod.openid)
                : undefined}
              inUse={openIdInUse(accessMethod.openid)}
            />
          {/if}
        </li>
      {/each}
    {/key}
  </ul>
</div>

{#if isAddAccessMethodWizardOpen}
  <Dialog onClose={() => (isAddAccessMethodWizardOpen = false)}>
    <AddAccessMethodWizard
      onOpenIdLinked={handleOpenIdLinked}
      onPasskeyRegistered={handlePasskeyRegistered}
      onOtherDeviceRegistered={handleOtherDeviceRegistered}
      onError={(error) => {
        isAddAccessMethodWizardOpen = false;
        handleError(error);
      }}
      {maxPasskeysReached}
      {isUsingPasskeys}
      {openIdCredentials}
    />
  </Dialog>
{/if}

{#if renamablePasskey}
  <Dialog onClose={() => (renamablePasskey = undefined)}>
    <RenamePasskey
      name={getAuthnMethodAlias(renamablePasskey)}
      onRename={handleRenamePasskey}
      onCancel={() => (renamablePasskey = undefined)}
    />
  </Dialog>
{/if}

{#if removablePasskey}
  <Dialog onClose={() => (removablePasskey = undefined)}>
    <RemovePasskey
      onRemove={handleRemovePasskey}
      onCancel={() => (removablePasskey = undefined)}
      isCurrentAccessMethod={passkeyInUse(removablePasskey)}
    />
  </Dialog>
{/if}

{#if unlinkableOpenIdCredential}
  <Dialog onClose={() => (unlinkableOpenIdCredential = undefined)}>
    <UnlinkOpenIdCredential
      onUnlink={handleUnlinkOpenIdCredential}
      onCancel={() => (unlinkableOpenIdCredential = undefined)}
      providerName={openIdName(
        unlinkableOpenIdCredential.iss,
        unlinkableOpenIdCredential.metadata,
      ) ?? $t`Unknown`}
      isCurrentAccessMethod={openIdInUse(unlinkableOpenIdCredential)}
    />
  </Dialog>
{/if}

{#if nonNullish(pendingRegistrationId)}
  <Dialog onClose={() => (pendingRegistrationId = null)}>
    <ConfirmAccessMethodWizard
      registrationId={pendingRegistrationId}
      onConfirm={handleConfirmAccessMethod}
      onError={(error) => {
        handleError(error);
        pendingRegistrationId = null;
      }}
    />
  </Dialog>
{/if}
