<script lang="ts">
  import type { PageProps } from "./$types";
  import { formatRelative, formatDate, t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import {
    PlusIcon,
    EllipsisVerticalIcon,
    PencilIcon,
    Trash2Icon,
    Link2OffIcon,
  } from "@lucide/svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { getMetadataString, openIdLogo, openIdName } from "$lib/utils/openID";
  import { nanosToMillis } from "$lib/utils/time";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import type {
    AuthnMethodData,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";
  import Select from "$lib/components/ui/Select.svelte";
  import RenamePasskey from "$lib/components/views/RenamePasskey.svelte";
  import RemovePasskey from "$lib/components/views/RemovePasskey.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { throwCanisterError } from "$lib/utils/utils";
  import {
    authnMethodEqual,
    authnMethodToPublicKey,
  } from "$lib/utils/webAuthn";
  import { goto, invalidateAll } from "$app/navigation";
  import { AddAccessMethodWizard } from "$lib/components/wizards/addAccessMethod";
  import { flip } from "svelte/animate";
  import { scale } from "svelte/transition";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { bytesToHex } from "@noble/hashes/utils";
  import { Principal } from "@icp-sdk/core/principal";
  import { OpenIdDelegationIdentity } from "$lib/utils/authentication";
  import UnlinkOpenIdCredential from "$lib/components/views/UnlinkOpenIdCredential.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import { get } from "svelte/store";

  const MAX_PASSKEYS = 8;

  type AccessMethod =
    | { type: "passkey"; data: AuthnMethodData }
    | { type: "openid"; data: OpenIdCredential };

  const { data }: PageProps = $props();

  let isAddAccessMethodWizardOpen = $state(false);
  let renamablePasskey = $state<AuthnMethodData>();
  let removablePasskey = $state<AuthnMethodData>();
  let unlinkableOpenIdCredential = $state<OpenIdCredential>();

  const sortAccessMethods = (a: AccessMethod, b: AccessMethod) => {
    const aVal =
      a.type === "passkey"
        ? a.data.last_authentication[0]
        : a.data.last_usage_timestamp[0];
    const bVal =
      b.type === "passkey"
        ? b.data.last_authentication[0]
        : b.data.last_usage_timestamp[0];
    // If items are equal (either undefined or same timestamp),
    // the OpenID items should come first before the passkeys.
    if (aVal === bVal) {
      if (a.type === b.type) return 0;
      return a.type === "openid" ? -1 : 1;
    }
    // Undefined should come first (new/unused access methods at the top),
    // defined should sort descending (most recently used first).
    if (isNullish(bVal)) return 1;
    if (isNullish(aVal)) return -1;
    return bVal > aVal ? 1 : bVal < aVal ? -1 : 0;
  };
  const accessMethodKey = (accessMethod: AccessMethod) =>
    accessMethod.type +
    (accessMethod.type === "passkey"
      ? bytesToHex(authnMethodToPublicKey(accessMethod.data))
      : accessMethod.data.sub);
  const passkeyInUse = (data: AuthnMethodData) =>
    $authenticatedStore.identity.getPrincipal().toText() ===
    Principal.selfAuthenticating(authnMethodToPublicKey(data)).toText();
  const openIdInUse = (credential: OpenIdCredential) =>
    $authenticatedStore.identity instanceof OpenIdDelegationIdentity &&
    $authenticatedStore.identity.iss === credential.iss &&
    $authenticatedStore.identity.sub === credential.sub;

  let accessMethods = $derived(
    [
      // Reverse to put the latest unused first
      ...(data.identityInfo.openid_credentials[0] ?? [])
        .map((data) => ({ type: "openid", data }) as const)
        .reverse(),
      ...data.identityInfo.authn_methods
        .map((data) => ({ type: "passkey", data }) as const)
        .reverse(),
    ].sort(sortAccessMethods),
  );
  const isUsingPasskeys = $derived(
    accessMethods.some(({ type }) => type === "passkey"),
  );
  const maxPasskeysReached = $derived(
    accessMethods.filter(({ type }) => type === "passkey").length >=
      MAX_PASSKEYS,
  );
  const openIdCredentials = $derived(
    accessMethods
      .filter((accessMethod) => accessMethod.type === "openid")
      .map(({ data }) => data),
  );

  const handleOpenIdLinked = async (data: OpenIdCredential) => {
    isAddAccessMethodWizardOpen = false;
    accessMethods = [{ type: "openid", data } as const, ...accessMethods].sort(
      sortAccessMethods,
    );
    void invalidateAll();
  };
  const handlePasskeyRegistered = async (data: AuthnMethodData) => {
    isAddAccessMethodWizardOpen = false;
    accessMethods = [{ type: "passkey", data } as const, ...accessMethods].sort(
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
          accessMethod.type === "passkey" &&
          authnMethodEqual(accessMethod.data, passkey)
            ? ({ type: "passkey", data: passkey } as const)
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
            accessMethod.type !== "passkey" ||
            !authnMethodEqual(accessMethod.data, passkey),
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
            accessMethod.type !== "openid" ||
            accessMethod.data.iss !== credential.iss ||
            accessMethod.data.sub !== credential.sub,
        )
        .sort(sortAccessMethods);
      void invalidateAll();
    } catch (error) {
      handleError(error);
    }
  };
  const logoutAndForgetIdentity = () => {
    lastUsedIdentitiesStore.removeIdentity(
      get(authenticatedStore).identityNumber,
    );
    location.replace("/login");
  };
</script>

{#snippet passkeyListItem(item: AuthnMethodData)}
  <div class="mb-3 flex h-9 flex-row items-center justify-between">
    <div class="relative">
      <PasskeyIcon class="text-fg-primary size-6" />
      {#if passkeyInUse(item)}
        <div
          class="bg-bg-success-secondary border-bg-primary absolute top-0 -right-0.25 size-2.5 rounded-full border-2"
        ></div>
      {/if}
    </div>
    <Select
      options={[
        {
          label: $t`Rename`,
          icon: PencilIcon,
          onClick: () => (renamablePasskey = item),
        },
        ...(accessMethods.length > 1
          ? [
              {
                label: $t`Remove`,
                icon: Trash2Icon,
                onClick: () => (removablePasskey = item),
              },
            ]
          : []),
      ]}
      align="end"
    >
      <Button variant="tertiary" size="sm" iconOnly>
        <EllipsisVerticalIcon class="size-5" />
      </Button>
    </Select>
  </div>
  <div class="text-text-primary mb-1 text-base font-semibold">
    {getMetadataString(item.metadata, "alias")}
  </div>
  <div class="text-text-tertiary text-sm">
    {$t`Passkey`}
  </div>
  <div class="border-border-tertiary my-5 border-t"></div>
  <div class="mb-4 flex flex-row">
    <div class="flex flex-1 flex-col gap-1">
      <div class="text-text-primary text-xs font-semibold">
        {$t`Last used`}
      </div>
      <div class="text-text-primary cursor-default text-xs">
        {#if passkeyInUse(item)}
          <Tooltip
            label={$t`Currently signed in with this passkey`}
            direction="up"
            align="start"
          >
            <span>{$t`Right now`}</span>
          </Tooltip>
        {:else if nonNullish(item.last_authentication[0])}
          {@const date = new Date(nanosToMillis(item.last_authentication[0]))}
          <Tooltip
            label={$formatDate(date, {
              timeStyle: "short",
              dateStyle: "medium",
            })}
            direction="up"
            align="start"
          >
            <span>{$formatRelative(date, { style: "long" })}</span>
          </Tooltip>
        {:else}
          <Tooltip
            label={$t`Has not been used yet`}
            direction="up"
            align="start"
          >
            <span>{$t`n/a`}</span>
          </Tooltip>
        {/if}
      </div>
    </div>
  </div>
  <div class="text-text-primary text-xs">
    {$t`Stored securely on your device, in your password manager, or on a security key.`}
  </div>
{/snippet}

{#snippet openIdListItem(item: OpenIdCredential)}
  {@const name = openIdName(item.iss, item.metadata)}
  {@const email = getMetadataString(item.metadata, "email")}

  <div class="mb-3 flex h-9 flex-row items-center justify-between">
    <div class="text-fg-primary relative size-6">
      {@html openIdLogo(item.iss, item.metadata)}
      {#if openIdInUse(item)}
        <div
          class="bg-bg-success-secondary border-bg-primary absolute -top-0.25 -right-0.5 size-2.5 rounded-full border-2"
        ></div>
      {/if}
    </div>
    {#if accessMethods.length > 1}
      <Select
        options={[
          {
            label: $t`Unlink`,
            icon: Link2OffIcon,
            onClick: () => (unlinkableOpenIdCredential = item),
          },
        ]}
        align="end"
      >
        <Button variant="tertiary" size="sm" iconOnly>
          <EllipsisVerticalIcon class="size-5" />
        </Button>
      </Select>
    {/if}
  </div>
  <div class="text-text-primary mb-1 text-base font-semibold">
    {$t`${name} account`}
  </div>
  <div class="text-text-tertiary text-sm">
    {email ?? $t`Hidden email`}
  </div>
  <div class="border-border-tertiary my-5 border-t"></div>
  <div class="mb-4 flex flex-row">
    <div class="flex flex-1 flex-col gap-1">
      <div class="text-text-primary text-xs font-semibold">
        {$t`Last used`}
      </div>
      <div class="text-text-primary cursor-default text-xs">
        {#if openIdInUse(item)}
          <Tooltip
            label={$t`Currently signed in with this account`}
            direction="up"
            align="start"
          >
            <span>{$t`Right now`}</span>
          </Tooltip>
        {:else if nonNullish(item.last_usage_timestamp[0])}
          {@const date = new Date(nanosToMillis(item.last_usage_timestamp[0]))}
          <Tooltip
            label={$formatDate(date, {
              timeStyle: "short",
              dateStyle: "medium",
            })}
            direction="up"
            align="start"
          >
            <span>{$formatRelative(date, { style: "long" })}</span>
          </Tooltip>
        {:else}
          <Tooltip
            label={$t`Has not been used yet`}
            direction="up"
            align="start"
          >
            <span>{$t`n/a`}</span>
          </Tooltip>
        {/if}
      </div>
    </div>
  </div>
  <div class="text-text-primary text-xs">
    {$t`Sign in with your ${name} account from any device.`}
  </div>
{/snippet}

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
  {#key data.identityNumber}
    {#each accessMethods as accessMethod (accessMethodKey(accessMethod))}
      <div
        animate:flip={{ duration: 300, delay: 300 }}
        in:scale={{ duration: 300, delay: 500, start: 0.95 }}
        out:scale={{ duration: 300, delay: 300, start: 0.95 }}
        class={[
          "flex flex-col",
          "bg-bg-primary border-border-secondary rounded-sm p-6 not-dark:shadow-sm dark:border",
        ]}
      >
        {#if accessMethod.type === "passkey"}
          {@render passkeyListItem(accessMethod.data)}
        {:else if accessMethod.type === "openid"}
          {@render openIdListItem(accessMethod.data)}
        {/if}
      </div>
    {/each}
  {/key}
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
      name={getMetadataString(renamablePasskey.metadata, "alias") ?? ""}
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
