<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { EditIcon, Link2OffIcon, Trash2Icon } from "@lucide/svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import AccessMethod from "$lib/components/ui/AccessMethod.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import RemoveOpenIdCredential from "$lib/components/views/RemoveOpenIdCredential.svelte";
  import type {
    AuthnMethodData,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";
  import RemovePasskeyDialog from "$lib/components/views/RemovePasskeyDialog.svelte";
  import RenamePasskeyDialog from "$lib/components/views/RenamePasskeyDialog.svelte";
  import { nonNullish } from "@dfinity/utils";
  import { handleError } from "$lib/components/utils/error";
  import { isWebAuthnMetaData } from "$lib/utils/accessMethods";
  import { authnMethodEqual, getAuthnMethodAlias } from "$lib/utils/webAuthn";

  interface Props {
    authnMethods: AuthnMethodData[];
    openIdCredentials?: OpenIdCredential[];
    lastUsedAccessMethod: any;
    isRemoveAccessMethodVisible: boolean;
  }

  const {
    authnMethods,
    openIdCredentials = [],
    lastUsedAccessMethod,
    isRemoveAccessMethodVisible,
  }: Props = $props();

  const isRemovableAuthnMethodCurrentAccessMethod = $derived(
    nonNullish(identityInfo.removableAuthnMethod) &&
      "WebAuthn" in identityInfo.removableAuthnMethod.authn_method &&
      identityInfo.isCurrentAccessMethod({
        passkey: {
          credentialId: new Uint8Array(
            identityInfo.removableAuthnMethod.authn_method.WebAuthn.credential_id,
          ),
        },
      }),
  );
  const isRemovableOpenIdCredentialCurrentAccessMethod = $derived(
    nonNullish(identityInfo.removableOpenIdCredential) &&
      identityInfo.isCurrentAccessMethod({
        openid: identityInfo.removableOpenIdCredential,
      }),
  );

  const handleRemoveOpenIdCredential = async () => {
    try {
      await identityInfo.removeGoogle();
    } catch (error) {
      handleError(error);
    }
  };
  const handleRemovePasskey = async () => {
    try {
      await identityInfo.removePasskey();
    } catch (error) {
      handleError(error);
    }
  };

  const handleRenamePasskey = async (newName: string) => {
    try {
      await identityInfo.renamePasskey(newName);
    } catch (error) {
      handleError(error);
    }
  };

  const isCurrentAccessMethod = (accessMethod: AuthnMethodData) => {
    return (
      nonNullish(lastUsedAccessMethod) &&
      isWebAuthnMetaData(lastUsedAccessMethod) &&
      authnMethodEqual(accessMethod, lastUsedAccessMethod)
    );
  };
</script>

<div
  class={`grid grid-cols-[min-content_1fr_min-content] grid-rows-[${identityInfo.totalAccessMethods}]`}
>
  {#each authnMethods as authnMethod}
    <div
      class="border-border-tertiary col-span-3 grid grid-cols-subgrid border-t py-4"
    >
      <div
        class="text-text-primary flex min-w-8 items-center justify-center px-4 pr-4"
      >
        <PasskeyIcon />
      </div>
      <AccessMethod
        accessMethod={authnMethod}
        isCurrent={isCurrentAccessMethod(authnMethod)}
      />
      <div class="flex items-center justify-end gap-2 px-4">
        <Button
          onclick={() => (identityInfo.renamableAuthnMethod = authnMethod)}
          variant="tertiary"
          iconOnly
          aria-label={`Rename ${isCurrentAccessMethod(authnMethod) ? "current" : ""} passkey`}
        >
          <EditIcon size="1.25rem" />
        </Button>
        {#if isRemoveAccessMethodVisible}
          <Button
            onclick={() => (identityInfo.removableAuthnMethod = authnMethod)}
            variant="tertiary"
            iconOnly
            aria-label={`Remove ${isCurrentAccessMethod(authnMethod) ? "current" : ""} passkey`}
            class="!text-fg-error-secondary"
          >
            <Trash2Icon size="1.25rem" />
          </Button>
        {/if}
      </div>
    </div>
  {/each}
  {#each openIdCredentials as credential}
    <div
      class="border-border-tertiary col-span-3 grid grid-cols-subgrid border-t py-4"
    >
      <div
        class="text-text-primary flex min-w-8 items-center justify-center px-4 pr-4"
      >
        <GoogleIcon />
      </div>

      <AccessMethod
        accessMethod={credential}
        isCurrent={nonNullish(lastUsedAccessMethod) &&
          !isWebAuthnMetaData(lastUsedAccessMethod) &&
          lastUsedAccessMethod.sub === credential.sub}
      />

      <div class="flex items-center justify-end px-4">
        {#if isRemoveAccessMethodVisible}
          <Button
            onclick={() =>
              (identityInfo.removableOpenIdCredential = credential)}
            variant="tertiary"
            iconOnly
            class="!text-fg-error-secondary"
          >
            <Link2OffIcon size="1.25rem" />
          </Button>
        {/if}
      </div>
    </div>
  {/each}
</div>

{#if identityInfo.removableOpenIdCredential}
  <RemoveOpenIdCredential
    onRemove={handleRemoveOpenIdCredential}
    onClose={() => (identityInfo.removableOpenIdCredential = null)}
    isCurrentAccessMethod={isRemovableOpenIdCredentialCurrentAccessMethod}
  />
{/if}

{#if identityInfo.removableAuthnMethod}
  <RemovePasskeyDialog
    onRemove={handleRemovePasskey}
    onClose={() => (identityInfo.removableAuthnMethod = null)}
    isCurrentAccessMethod={isRemovableAuthnMethodCurrentAccessMethod}
  />
{/if}

{#if identityInfo.renamableAuthnMethod}
  <RenamePasskeyDialog
    currentName={getAuthnMethodAlias(identityInfo.renamableAuthnMethod)}
    onRename={handleRenamePasskey}
    onClose={() => (identityInfo.renamableAuthnMethod = null)}
  />
{/if}
