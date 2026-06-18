<script lang="ts">
  import type { PageProps } from "./$types";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { PlusIcon } from "@lucide/svelte";
  import { getMetadataString, openIdName } from "$lib/utils/openID";
  import type {
    AuthnMethodData,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";
  import RenamePasskey from "./components/RenamePasskey.svelte";
  import RemoveAccessMethod from "./components/RemoveAccessMethod.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { authenticatedStore } from "$lib/stores/authentication.store";
  import { throwCanisterError } from "$lib/utils/utils";
  import { authnMethodToPublicKey } from "$lib/utils/webAuthn";
  import { nanosToMillis } from "$lib/utils/time";
  import {
    afterNavigate,
    goto,
    invalidateAll,
    replaceState,
  } from "$app/navigation";
  import { canisterId } from "$lib/globals";
  import { authenticationStore } from "$lib/stores/authentication.store";
  import {
    mintSession,
    purgeSession,
  } from "$lib/stores/session-delegation.store";
  import { authenticateWithPasskey } from "$lib/utils/authentication/passkey";
  import { authenticateWithJWT } from "$lib/utils/authentication/jwt";
  import {
    decodeJWT,
    findConfig,
    requestJWT,
    requestWithPopup,
    selectAuthScopes,
  } from "$lib/utils/openID";
  import { discoverSsoConfig } from "$lib/utils/ssoDiscovery";
  import { get } from "svelte/store";
  import { AddAccessMethodWizard } from "$lib/components/wizards/addAccessMethod";
  import { flip } from "svelte/animate";
  import { scale } from "svelte/transition";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
  import OpenIdItem from "./components/OpenIdItem.svelte";
  import PasskeyItem from "./components/PasskeyItem.svelte";
  import SwitchAccessMethod from "./components/SwitchAccessMethod.svelte";
  import {
    compareAccessMethods,
    toAccessMethods,
    toKey,
    isCurrentAccessMethod,
  } from "./utils";
  import { sessionStore } from "$lib/stores/session.store";
  import { page } from "$app/state";
  import { ConfirmAccessMethodWizard } from "$lib/components/wizards/confirmAccessMethod";
  import { toaster } from "$lib/components/utils/toaster";

  const MAX_PASSKEYS = 16;

  const { data }: PageProps = $props();

  // State
  let isAddingAccessMethod = $state(false);
  let renamingAccessMethodKey = $state<string>();
  let removingAccessMethodKey = $state<string>();
  let switchingAccessMethodKey = $state<string>();
  let accessMethods = $derived(toAccessMethods(data.identityInfo));
  let pendingRegistrationId = $state(data.pendingRegistrationId);
  let showRegistrationDialog = $state(data.pendingRegistrationId !== null);

  // Derived
  const renamingAccessMethod = $derived(
    accessMethods.find((m) => renamingAccessMethodKey === toKey(m)),
  );
  const removingAccessMethod = $derived(
    accessMethods.find((m) => removingAccessMethodKey === toKey(m)),
  );
  const switchingAccessMethod = $derived(
    accessMethods.find((m) => switchingAccessMethodKey === toKey(m)),
  );
  const isSignedInWithRecovery = $derived(
    "recoveryPhrase" in $authenticatedStore.authMethod ||
      "emailRecovery" in $authenticatedStore.authMethod,
  );
  const isUsingPasskeys = $derived(accessMethods.some((m) => "passkey" in m));
  const maxPasskeysReached = $derived(
    accessMethods.filter((m) => "passkey" in m).length >= MAX_PASSKEYS,
  );
  const openIdCredentials = $derived(
    accessMethods.filter((m) => "openid" in m).map(({ openid }) => openid),
  );
  let recoveryPhraseStatus: "missing" | "unverified" | "verified" = $derived.by(
    () => {
      const value = data.identityInfo.authn_methods.find(
        (m) =>
          "Recovery" in m.security_settings.purpose &&
          getMetadataString(m.metadata, "usage") === "recovery_phrase",
      );
      return value === undefined
        ? "missing"
        : value.last_authentication[0] === undefined ||
            "Protected" in value.security_settings.protection
          ? "unverified"
          : "verified";
    },
  );

  // Handlers
  const handleOpenIdLinked = (openid: OpenIdCredential) => {
    isAddingAccessMethod = false;
    accessMethods = [{ openid } as const, ...accessMethods].sort(
      compareAccessMethods,
    );
    lastUsedIdentitiesStore.addLastUsedIdentityIfMissing({
      identityNumber: data.identityNumber,
      name: data.identityInfo.name[0],
      createdAtMillis:
        data.identityInfo.created_at[0] !== undefined
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
  const handlePasskeyRegistered = (passkey: AuthnMethodData) => {
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
          data.identityInfo.created_at[0] !== undefined
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
  const handleOtherDeviceRegistered = () => {
    isAddingAccessMethod = false;
    void invalidateAll();
  };
  const handleOtherDeviceConfirmed = () => {
    toaster.success({
      title: $t`Passkey has been registered from another device.`,
    });
    showRegistrationDialog = false;
    pendingRegistrationId = null;
    // Remove searchParam and update state
    void goto(page.url.pathname, { replaceState: true, invalidateAll: true });
  };
  // Called when the user clicks "Start over" inside the wizard. The wizard
  // generates a fresh registrationId internally; here we just drop the URL
  // ?activate=<id> so a refresh doesn't reopen the old pending session.
  const handleConfirmRestart = () => {
    pendingRegistrationId = null;
    if (page.url.searchParams.has("activate")) {
      void goto(page.url.pathname, { replaceState: true });
    }
  };
  const closeConfirmDialog = () => {
    showRegistrationDialog = false;
    pendingRegistrationId = null;
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
  const handleSwitchConfirmed = async () => {
    if (switchingAccessMethod === undefined) return;
    const session = get(sessionStore);
    const createdAtMillis =
      data.identityInfo.created_at[0] !== undefined
        ? nanosToMillis(data.identityInfo.created_at[0])
        : undefined;
    try {
      if (
        "passkey" in switchingAccessMethod &&
        "WebAuthn" in switchingAccessMethod.passkey.authn_method
      ) {
        const credentialId = new Uint8Array(
          switchingAccessMethod.passkey.authn_method.WebAuthn.credential_id,
        );
        const {
          identity,
          identityNumber,
          credentialId: authedId,
        } = await authenticateWithPasskey({
          canisterId,
          session,
          credentialIds: [credentialId],
        });
        // Guard against the assertion resolving to a different anchor (e.g.
        // shared authenticator). We're only switching the active method for
        // *this* identity — anything else is a programming error or attack.
        if (identityNumber !== data.identityNumber) {
          throw new Error(
            "Authenticated identity does not match the identity being managed.",
          );
        }
        await authenticationStore.set({
          identity,
          identityNumber,
          authMethod: { passkey: { credentialId: authedId } },
        });
        void mintSession({
          identityNumber,
          actor: get(authenticatedStore).actor,
        });
        lastUsedIdentitiesStore.addLastUsedIdentity({
          identityNumber,
          name: data.identityInfo.name[0],
          createdAtMillis,
          authMethod: { passkey: { credentialId: authedId } },
        });
      } else if ("openid" in switchingAccessMethod) {
        const { iss, aud, metadata, sso_domain } = switchingAccessMethod.openid;
        const ssoDomain = sso_domain[0];
        // Re-use the loginHint we stored the last time this identity signed in
        // via an OpenID/SSO method — most providers honor it and skip the
        // account picker. Authoritative source is the lastUsedIdentitiesStore
        // (populated from `decodeJWT(...).loginHint` on each sign-in).
        const lastUsed = get(lastUsedIdentitiesStore).identities[
          data.identityNumber.toString()
        ];
        const lastUsedAuthMethod = lastUsed?.authMethod;
        const loginHint =
          lastUsedAuthMethod !== undefined && "openid" in lastUsedAuthMethod
            ? lastUsedAuthMethod.openid.loginHint
            : lastUsedAuthMethod !== undefined && "sso" in lastUsedAuthMethod
              ? lastUsedAuthMethod.sso.loginHint
              : undefined;
        let jwt: string;
        if (ssoDomain !== undefined) {
          jwt = await requestWithPopup(
            discoverSsoConfig(ssoDomain).then((ssoResult) => ({
              clientId: ssoResult.clientId,
              authURL: ssoResult.discovery.authorization_endpoint,
              authScope: selectAuthScopes(
                ssoResult.discovery.scopes_supported,
              ).join(" "),
            })),
            { nonce: session.nonce, mediation: "optional", loginHint },
          );
        } else {
          const config = findConfig(iss, aud, metadata);
          if (config === undefined)
            throw new Error(
              "OpenID authentication is not available for this account.",
            );
          jwt = await requestJWT(
            {
              clientId: config.client_id,
              configURL: config.fedcm_uri[0],
              authURL: config.auth_uri,
              authScope: config.auth_scope.join(" "),
            },
            { nonce: session.nonce, mediation: "optional", loginHint },
          );
        }
        const { iss: jwtIss, sub, loginHint: jwtLoginHint } = decodeJWT(jwt);
        const { identity, identityNumber } = await authenticateWithJWT({
          canisterId,
          session,
          jwt,
        });
        // The OAuth popup lets the user pick any account — if they choose one
        // linked to a different anchor, the JWT authenticates to that anchor.
        // Refuse the switch so we don't leak the wrong identity into the
        // last-used store or the active session.
        if (identityNumber !== data.identityNumber) {
          throw new Error(
            "Authenticated identity does not match the identity being managed.",
          );
        }
        await authenticationStore.set({
          identity,
          identityNumber,
          authMethod: { openid: { iss: jwtIss, sub } },
        });
        void mintSession({
          identityNumber,
          actor: get(authenticatedStore).actor,
        });
        lastUsedIdentitiesStore.addLastUsedIdentity({
          identityNumber,
          name: data.identityInfo.name[0],
          createdAtMillis,
          authMethod: {
            openid: { iss: jwtIss, sub, loginHint: jwtLoginHint, metadata },
          },
        });
      }
      switchingAccessMethodKey = undefined;
      toaster.success({
        title: $t`Successfully switched access method`,
        description: $t`This is now the default method used to authenticate moving forward on this device.`,
      });
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
            removingAccessMethod.openid.aud,
          ])
          .then(throwCanisterError);
      }
      // Logout and forget identity if it's the current access method
      if (isCurrentAccessMethod($authenticatedStore, removingAccessMethod)) {
        const identityNumber = $authenticatedStore.identityNumber;
        lastUsedIdentitiesStore.removeIdentity(identityNumber);
        void purgeSession(identityNumber);
        sessionStore.reset();
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

  // Deep-link entry: open the Add Access Method wizard when navigated
  // to with page state `{ add: true }` (set by the home dashboard's
  // smart-action strip).
  afterNavigate(() => {
    if (!("add" in page.state)) return;
    replaceState("", {});
    isAddingAccessMethod = true;
  });
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
              onRemove={() => (removingAccessMethodKey = toKey(accessMethod))}
              onSwitch={() => (switchingAccessMethodKey = toKey(accessMethod))}
              isCurrentAccessMethod={isCurrentAccessMethod(
                $authenticatedStore,
                accessMethod,
              )}
              isLastAccessMethod={accessMethods.length === 1}
              {isSignedInWithRecovery}
              {recoveryPhraseStatus}
            />
          {:else if "openid" in accessMethod}
            <OpenIdItem
              openid={accessMethod.openid}
              onUnlink={() => (removingAccessMethodKey = toKey(accessMethod))}
              onSwitch={() => (switchingAccessMethodKey = toKey(accessMethod))}
              isCurrentAccessMethod={isCurrentAccessMethod(
                $authenticatedStore,
                accessMethod,
              )}
              isLastAccessMethod={accessMethods.length === 1}
              {isSignedInWithRecovery}
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
    {#if "openid" in removingAccessMethod}
      <RemoveAccessMethod
        type="openid"
        onRemove={handleRemoveConfirmed}
        onCancel={() => (removingAccessMethodKey = undefined)}
        providerName={openIdName(
          removingAccessMethod.openid.iss,
          removingAccessMethod.openid.aud,
          removingAccessMethod.openid.metadata,
          removingAccessMethod.openid.sso_name[0],
          removingAccessMethod.openid.sso_domain[0],
        ) ?? $t`Unknown`}
        isCurrentAccessMethod={isCurrentAccessMethod(
          $authenticatedStore,
          removingAccessMethod,
        )}
        isLastAccessMethod={accessMethods.length === 1}
      />
    {:else}
      <RemoveAccessMethod
        type="passkey"
        onRemove={handleRemoveConfirmed}
        onCancel={() => (removingAccessMethodKey = undefined)}
        isCurrentAccessMethod={isCurrentAccessMethod(
          $authenticatedStore,
          removingAccessMethod,
        )}
        isLastAccessMethod={accessMethods.length === 1}
      />
    {/if}
  </Dialog>
{/if}

{#if switchingAccessMethod !== undefined}
  <Dialog onClose={() => (switchingAccessMethodKey = undefined)}>
    <SwitchAccessMethod
      onSwitch={handleSwitchConfirmed}
      onCancel={() => (switchingAccessMethodKey = undefined)}
    />
  </Dialog>
{/if}

{#if showRegistrationDialog}
  <Dialog onClose={closeConfirmDialog}>
    <ConfirmAccessMethodWizard
      registrationId={pendingRegistrationId ?? undefined}
      onConfirm={handleOtherDeviceConfirmed}
      onRestart={handleConfirmRestart}
      onError={(error) => {
        closeConfirmDialog();
        handleError(error);
        void goto(page.url.pathname, { replaceState: true });
      }}
    />
  </Dialog>
{/if}
