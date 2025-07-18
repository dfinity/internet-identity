import { get } from "svelte/store";
import {
  AuthnMethodData,
  AuthnMethodRegistrationInfo,
  MetadataMapV2,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { authenticatedStore } from "./authentication.store";
import { isNullish, nonNullish } from "@dfinity/utils";
import { canisterConfig } from "$lib/globals";
import {
  createAnonymousNonce,
  createGoogleRequestConfig,
  decodeJWTWithNameAndEmail,
  requestJWT,
} from "$lib/utils/openID";
import { throwCanisterError } from "$lib/utils/utils";
import {
  lastUsedIdentitiesStore,
  lastUsedIdentityStore,
} from "./last-used-identities.store";
import { bufEquals } from "@dfinity/agent";
import { authnMethodEqual, authnMethodToPublicKey } from "$lib/utils/webAuthn";

const fetchIdentityInfo = async () => {
  const authenticated = get(authenticatedStore);

  const identityInfoResponse = await authenticated.actor.identity_info(
    authenticated.identityNumber,
  );

  if ("Err" in identityInfoResponse)
    throw Error("Failed to fetch identity info");

  return identityInfoResponse.Ok;
};

class IdentityInfo {
  loaded = $state(false);
  name = $state("");
  authnMethods = $state<AuthnMethodData[]>([]);
  authnMethodRegistration = $state<AuthnMethodRegistrationInfo>();

  openIdCredentials = $state<OpenIdCredential[]>([]);
  removableOpenIdCredential = $state<OpenIdCredential | null>(null);
  removableAuthnMethod = $state<AuthnMethodData | null>(null);
  renamableAuthnMethod = $state<AuthnMethodData | null>(null);

  totalAccessMethods = $derived<number>(
    this.authnMethods.length + this.openIdCredentials.length,
  );

  fetch = async () => {
    try {
      const {
        name,
        openid_credentials,
        authn_methods,
        authn_method_registration,
      } = await fetchIdentityInfo();
      if (nonNullish(name[0])) {
        this.name = name[0];
      }
      if (nonNullish(authn_methods)) {
        this.authnMethods = authn_methods;
      }
      if (nonNullish(openid_credentials[0])) {
        this.openIdCredentials = openid_credentials[0];
      }
      if (nonNullish(authn_method_registration[0])) {
        this.authnMethodRegistration = authn_method_registration[0];
      }
      this.loaded = true;
    } catch (e) {
      this.reset();
      throw e;
    }
  };

  addGoogle = async () => {
    const googleClientId = canisterConfig.openid_google[0]?.[0]?.client_id;
    if (isNullish(googleClientId)) throw new Error("Missing Google client ID");
    const { nonce, salt } = await createAnonymousNonce(
      get(authenticatedStore).identity.getPrincipal(),
    );
    const jwt = await requestJWT(createGoogleRequestConfig(googleClientId), {
      mediation: "required",
      nonce,
    });

    const { iss, sub, aud, name, email } = decodeJWTWithNameAndEmail(jwt);

    if (this.openIdCredentials.find((c) => c.iss === iss && c.sub === sub)) {
      throw new Error("Account already linked");
    }

    const { identityNumber } = get(authenticatedStore);

    const googleAddPromise = get(
      authenticatedStore,
    ).actor.openid_credential_add(identityNumber, jwt, salt);
    // Optimistically show as added
    this.openIdCredentials.push({
      aud,
      iss,
      sub,
      metadata: [
        ["name", { String: name }],
        ["email", { String: email }],
      ],
      last_usage_timestamp: [],
    });

    const googleAddResult = await googleAddPromise;

    if ("Ok" in googleAddResult) {
      void this.fetch();
    } else {
      this.openIdCredentials = this.openIdCredentials.filter(
        (cred) => !(cred.iss === iss && cred.sub === sub),
      );
    }

    await throwCanisterError(googleAddResult);
  };

  removeGoogle = async () => {
    if (!this.removableOpenIdCredential)
      throw Error("Must first select a credential to be removed!");

    const googleRemovePromise = get(
      authenticatedStore,
    ).actor.openid_credential_remove(get(authenticatedStore).identityNumber, [
      this.removableOpenIdCredential.iss,
      this.removableOpenIdCredential.sub,
    ]);
    // Optimistically show as removed
    this.openIdCredentials = this.openIdCredentials.filter(
      (cred) =>
        !(
          cred.iss === this.removableOpenIdCredential!.iss &&
          cred.sub === this.removableOpenIdCredential!.sub
        ),
    );
    const temporaryCredential = this.removableOpenIdCredential;
    this.removableOpenIdCredential = null;

    const googleRemoveResult = await googleRemovePromise;

    if ("Ok" in googleRemoveResult) {
      // If we just deleted the method we are logged in with, we log the user out.
      if (this.isCurrentAccessMethod({ openid: temporaryCredential })) {
        this.logout();
        lastUsedIdentitiesStore.removeIdentity(
          get(authenticatedStore).identityNumber,
        );
        return;
      }

      void this.fetch();
    } else {
      this.openIdCredentials.push(temporaryCredential);
    }

    await throwCanisterError(googleRemoveResult);
  };

  async removePasskey(): Promise<void> {
    const { actor, identityNumber } = get(authenticatedStore);
    if (isNullish(this.removableAuthnMethod)) {
      throw new Error("No passkey to remove");
    }
    const authnMethod = this.removableAuthnMethod;
    const publicKey = new Uint8Array(
      "WebAuthn" in authnMethod.authn_method
        ? authnMethod.authn_method.WebAuthn.pubkey
        : authnMethod.authn_method.PubKey.pubkey,
    );
    this.removableAuthnMethod = null;
    const index = this.authnMethods.findIndex((value) =>
      authnMethodEqual(value, authnMethod),
    );
    this.authnMethods.splice(index, 1);
    try {
      await actor
        .authn_method_remove(identityNumber, publicKey)
        .then(throwCanisterError);

      if (
        "WebAuthn" in authnMethod.authn_method &&
        this.isCurrentAccessMethod({
          passkey: {
            credentialId: new Uint8Array(
              authnMethod.authn_method.WebAuthn.credential_id,
            ),
          },
        })
      ) {
        lastUsedIdentitiesStore.removeIdentity(identityNumber);
        this.logout();
        return;
      }
      await this.fetch();
    } catch (error) {
      this.authnMethods.splice(index, 0, authnMethod);
      throw error;
    }
  }

  async renamePasskey(newName: string): Promise<void> {
    if (!this.renamableAuthnMethod)
      throw Error("Must first select a credential to be renamed!");

    const authnMethod = this.renamableAuthnMethod;
    let renamed = false;
    const newMetadata: MetadataMapV2 = authnMethod.metadata.map(
      ([key, value]) => {
        if (key === "alias") {
          renamed = true;
          return [key, { String: newName }];
        }
        return [key, value];
      },
    );
    if (!renamed) {
      newMetadata.push(["alias", { String: newName }]);
    }
    const { actor, identityNumber } = get(authenticatedStore);
    await actor
      .authn_method_metadata_replace(
        identityNumber,
        authnMethodToPublicKey(authnMethod),
        newMetadata,
      )
      .then(throwCanisterError);
    // Update authnMethods locally for faster feedback
    this.authnMethods = this.authnMethods.map((value) =>
      authnMethodEqual(value, authnMethod)
        ? {
            ...value,
            metadata: newMetadata,
          }
        : value,
    );
    await this.fetch();
  }

  logout = () => {
    // TODO: When we keep a session open we'll need to clean that session.
    // For now we just reload the page to make sure all the states are cleared
    window.location.reload();
  };

  isCurrentAccessMethod = (
    accessMethod:
      | { passkey: { credentialId: Uint8Array } }
      | { openid: { iss: string; sub: string } },
  ) => {
    const lastUsedAuthMethod = get(lastUsedIdentityStore)?.authMethod;
    if (
      nonNullish(lastUsedAuthMethod) &&
      "openid" in lastUsedAuthMethod &&
      "openid" in accessMethod
    ) {
      return (
        lastUsedAuthMethod.openid.iss === accessMethod.openid.iss &&
        lastUsedAuthMethod.openid.sub === accessMethod.openid.sub
      );
    }
    if (
      nonNullish(lastUsedAuthMethod) &&
      "passkey" in lastUsedAuthMethod &&
      "passkey" in accessMethod
    ) {
      return bufEquals(
        lastUsedAuthMethod.passkey.credentialId,
        accessMethod.passkey.credentialId,
      );
    }
    return false;
  };

  reset = () => {
    this.name = "";
    this.authnMethods = [];
    this.openIdCredentials = [];
    this.authnMethodRegistration = undefined;
    this.loaded = false;
  };
}

export default new IdentityInfo();
