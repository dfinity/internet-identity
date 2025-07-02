import { get } from "svelte/store";
import {
  AuthnMethodData,
  AuthnMethodRegistrationInfo,
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
import { authorizationStore } from "./authorization.store";
import { goto } from "$app/navigation";

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

  logout = () => {
    this.reset();
    void authorizationStore.init();
    void goto("/");
  };

  isCurrentAccessMethod = (
    accessMethod:
      | { passkey: { credentialId: Uint8Array } }
      | { openid: { iss: string; sub: string } },
  ) => {
    const lastUsedAuthMethod = get(lastUsedIdentityStore)?.authMethod;
    if (
      lastUsedAuthMethod &&
      "openid" in lastUsedAuthMethod &&
      "openid" in accessMethod &&
      lastUsedAuthMethod.openid.sub === accessMethod.openid.sub
    ) {
      return true;
    }
    if (
      lastUsedAuthMethod &&
      "passkey" in lastUsedAuthMethod &&
      "passkey" in accessMethod &&
      lastUsedAuthMethod.passkey.credentialId ===
        accessMethod.passkey.credentialId
    ) {
      return true;
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
