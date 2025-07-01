import { get } from "svelte/store";
import {
  AuthnMethodData,
  AuthnMethodRegistrationInfo,
  OpenIdCredential,
  OpenIdCredentialAddError,
  OpenIdCredentialRemoveError,
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
import { toaster } from "$lib/components/utils/toaster";
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

const formatOpenIdAddError = (err: OpenIdCredentialAddError) => {
  if ("OpenIdCredentialAlreadyRegistered" in err) {
    return "This credential is already linked to another identity";
  }
  if ("Unauthorized" in err) {
    return "You are not authorized to add this credential";
  }
  if ("InternalCanisterError" in err) {
    return "An internal error occurred: " + err.InternalCanisterError;
  }
  if ("JwtVerificationFailed" in err) {
    return "The JWT is invalid";
  }
  return "An unknown error occurred";
};

const formatOpenIdRemoveError = (err: OpenIdCredentialRemoveError) => {
  if ("OpenIdCredentialNotFound" in err) {
    return "This credential is not linked to this identity";
  }
  if ("Unauthorized" in err) {
    return "You are not authorized to remove this credential";
  }
  if ("InternalCanisterError" in err) {
    return "An internal error occurred: " + err.InternalCanisterError;
  }
  return "An unknown error occurred";
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

    const googleAddPromise = get(
      authenticatedStore,
    ).actor.openid_credential_add(
      get(authenticatedStore).identityNumber,
      jwt,
      salt,
    );
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
      toaster.error({
        title: "Failed to add Google Account",
        description: formatOpenIdAddError(googleAddResult.Err),
      });
      throw new Error(Object.keys(googleAddResult.Err)[0]);
    }
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
      void this.fetch();
    } else {
      this.openIdCredentials.push(temporaryCredential);
      toaster.error({
        title: "Failed to remove Google Account",
        description: formatOpenIdRemoveError(googleRemoveResult.Err),
      });
      throw new Error(Object.keys(googleRemoveResult.Err)[0]);
    }
  };

  logout = () => {
    void this.reset();
    void authorizationStore.init();
    goto("/");
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
