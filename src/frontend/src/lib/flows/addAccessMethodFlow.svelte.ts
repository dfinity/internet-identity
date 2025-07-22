import { canisterConfig } from "$lib/globals";
import { get } from "svelte/store";
import { isNullish, nonNullish } from "@dfinity/utils";
import {
  createAnonymousNonce,
  createGoogleRequestConfig,
  decodeJWTWithNameAndEmail,
  requestJWT,
} from "$lib/utils/openID";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { throwCanisterError } from "$lib/utils/utils";
import type {
  AuthnMethodData,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { features } from "$lib/legacy/features";
import { DiscoverableDummyIdentity } from "$lib/utils/discoverableDummyIdentity";
import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { inferPasskeyAlias, loadUAParser } from "$lib/legacy/flows/register";
import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";

export interface AddAccessMethodFlowOptions {
  isMaxOpenIdCredentialsReached?: boolean;
}

export class AddAccessMethodFlow {
  #view = $state<"chooseMethod" | "addPasskey">("chooseMethod");
  #isSystemOverlayVisible = $state(false);

  get view() {
    return this.#view;
  }

  get isSystemOverlayVisible() {
    return this.#isSystemOverlayVisible;
  }

  constructor(options?: AddAccessMethodFlowOptions) {
    if (options?.isMaxOpenIdCredentialsReached === true) {
      this.#view = "addPasskey";
    }
  }

  linkGoogleAccount = async (): Promise<OpenIdCredential> => {
    const { identity, actor, identityNumber } = get(authenticatedStore);

    const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id;
    if (isNullish(clientId)) {
      throw new Error("Google is not configured");
    }
    try {
      const requestConfig = createGoogleRequestConfig(clientId);
      this.#isSystemOverlayVisible = true;
      const { nonce, salt } = await createAnonymousNonce(
        identity.getPrincipal(),
      );
      const jwt = await requestJWT(requestConfig, {
        nonce,
        mediation: "required",
      });
      const { iss, sub, aud, name, email } = decodeJWTWithNameAndEmail(jwt);
      this.#isSystemOverlayVisible = false;
      await actor
        .openid_credential_add(identityNumber, jwt, salt)
        .then(throwCanisterError);
      return {
        aud,
        iss,
        sub,
        metadata: [
          ["name", { String: name }],
          ["email", { String: email }],
        ],
        last_usage_timestamp: [],
      };
    } finally {
      this.#isSystemOverlayVisible = false;
    }
  };

  createPasskey = async (): Promise<AuthnMethodData> => {
    const { selected } = get(lastUsedIdentitiesStore);
    const { actor, identityNumber } = get(authenticatedStore);
    const name = selected?.name;
    if (isNullish(name)) {
      throw new Error("Identity is missing a name");
    }

    const passkeyIdentity =
      features.DUMMY_AUTH || nonNullish(canisterConfig.dummy_auth[0]?.[0])
        ? await DiscoverableDummyIdentity.createNew(name)
        : await DiscoverablePasskeyIdentity.createNew(name);
    const credentialId = passkeyIdentity.getCredentialId();
    if (isNullish(credentialId)) {
      throw new Error("Credential ID is missing");
    }
    const uaParser = loadUAParser();
    const alias = await inferPasskeyAlias({
      authenticatorType: passkeyIdentity.getAuthenticatorAttachment(),
      userAgent: navigator.userAgent,
      uaParser,
      aaguid: passkeyIdentity.getAaguid(),
    });
    const authnMethodData = passkeyAuthnMethodData({
      alias,
      pubKey: passkeyIdentity.getPublicKey().toDer(),
      credentialId,
      authenticatorAttachment: passkeyIdentity.getAuthenticatorAttachment(),
      origin: window.location.origin,
    });
    await actor
      .authn_method_add(identityNumber, authnMethodData)
      .then(throwCanisterError);
    return authnMethodData;
  };

  continueWithPasskey = () => {
    this.#view = "addPasskey";
  };
}
