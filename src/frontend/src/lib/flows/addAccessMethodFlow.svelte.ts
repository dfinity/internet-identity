import { canisterConfig } from "$lib/globals";
import { get } from "svelte/store";
import { isNullish, nonNullish } from "@dfinity/utils";
import { decodeJWT, requestJWT } from "$lib/utils/openID";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { throwCanisterError } from "$lib/utils/utils";
import type {
  AuthnMethodData,
  OpenIdCredential,
  OpenIdConfig,
  MetadataMapV2,
} from "$lib/generated/internet_identity_types";
import { features } from "$lib/legacy/features";
import { DiscoverableDummyIdentity } from "$lib/utils/discoverableDummyIdentity";
import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
import { inferPasskeyAlias, loadUAParser } from "$lib/legacy/flows/register";
import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
import { aaguidToString } from "$lib/utils/webAuthn";

export class AddAccessMethodFlow {
  #view = $state<"chooseMethod" | "addPasskey">("chooseMethod");
  #isSystemOverlayVisible = $state(false);

  get view() {
    return this.#view;
  }

  get isSystemOverlayVisible() {
    return this.#isSystemOverlayVisible;
  }

  linkOpenIdAccount = async (
    config: OpenIdConfig,
  ): Promise<OpenIdCredential> => {
    const { actor, identityNumber, salt, nonce } = get(authenticatedStore);

    try {
      this.#isSystemOverlayVisible = true;
      const jwt = await requestJWT(
        {
          clientId: config.client_id,
          authURL: config.auth_uri,
          configURL: config.fedcm_uri?.[0],
          authScope: config.auth_scope.join(" "),
        },
        {
          nonce,
          mediation: "required",
        },
      );
      const { iss, sub, aud, name, email } = decodeJWT(jwt);
      this.#isSystemOverlayVisible = false;
      await actor
        .openid_credential_add(identityNumber, jwt, salt)
        .then(throwCanisterError);

      const metadata: MetadataMapV2 = [];
      if (nonNullish(name)) {
        metadata.push(["name", { String: name }]);
      }
      if (nonNullish(email)) {
        metadata.push(["email", { String: email }]);
      }
      return {
        aud,
        iss,
        sub,
        metadata,
        last_usage_timestamp: [],
      };
    } finally {
      this.#isSystemOverlayVisible = false;
    }
  };

  createPasskey = async (name?: string): Promise<AuthnMethodData> => {
    const { actor, identityNumber } = get(authenticatedStore);
    // TODO: Do not fail if name is not provided, maybe use the identity number as a fallback?
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
    const aaguid = passkeyIdentity.getAaguid();
    const alias = await inferPasskeyAlias({
      authenticatorType: passkeyIdentity.getAuthenticatorAttachment(),
      userAgent: navigator.userAgent,
      uaParser,
      aaguid,
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
