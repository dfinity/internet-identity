import { frontendCanisterConfig } from "$lib/globals";
import { get } from "svelte/store";
import {
  OpenIdCredentialAlreadyLinkedHereError,
  decodeJWT,
  requestJWT,
  selectAuthScopes,
} from "$lib/utils/openID";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import type {
  AuthnMethodData,
  OpenIdCredential,
  OpenIdCredentialAddError,
  OpenIdConfig,
  MetadataMapV2,
} from "$lib/generated/internet_identity_types";
import { features } from "$lib/legacy/features";
import { DiscoverableDummyIdentity } from "$lib/utils/discoverableDummyIdentity";
import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";
import { rememberSsoDomainForCredential } from "$lib/utils/ssoDomainStorage";

export class AddAccessMethodFlow {
  #view = $state<"chooseMethod" | "addPasskey" | "signInWithSso">(
    "chooseMethod",
  );
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
      try {
        await actor
          .openid_credential_add(identityNumber, jwt, salt)
          .then(throwCanisterError);
      } catch (err) {
        // Specialize the generic "already registered to another identity"
        // error: if the credential's (iss, sub, aud) is already attached
        // to *this* identity, tell the user that specifically instead of
        // implying another identity has it.
        if (
          isCanisterError<OpenIdCredentialAddError>(err) &&
          err.type === "OpenIdCredentialAlreadyRegistered"
        ) {
          const info = await actor.get_anchor_info(identityNumber);
          const linkedHere =
            info.openid_credentials[0]?.some(
              (c) => c.iss === iss && c.sub === sub && c.aud === aud,
            ) ?? false;
          if (linkedHere) {
            throw new OpenIdCredentialAlreadyLinkedHereError();
          }
        }
        throw err;
      }

      const metadata: MetadataMapV2 = [];
      if (name !== undefined) {
        metadata.push(["name", { String: name }]);
      }
      if (email !== undefined) {
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
    if (name === undefined) {
      throw new Error("Identity is missing a name");
    }

    const passkeyIdentity =
      features.DUMMY_AUTH ||
      frontendCanisterConfig.dummy_auth[0]?.[0] !== undefined
        ? await DiscoverableDummyIdentity.createNew(name)
        : await DiscoverablePasskeyIdentity.createNew(name);
    const credentialId = passkeyIdentity.getCredentialId();
    if (credentialId === undefined) {
      throw new Error("Credential ID is missing");
    }
    const authnMethodData = passkeyAuthnMethodData({
      pubKey: passkeyIdentity.getPublicKey().toDer(),
      credentialId,
      authenticatorAttachment: passkeyIdentity.getAuthenticatorAttachment(),
      origin: window.location.origin,
      aaguid: passkeyIdentity.getAaguid(),
    });
    await actor
      .authn_method_add(identityNumber, authnMethodData)
      .then(throwCanisterError);
    return authnMethodData;
  };

  continueWithPasskey = () => {
    this.#view = "addPasskey";
  };

  signInWithSso = () => {
    this.#view = "signInWithSso";
  };

  chooseMethod = () => {
    this.#view = "chooseMethod";
  };

  /**
   * Link an SSO-discovered provider as an access method to the current
   * identity. Reuses {@link linkOpenIdAccount} by synthesizing an
   * `OpenIdConfig` from the two-hop discovery result — the issuer and
   * client_id we got from discovery plus a default name.
   *
   * On success we also remember `(iss, sub, aud) → domain` in localStorage
   * so the access-methods UI can later label this credential by the SSO
   * domain the user typed instead of by the underlying IdP's issuer.
   */
  linkSsoAccount = async (
    result: SsoDiscoveryResult,
  ): Promise<OpenIdCredential> => {
    const { domain, clientId, discovery } = result;
    const syntheticConfig: OpenIdConfig = {
      auth_uri: discovery.authorization_endpoint,
      jwks_uri: "",
      logo: "",
      name: "SSO",
      fedcm_uri: [],
      email_verification: [],
      issuer: discovery.issuer,
      auth_scope: selectAuthScopes(discovery.scopes_supported),
      client_id: clientId,
    };
    const credential = await this.linkOpenIdAccount(syntheticConfig);
    rememberSsoDomainForCredential(
      { iss: credential.iss, sub: credential.sub, aud: credential.aud },
      domain,
    );
    return credential;
  };
}
