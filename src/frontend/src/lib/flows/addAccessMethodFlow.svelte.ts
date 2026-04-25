import { frontendCanisterConfig } from "$lib/globals";
import { get } from "svelte/store";
import { decodeJWT, requestJWT, selectAuthScopes } from "$lib/utils/openID";
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
import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";

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
      // AddAccessMethod.svelte already disables the per-provider button
      // when that provider is linked to the current identity, and
      // SignInWithSso.svelte disables Continue once discovery reveals
      // the same `(iss, aud)` on the identity, so the canister's
      // `OpenIdCredentialAlreadyRegistered` reply here is only hit when
      // the credential is linked to another identity. Treat it as-is.
      await actor
        .openid_credential_add(identityNumber, jwt, salt)
        .then(throwCanisterError);

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
        // `sso_domain` / `sso_name` are populated by the canister when
        // the credential is later returned via `get_anchor_info`; the
        // FE-constructed echo of the just-added credential doesn't know
        // them, so leave empty (Candid `opt text` = `[] | [string]`).
        sso_domain: [],
        sso_name: [],
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
   * The canister itself stamps `sso_domain` (and, when published by the
   * domain, `sso_name`) onto the credential metadata on verification, so
   * the access-methods UI can label SSO credentials by domain across
   * devices — no per-device FE bookkeeping needed.
   */
  linkSsoAccount = (result: SsoDiscoveryResult): Promise<OpenIdCredential> => {
    const { clientId, discovery } = result;
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
    return this.linkOpenIdAccount(syntheticConfig);
  };
}
