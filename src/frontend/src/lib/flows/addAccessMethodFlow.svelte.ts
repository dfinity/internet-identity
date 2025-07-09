import { canisterConfig } from "$lib/globals";
import { get } from "svelte/store";
import { isNullish } from "@dfinity/utils";
import {
  createAnonymousNonce,
  createGoogleRequestConfig,
  decodeJWTWithNameAndEmail,
  requestJWT,
} from "$lib/utils/openID";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { throwCanisterError } from "$lib/utils/utils";
import type { OpenIdCredential } from "$lib/generated/internet_identity_types";

export class AddAccessMethodFlow {
  view = $state<"chooseMethod">("chooseMethod");
  isGoogleAuthenticating = $state(false);
  isSystemOverlayVisible = $state(false);

  linkGoogleAccount = async (): Promise<OpenIdCredential> => {
    const { identity, actor, identityNumber } = get(authenticatedStore);

    const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id;
    if (isNullish(clientId)) {
      throw new Error("Google is not configured");
    }
    try {
      const requestConfig = createGoogleRequestConfig(clientId);
      this.isGoogleAuthenticating = true;
      this.isSystemOverlayVisible = true;
      const { nonce, salt } = await createAnonymousNonce(
        identity.getPrincipal(),
      );
      const jwt = await requestJWT(requestConfig, {
        nonce,
        mediation: "required",
      });
      const { iss, sub, aud, name, email } = decodeJWTWithNameAndEmail(jwt);
      this.isSystemOverlayVisible = false;
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
    } catch (error) {
      this.isSystemOverlayVisible = false;
      throw error;
    } finally {
      this.isGoogleAuthenticating = false;
    }
  };
}
