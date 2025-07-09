import { canisterConfig } from "$lib/globals";
import { sessionStore } from "$lib/stores/session.store";
import { get } from "svelte/store";
import { isNullish } from "@dfinity/utils";
import { createGoogleRequestConfig, requestJWT } from "$lib/utils/openID";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { throwCanisterError } from "$lib/utils/utils";

export class AddAccessMethodFlow {
  view = $state<"chooseMethod">("chooseMethod");
  isGoogleAuthenticating = $state(false);
  isSystemOverlayVisible = $state(false);

  linkGoogleAccount = async (): Promise<void> => {
    const { nonce, salt } = get(sessionStore);
    const { actor, identityNumber } = get(authenticatedStore);

    const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id;
    if (isNullish(clientId)) {
      throw new Error("Google is not configured");
    }
    try {
      const requestConfig = createGoogleRequestConfig(clientId);
      this.isGoogleAuthenticating = true;
      this.isSystemOverlayVisible = true;
      const jwt = await requestJWT(requestConfig, {
        nonce,
        mediation: "required",
      });
      this.isSystemOverlayVisible = false;
      await actor
        .openid_credential_add(identityNumber, jwt, salt)
        .then(throwCanisterError);
    } catch (error) {
      this.isSystemOverlayVisible = false;
      throw error;
    } finally {
      this.isGoogleAuthenticating = false;
    }
  };
}
