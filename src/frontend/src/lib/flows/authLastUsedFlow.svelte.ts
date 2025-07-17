import {
  authenticateWithJWT,
  authenticateWithPasskey,
} from "$lib/utils/authentication";
import { canisterConfig, canisterId } from "$lib/globals";
import { authenticationStore } from "$lib/stores/authentication.store";
import {
  lastUsedIdentitiesStore,
  type LastUsedIdentity,
} from "$lib/stores/last-used-identities.store";
import { createGoogleRequestConfig, requestJWT } from "$lib/utils/openID";
import { get } from "svelte/store";
import { sessionStore } from "$lib/stores/session.store";
import { isNullish } from "@dfinity/utils";

export class AuthLastUsedFlow {
  systemOverlay = $state(false);
  authenticatingIdentity = $state<bigint | null>(null);
  authenticate = async (lastUsedIdentity: LastUsedIdentity): Promise<void> => {
    this.authenticatingIdentity = lastUsedIdentity.identityNumber;
    try {
      if ("passkey" in lastUsedIdentity.authMethod) {
        const { identity, identityNumber } = await authenticateWithPasskey({
          canisterId,
          session: get(sessionStore),
          credentialId: lastUsedIdentity.authMethod.passkey.credentialId,
        });
        authenticationStore.set({ identity, identityNumber });
        lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedIdentity);
      } else if (
        "openid" in lastUsedIdentity.authMethod &&
        lastUsedIdentity.authMethod.openid.iss === "https://accounts.google.com"
      ) {
        this.systemOverlay = true;
        const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id;
        if (isNullish(clientId)) {
          throw new Error("Google is not configured");
        }
        const requestConfig = createGoogleRequestConfig(clientId);
        const jwt = await requestJWT(requestConfig, {
          nonce: get(sessionStore).nonce,
          mediation: "required",
          loginHint: lastUsedIdentity.authMethod.openid.sub,
        });
        this.systemOverlay = false;
        const { identity, identityNumber } = await authenticateWithJWT({
          canisterId,
          session: get(sessionStore),
          jwt,
        });
        authenticationStore.set({ identity, identityNumber });
        lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedIdentity);
      } else {
        throw new Error("Unrecognized authentication method");
      }
    } finally {
      this.authenticatingIdentity = null;
    }
  };
}
