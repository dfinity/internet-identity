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
import { isNullish, nonNullish } from "@dfinity/utils";
import { fetchIdentityCredentials } from "$lib/utils/fetchCredentials";

export class AuthLastUsedFlow {
  systemOverlay = $state(false);
  authenticatingIdentity = $state<bigint | null>(null);
  #identityCredentials: Map<bigint, Promise<Uint8Array[] | undefined>> =
    new Map();

  init(identities: bigint[]) {
    identities.forEach((identityNumber) => {
      this.#identityCredentials.set(
        identityNumber,
        fetchIdentityCredentials(identityNumber),
      );
    });
  }

  authenticate = async (lastUsedIdentity: LastUsedIdentity): Promise<void> => {
    this.authenticatingIdentity = lastUsedIdentity.identityNumber;
    try {
      if ("passkey" in lastUsedIdentity.authMethod) {
        const credentialIds = (await this.#identityCredentials.get(
          lastUsedIdentity.identityNumber,
        )) ?? [lastUsedIdentity.authMethod.passkey.credentialId];
        const { identity, identityNumber } = await authenticateWithPasskey({
          canisterId,
          session: get(sessionStore),
          credentialIds,
        });
        authenticationStore.set({ identity, identityNumber });
        lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedIdentity);
      } else if ("openid" in lastUsedIdentity.authMethod) {
        this.systemOverlay = true;
        const issuer = lastUsedIdentity.authMethod.openid.iss;
        const clientId =
          nonNullish(canisterConfig.openid_google?.[0]?.[0]) &&
          issuer === "https://accounts.google.com"
            ? canisterConfig.openid_google?.[0]?.[0]?.client_id
            : canisterConfig.openid_configs[0]?.find(
                (config) => config.issuer === issuer,
              )?.client_id;
        if (isNullish(clientId)) {
          throw new Error("Google is not configured");
        }
        const requestConfig = createGoogleRequestConfig(clientId);
        const jwt = await requestJWT(requestConfig, {
          nonce: get(sessionStore).nonce,
          mediation: "optional",
          loginHint:
            lastUsedIdentity.authMethod.openid.loginHint ??
            lastUsedIdentity.authMethod.openid.sub,
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
      this.systemOverlay = false;
      this.authenticatingIdentity = null;
    }
  };
}
