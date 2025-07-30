import {
  authenticateWithJWT,
  authenticateWithPasskey,
} from "$lib/utils/authentication";
import { anonymousActor, canisterConfig, canisterId } from "$lib/globals";
import { authenticationStore } from "$lib/stores/authentication.store";
import {
  lastUsedIdentitiesStore,
  type LastUsedIdentity,
} from "$lib/stores/last-used-identities.store";
import { createGoogleRequestConfig, requestJWT } from "$lib/utils/openID";
import { get } from "svelte/store";
import { sessionStore } from "$lib/stores/session.store";
import { isNullish, nonNullish } from "@dfinity/utils";
import { convertToValidCredentialData } from "$lib/utils/credential-devices";

const fetchIdentityCredentials = async (
  identityNumber: bigint,
): Promise<Uint8Array[] | undefined> => {
  try {
    const identityCredentials = await anonymousActor.lookup(identityNumber);
    const validCredentials = identityCredentials
      .filter((device) => "authentication" in device.purpose)
      .filter(({ key_type }) => !("browser_storage_key" in key_type))
      .map(convertToValidCredentialData)
      .filter(nonNullish);

    if (validCredentials.length > 0) {
      return validCredentials.map(
        (credential) => new Uint8Array(credential.credentialId),
      );
    }

    return undefined;
  } catch (error) {
    console.warn(
      `Error looking up identity ${identityNumber} credentials:`,
      error,
    );
    return undefined;
  }
};

export class AuthLastUsedFlow {
  systemOverlay = $state(false);
  authenticatingIdentity = $state<bigint | null>(null);
  authenticate = async (lastUsedIdentity: LastUsedIdentity): Promise<void> => {
    this.authenticatingIdentity = lastUsedIdentity.identityNumber;
    try {
      if ("passkey" in lastUsedIdentity.authMethod) {
        // If there is a problem looking up the credentials, we fallback to the credentialId provided by the lastUsedIdentity
        const credentialIds = (await fetchIdentityCredentials(
          lastUsedIdentity.identityNumber,
        )) ?? [lastUsedIdentity.authMethod.passkey.credentialId];
        const { identity, identityNumber } = await authenticateWithPasskey({
          canisterId,
          session: get(sessionStore),
          credentialIds,
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
      this.systemOverlay = false;
      this.authenticatingIdentity = null;
    }
  };
}
