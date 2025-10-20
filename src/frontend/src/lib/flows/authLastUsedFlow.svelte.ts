import {
  authenticateWithJWT,
  authenticateWithPasskey,
} from "$lib/utils/authentication";
import { canisterId } from "$lib/globals";
import { authenticationStore } from "$lib/stores/authentication.store";
import {
  lastUsedIdentitiesStore,
  type LastUsedIdentity,
} from "$lib/stores/last-used-identities.store";
import {
  createGoogleRequestConfig,
  findConfig,
  isOpenIdConfig,
  requestJWT,
} from "$lib/utils/openID";
import { get } from "svelte/store";
import { sessionStore } from "$lib/stores/session.store";
import { isNullish } from "@dfinity/utils";
import { fetchIdentityCredentials } from "$lib/utils/fetchCredentials";
import {
  AuthenticationV2Events,
  authenticationV2Funnel,
} from "$lib/utils/analytics/authenticationV2Funnel";

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
        await authenticationStore.set({ identity, identityNumber });
        lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedIdentity);
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.ContinueAsPasskey,
        );
      } else if ("openid" in lastUsedIdentity.authMethod) {
        this.systemOverlay = true;
        const issuer = lastUsedIdentity.authMethod.openid.iss;
        const config = findConfig(
          issuer,
          lastUsedIdentity.authMethod.openid.metadata ?? [],
        );
        if (isNullish(config)) {
          throw new Error(
            "OpenID authentication is not available for this account.",
          );
        }
        const requestConfig = isOpenIdConfig(config)
          ? {
              issuer,
              clientId: config.client_id,
              configURL: config.fedcm_uri[0],
              authURL: config.auth_uri,
              authScope: config.auth_scope.join(" "),
            }
          : createGoogleRequestConfig(config.client_id);
        const jwt = await requestJWT(requestConfig, {
          nonce: get(sessionStore).nonce,
          mediation: "optional",
          loginHint: lastUsedIdentity.authMethod.openid.loginHint,
        });
        this.systemOverlay = false;
        const { identity, identityNumber } = await authenticateWithJWT({
          canisterId,
          session: get(sessionStore),
          jwt,
        });
        await authenticationStore.set({ identity, identityNumber });
        lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedIdentity);
        authenticationV2Funnel.addProperties({
          provider: isOpenIdConfig(config) ? config.name : "Google",
        });
        authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsOpenID);
      } else {
        throw new Error("Unrecognized authentication method");
      }
    } finally {
      this.systemOverlay = false;
      this.authenticatingIdentity = null;
    }
  };
}
