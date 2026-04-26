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
  decodeJWT,
  findConfig,
  requestJWT,
  requestWithPopup,
  selectAuthScopes,
} from "$lib/utils/openID";
import { discoverSsoConfig } from "$lib/utils/ssoDiscovery";
import { get } from "svelte/store";
import { sessionStore } from "$lib/stores/session.store";
import { fetchIdentityCredentials } from "$lib/utils/fetchCredentials";
import {
  AuthenticationV2Events,
  authenticationV2Funnel,
} from "$lib/utils/analytics/authenticationV2Funnel";
import { SvelteMap } from "svelte/reactivity";

export class AuthLastUsedFlow {
  systemOverlay = $state(false);
  authenticatingIdentity = $state<bigint | null>(null);
  #identityCredentials: Map<bigint, Promise<Uint8Array[] | undefined>> =
    new SvelteMap();

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
        const { identity, identityNumber, credentialId } =
          await authenticateWithPasskey({
            canisterId,
            session: get(sessionStore),
            credentialIds,
          });
        await authenticationStore.set({
          identity,
          identityNumber,
          authMethod: { passkey: { credentialId } },
        });
        lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedIdentity);
        authenticationV2Funnel.trigger(
          AuthenticationV2Events.ContinueAsPasskey,
        );
      } else if ("openid" in lastUsedIdentity.authMethod) {
        this.systemOverlay = true;
        const issuer = lastUsedIdentity.authMethod.openid.iss;
        const config = findConfig(
          issuer,
          // `aud` not tracked on `LastUsedIdentity`; see #3795. Falls back
          // to issuer-only matching.
          undefined,
          lastUsedIdentity.authMethod.openid.metadata ?? [],
        );
        if (config === undefined) {
          throw new Error(
            "OpenID authentication is not available for this account.",
          );
        }
        const requestConfig = {
          issuer,
          clientId: config.client_id,
          configURL: config.fedcm_uri[0],
          authURL: config.auth_uri,
          authScope: config.auth_scope.join(" "),
        };
        const jwt = await requestJWT(requestConfig, {
          nonce: get(sessionStore).nonce,
          mediation: "optional",
          loginHint: lastUsedIdentity.authMethod.openid.loginHint,
        });
        const { iss, sub } = decodeJWT(jwt);
        this.systemOverlay = false;
        const { identity, identityNumber } = await authenticateWithJWT({
          canisterId,
          session: get(sessionStore),
          jwt,
        });
        await authenticationStore.set({
          identity,
          identityNumber,
          authMethod: { openid: { iss, sub } },
        });
        lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedIdentity);
        authenticationV2Funnel.addProperties({
          provider: config.name,
        });
        authenticationV2Funnel.trigger(AuthenticationV2Events.ContinueAsOpenID);
      } else if ("sso" in lastUsedIdentity.authMethod) {
        this.systemOverlay = true;
        // SSO providers aren't in the static `openid_configs` list — the
        // canister only knows the discovery domain. Re-run the FE-side
        // two-hop chain so the request config is rebuilt from a fresh
        // provider discovery doc. No `add_discoverable_oidc_config` call
        // here: the only way to have a stored `sso` LastUsedIdentity is
        // to have completed an initial sign-up that already registered
        // the domain canister-side, and the canister persists those
        // registrations across upgrades.
        //
        // Calling `requestWithPopup` directly (rather than `requestJWT`)
        // with a `Promise<RequestConfig>` so the popup opens synchronously
        // in the same task as the user click — discovery resolves while
        // the popup shows about:blank, then navigates to the IdP. Awaiting
        // discovery before `window.open` would let Safari block the popup.
        const { domain, loginHint } = lastUsedIdentity.authMethod.sso;
        const jwt = await requestWithPopup(
          discoverSsoConfig(domain).then((ssoResult) => ({
            clientId: ssoResult.clientId,
            authURL: ssoResult.discovery.authorization_endpoint,
            authScope: selectAuthScopes(
              ssoResult.discovery.scopes_supported,
            ).join(" "),
          })),
          {
            nonce: get(sessionStore).nonce,
            mediation: "optional",
            loginHint,
          },
        );
        const { iss, sub } = decodeJWT(jwt);
        this.systemOverlay = false;
        const { identity, identityNumber } = await authenticateWithJWT({
          canisterId,
          session: get(sessionStore),
          jwt,
        });
        await authenticationStore.set({
          identity,
          identityNumber,
          authMethod: { openid: { iss, sub } },
        });
        lastUsedIdentitiesStore.addLastUsedIdentity(lastUsedIdentity);
        authenticationV2Funnel.addProperties({ provider: "SSO" });
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
