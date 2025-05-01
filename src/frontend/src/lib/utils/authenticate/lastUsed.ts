import { Principal } from "@dfinity/principal";
import { HttpAgent } from "@dfinity/agent";
import { DelegationIdentity } from "@dfinity/identity";
import { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import { authenticateWithPasskey } from "$lib/utils/authenticate/passkey";
import {
  createGoogleRequestConfig,
  decodeJWT,
  requestJWT,
} from "$lib/utils/openID";
import { InternetIdentityInit } from "$lib/generated/internet_identity_types";
import { isNullish } from "@dfinity/utils";
import { authenticateWithJWT } from "$lib/utils/authenticate/jwt";

export class IncorrectAccountError extends Error {
  constructor() {
    super();
    Object.setPrototypeOf(this, IncorrectAccountError.prototype);
  }

  message = "The account does not match the last used account";
}

export class UnknownAuthenticationMethodError extends Error {
  constructor() {
    super();
    Object.setPrototypeOf(this, UnknownAuthenticationMethodError.prototype);
  }

  message =
    "The authentication method in the last used account is not recognized";
}

export const authenticateWithLastUsed = async ({
  canisterId,
  config,
  session,
  lastUsed,
  expiration = 30 * 60 * 1000,
}: {
  canisterId: Principal;
  config: Pick<InternetIdentityInit, "openid_google">;
  session: {
    agent: HttpAgent;
    nonce: string;
    salt: Uint8Array;
  };
  lastUsed: LastUsedIdentity;
  expiration?: number;
}): Promise<{
  identity: DelegationIdentity;
  anchorNumber: bigint;
}> => {
  if ("passkey" in lastUsed.authMethod) {
    return authenticateWithPasskey({
      canisterId,
      agent: session.agent,
      expiration,
      credentialId: lastUsed.authMethod.passkey.credentialId,
    });
  }
  if (
    "openid" in lastUsed.authMethod &&
    lastUsed.authMethod.openid.iss === "https://accounts.google.com"
  ) {
    const clientId = config.openid_google?.[0]?.[0]?.client_id;
    if (isNullish(clientId)) {
      throw new Error("OpenID Google config is missing");
    }
    const requestConfig = createGoogleRequestConfig(clientId);
    const jwt = await requestJWT(requestConfig, {
      nonce: session.nonce,
      mediation: "required",
      loginHint: lastUsed.authMethod.openid.sub,
    });
    const { sub } = decodeJWT(jwt);
    if (sub !== lastUsed.authMethod.openid.sub) {
      throw new IncorrectAccountError();
    }
    return authenticateWithJWT({
      canisterId,
      agent: session.agent,
      jwt,
      salt: session.salt,
    });
  }
  throw new UnknownAuthenticationMethodError();
};
