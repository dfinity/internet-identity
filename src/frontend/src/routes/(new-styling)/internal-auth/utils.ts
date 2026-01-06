import {
  DelegationChain,
  DelegationIdentity,
  JsonnableDelegationChain,
} from "@icp-sdk/core/identity";
import type { Authenticated } from "$lib/stores/authentication.store";
import { fromBase64, toBase64 } from "$lib/utils/utils";
import { sessionStore } from "$lib/stores/session.store";
import { get } from "svelte/store";
import { Principal } from "@icp-sdk/core/principal";
import {
  waitForWindowReadyRequest,
  waitForWindowReadyResponse,
} from "$lib/utils/internalPostMessage";

interface JsonnableAuthenticated {
  identityNumber: string;
  delegation: JsonnableDelegationChain;
  authMethod:
    | { passkey: { credentialId: string } }
    | { openid: { iss: string; sub: string } }
    | { recoveryPhrase: { principal: string } };
}

/**
 * Map `Authenticated` object to jsonnable object
 * @param authenticated object
 * @return jssonable object
 */
export const authenticatedToJson = (
  authenticated: Omit<Authenticated, "agent" | "actor" | "salt" | "nonce">,
): JsonnableAuthenticated => {
  const identityNumber = authenticated.identityNumber.toString();
  const delegation = authenticated.identity.getDelegation().toJSON();
  let authMethod: JsonnableAuthenticated["authMethod"];
  if ("passkey" in authenticated.authMethod) {
    authMethod = {
      passkey: {
        credentialId: toBase64(
          new Uint8Array(authenticated.authMethod.passkey.credentialId).buffer,
        ),
      },
    };
  } else if ("openid" in authenticated.authMethod) {
    authMethod = authenticated.authMethod;
  } else if ("recoveryPhrase" in authenticated.authMethod) {
    authMethod = {
      recoveryPhrase: {
        principal: authenticated.authMethod.recoveryPhrase.principal.toText(),
      },
    };
  } else {
    throw new Error("Unknown authentication method");
  }
  return { identityNumber, delegation, authMethod };
};

/**
 * Reconstruct `Authenticated` object from json.
 * @param jsonnable object
 * @return `Authenticated` object
 */
export const authenticatedFromJson = (
  json: JsonnableAuthenticated,
): Omit<Authenticated, "agent" | "actor" | "salt" | "nonce"> => {
  const identityNumber = BigInt(json.identityNumber);
  const delegation = DelegationChain.fromJSON(json.delegation);
  const identity = DelegationIdentity.fromDelegation(
    get(sessionStore).identity,
    delegation,
  );
  let authMethod: Authenticated["authMethod"];
  if ("passkey" in json.authMethod) {
    authMethod = {
      passkey: {
        credentialId: fromBase64(json.authMethod.passkey.credentialId),
      },
    };
  } else if ("openid" in json.authMethod) {
    authMethod = json.authMethod;
  } else if ("recoveryPhrase" in json.authMethod) {
    authMethod = {
      recoveryPhrase: {
        principal: Principal.fromText(json.authMethod.recoveryPhrase.principal),
      },
    };
  } else {
    throw new Error("Unknown authentication method");
  }
  return { identityNumber, identity, authMethod };
};

export interface InternalAuthRequest {
  ii_auth_request: {
    id: string;
  };
}

export interface InternalAuthResponse {
  ii_auth_response: {
    id: string;
    authenticated: JsonnableAuthenticated;
  };
}

export const isInternalAuthRequest = (
  data: unknown,
): data is InternalAuthRequest =>
  typeof data === "object" && data !== null && "ii_auth_request" in data;

export const isInternalAuthResponse = (
  data: unknown,
): data is InternalAuthResponse =>
  typeof data === "object" && data !== null && "ii_auth_response" in data;

/**
 * Open authenticated window, sends authentication state to window
 * @param pathname to navigate to in new window
 * @param authenticated state to send to new window
 */
export const openWindowWithAuth = async (
  pathname: string,
  authenticated: Omit<Authenticated, "agent" | "actor" | "salt" | "nonce">,
): Promise<void> => {
  const manageURL = new URL("/internal-auth", window.location.origin);
  manageURL.searchParams.set("next", pathname);
  const manageWindow = window.open(
    window.location.origin + "/internal-auth",
    "_blank",
  );
  if (manageWindow === null) {
    return;
  }
  await waitForWindowReadyResponse(manageWindow, window.location.origin);
  const listener = (event: MessageEvent) => {
    if (
      event.origin !== window.location.origin ||
      event.source !== manageWindow ||
      !isInternalAuthRequest(event.data)
    ) {
      return;
    }
    window.removeEventListener("message", listener);
    const message: InternalAuthResponse = {
      ii_auth_response: {
        id: event.data.ii_auth_request.id,
        authenticated: authenticatedToJson(authenticated),
      },
    };
    manageWindow.postMessage(message, window.location.origin);
  };
  window.addEventListener("message", listener);
};

/**
 * Request authentication state from window opener through PostMessage
 */
export const requestAuthFromOpener = async (): Promise<
  Omit<Authenticated, "agent" | "actor" | "salt" | "nonce">
> => {
  await waitForWindowReadyRequest(window.opener, [window.location.origin]);
  return new Promise((resolve) => {
    const id = window.crypto.randomUUID();
    const message: InternalAuthRequest = { ii_auth_request: { id } };
    const listener = (event: MessageEvent) => {
      if (
        event.origin !== window.location.origin ||
        event.source !== window.opener ||
        !isInternalAuthResponse(event.data) ||
        event.data.ii_auth_response.id !== id
      ) {
        return;
      }
      window.removeEventListener("message", listener);
      const authenticated = authenticatedFromJson(
        event.data.ii_auth_response.authenticated,
      );
      resolve(authenticated);
    };
    window.addEventListener("message", listener);
    window.opener.postMessage(message, window.location.origin);
  });
};
