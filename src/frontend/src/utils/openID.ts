import { MetadataMapV2 } from "$generated/internet_identity_types";
import { II_OPENID_GOOGLE_CLIENT_ID } from "$src/environment";
import { REDIRECT_CALLBACK_PATH, redirectInPopup } from "$src/flows/redirect";
import { Principal } from "@dfinity/principal";
import { isNullish, nonNullish } from "@dfinity/utils";

export interface RequestConfig {
  // OAuth client ID
  clientId: string;
  // OAuth authentication URL
  authURL: string;
  // Optional, FedCM config URL
  configURL?: string;
}

export interface RequestOptions {
  // Nonce created from a principal with `createAnonymousNonce`
  nonce: string;
  // Optional, account identifier (e.g. email) used as login hint
  loginHint?: string;
  // Optional, see: https://developers.google.com/privacy-sandbox/blog/fedcm-auto-reauthn#mediation-options
  mediation?: CredentialMediationRequirement;
}

export const GOOGLE_REQUEST_CONFIG: RequestConfig = {
  clientId: II_OPENID_GOOGLE_CLIENT_ID,
  authURL: "https://accounts.google.com/o/oauth2/v2/auth",
  configURL: "https://accounts.google.com/gsi/fedcm.json",
};

/**
 * Request JWT with the FedCM API
 * @param config of the OpenID provider
 * @param options for the JWT request
 */
const requestWithCredentials = async (
  config: Omit<RequestConfig, "authURL">,
  options: RequestOptions
): Promise<string> => {
  const identityCredential = await navigator.credentials.get({
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    identity: {
      providers: [
        {
          configURL: config.configURL,
          clientId: config.clientId,
          nonce: options.nonce,
          loginHint: options.loginHint,
        },
      ],
    },
    mediation: options.mediation,
  });

  if (
    identityCredential?.type !== "identity" ||
    !("token" in identityCredential) ||
    typeof identityCredential.token !== "string"
  ) {
    // This should be unreachable in FedCM spec compliant browsers
    throw new Error("Invalid credential received from FedCM API");
  }

  return identityCredential.token;
};

/**
 * @param error to check whether it is a FedCM not supported error
 */
export const isNotSupportedError = (error: unknown) =>
  error instanceof Error && error.name === "NotSupportedError";

/**
 * @param error to check whether it is a FedCM no permission error
 */
export const isPermissionError = (error: unknown) =>
  error instanceof Error && error.name === "NetworkError";

const toBase64 = (bytes: ArrayBuffer): string =>
  btoa(String.fromCharCode(...new Uint8Array(bytes)));

const toBase64URL = (bytes: ArrayBuffer): string =>
  toBase64(bytes).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");

/**
 * Request JWT through redirect flow in a popup
 * @param config of the OpenID provider
 * @param options for the JWT request
 */
const requestWithRedirect = async (
  config: Omit<RequestConfig, "configURL">,
  options: RequestOptions
): Promise<string> => {
  const state = toBase64URL(window.crypto.getRandomValues(new Uint8Array(12)));
  const redirectURL = new URL(REDIRECT_CALLBACK_PATH, window.location.origin);
  const authURL = new URL(config.authURL);
  authURL.searchParams.set("response_type", "id_token");
  authURL.searchParams.set("client_id", config.clientId);
  authURL.searchParams.set("redirect_uri", redirectURL.href);
  authURL.searchParams.set("scope", "openid profile email");
  authURL.searchParams.set("state", state);
  authURL.searchParams.set("nonce", options.nonce);
  if (options.mediation === "required") {
    authURL.searchParams.set(
      "prompt",
      isNullish(options.loginHint) ? "select_account" : "login"
    );
  }
  if (options.mediation === "silent") {
    authURL.searchParams.set("prompt", "silent");
  }
  if (nonNullish(options.loginHint)) {
    authURL.searchParams.set("login_hint", options.loginHint);
  }

  const callback = await redirectInPopup(authURL.href);
  const callbackURL = new URL(callback);
  const searchParams = new URLSearchParams(callbackURL.hash);
  const id_token = searchParams.get("id_token");
  if (searchParams.get("state") === state) {
    throw new Error("Invalid state");
  }
  if (isNullish(id_token)) {
    throw new Error("No token received");
  }

  return id_token;
};

/**
 * Create a salt and use it to anonymize the principal before using it as a
 * nonce to make sure the principal identity and JWT are bound together.
 *
 * The II canister will not accept incoming JWT tokens calls where the caller
 * is not equal to the principal that has been bound to the JWT token.
 * @param principal to anonymize with a salt
 */
export const createAnonymousNonce = async (
  principal: Principal
): Promise<{ nonce: string; salt: Uint8Array }> => {
  const salt = window.crypto.getRandomValues(new Uint8Array(32));
  const bytes = new Uint8Array(32 + principal.toUint8Array().byteLength);
  bytes.set(salt);
  bytes.set(principal.toUint8Array(), 32);
  const nonce = toBase64URL(
    await window.crypto.subtle.digest("SHA-256", bytes)
  );
  return { nonce, salt };
};

/**
 * Request JWT token through FedCM with redirect in a popup as fallback
 * @param config of the OpenID provider
 * @param options for the JWT request
 */
export const requestJWT = (
  config: RequestConfig,
  options: RequestOptions
): Promise<string> => {
  if (isNullish(config.configURL)) {
    // FedCM is not supported for this OpenID Provider
    return requestWithRedirect(config, options);
  }
  try {
    return requestWithCredentials(config, options);
  } catch (error) {
    if (isNotSupportedError(error)) {
      // FedCM is not supported in this browser
      return requestWithRedirect(config, options);
    }
    throw error;
  }
};

/**
 * Decode a JWT token so it can be compared with others
 * @param token to decode
 * @returns common claims
 */
export const decodeJWT = (
  token: string
): { iss: string; sub: string; aud: string } => {
  const [_header, body, _signature] = token.split(".");
  const { iss, sub, aud } = JSON.parse(atob(body));
  return { iss, sub, aud };
};

export const getMetadataString = (metadata: MetadataMapV2, key: string) => {
  const value = metadata.find((entry) => entry[0] === key)?.[1];
  return value && "String" in value ? value.String : undefined;
};
