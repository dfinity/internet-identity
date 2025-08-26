import type {
  GoogleOpenIdConfig,
  MetadataMapV2,
  OpenIdConfig,
} from "$lib/generated/internet_identity_types";
import { canisterConfig } from "$lib/globals";
import {
  PopupClosedError,
  REDIRECT_CALLBACK_PATH,
  redirectInPopup,
} from "$lib/legacy/flows/redirect";
import { ENABLE_GENERIC_OPEN_ID } from "$lib/state/featureFlags";
import { toBase64URL } from "$lib/utils/utils";
import { Principal } from "@dfinity/principal";
import { isNullish, nonNullish } from "@dfinity/utils";
import { get } from "svelte/store";

export interface RequestConfig {
  // OAuth client ID
  clientId: string;
  // OAuth authentication URL
  authURL: string;
  // OAuth authentication scope
  authScope: string;
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

export const createGoogleRequestConfig = (clientId: string): RequestConfig => ({
  clientId,
  authURL: "https://accounts.google.com/o/oauth2/v2/auth",
  authScope: "openid profile email",
  configURL: "https://accounts.google.com/gsi/fedcm.json",
});

/**
 * Request JWT with the FedCM API
 * @param config of the OpenID provider
 * @param options for the JWT request
 */
const requestWithCredentials = async (
  config: Omit<RequestConfig, "authURL">,
  options: RequestOptions,
): Promise<string> => {
  const identityCredential = await navigator.credentials.get({
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    identity: {
      context: "use",
      providers: [
        {
          configURL: config.configURL,
          clientId: config.clientId,
          nonce: options.nonce,
          loginHint: options.loginHint,
        },
      ],
      mode: "active",
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
export const isOpenIdCancelError = (error: unknown) => {
  return (
    (error instanceof Error && error.name === "NetworkError") ||
    error instanceof PopupClosedError
  );
};

/**
 * Request JWT through redirect flow in a popup
 * @param config of the OpenID provider
 * @param options for the JWT request
 */
const requestWithRedirect = async (
  config: Omit<RequestConfig, "configURL">,
  options: RequestOptions,
): Promise<string> => {
  const state = toBase64URL(window.crypto.getRandomValues(new Uint8Array(12)));
  const redirectURL = new URL(REDIRECT_CALLBACK_PATH, window.location.origin);
  const authURL = new URL(config.authURL);
  // Even though we only need an id token, we're still asking for a code
  // because some identity providers (AppleID) will throw an error otherwise.
  authURL.searchParams.set("response_type", "code id_token");
  authURL.searchParams.set("response_mode", "fragment");
  authURL.searchParams.set("client_id", config.clientId);
  authURL.searchParams.set("redirect_uri", redirectURL.href);
  authURL.searchParams.set("scope", config.authScope);
  authURL.searchParams.set("state", state);
  authURL.searchParams.set("nonce", options.nonce);
  if (options.mediation === "required" && isNullish(options.loginHint)) {
    authURL.searchParams.set("prompt", "select_account");
  }
  if (options.mediation === "silent") {
    authURL.searchParams.set("prompt", "silent");
  }
  if (nonNullish(options.loginHint)) {
    authURL.searchParams.set("login_hint", options.loginHint);
  }

  const callback = await redirectInPopup(authURL.href);
  const callbackURL = new URL(callback);
  const searchParams = new URLSearchParams(callbackURL.hash.slice(1));
  const id_token = searchParams.get("id_token");
  if (searchParams.get("state") !== state) {
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
  principal: Principal,
): Promise<{ nonce: string; salt: Uint8Array }> => {
  const salt = window.crypto.getRandomValues(new Uint8Array(32));
  const bytes = new Uint8Array(32 + principal.toUint8Array().byteLength);
  bytes.set(salt);
  bytes.set(principal.toUint8Array(), 32);
  const nonce = toBase64URL(
    await window.crypto.subtle.digest("SHA-256", bytes),
  );
  return { nonce, salt };
};

/**
 * Check if FedCM is supported based on user agent and config
 * @param userAgent browser user agent string
 * @param config of the OpenID provider
 * @returns boolean indicating if FedCM is supported
 */
export const isFedCMSupported = (
  userAgent: string,
  config: RequestConfig,
): boolean => {
  // Samsung browser runs an older version of FedCM not compatible with our params.
  const isSamsungBrowser = /SamsungBrowser/i.test(userAgent);
  if (isSamsungBrowser) {
    return false;
  }
  return nonNullish(config.configURL) && "IdentityCredential" in window;
};

export const findConfig = (
  issuer: string,
): OpenIdConfig | GoogleOpenIdConfig | undefined => {
  const genericOpenIdEnabled = get(ENABLE_GENERIC_OPEN_ID);
  return !genericOpenIdEnabled &&
    nonNullish(canisterConfig.openid_google?.[0]?.[0]) &&
    issuer === "https://accounts.google.com"
    ? canisterConfig.openid_google?.[0]?.[0]
    : canisterConfig.openid_configs?.[0]?.find(
        (config) => config.issuer === issuer,
      );
};

export const isOpenIdConfig = (
  config: GoogleOpenIdConfig | OpenIdConfig,
): config is OpenIdConfig => {
  return "auth_scope" in config;
};

/**
 * Request JWT token through FedCM with redirect in a popup as fallback
 * @param config of the OpenID provider
 * @param options for the JWT request
 */
export const requestJWT = async (
  config: RequestConfig,
  options: RequestOptions,
): Promise<string> => {
  const supportsFedCM = isFedCMSupported(navigator.userAgent, config);
  const jwt = supportsFedCM
    ? await requestWithCredentials(config, options)
    : await requestWithRedirect(config, options);
  return jwt;
};

/**
 * Decode a JWT token so it can be compared with others
 * @param token to decode
 * @returns common claims
 */
export const decodeJWT = (
  token: string,
): {
  iss: string;
  sub: string;
  aud: string;
  loginHint: string;
} => {
  const [_header, body, _signature] = token.split(".");
  const { iss, sub, aud, email, preferred_username } = JSON.parse(atob(body));
  // Login hint is usually preferred_username else fall back to email or even sub
  return { iss, sub, aud, loginHint: preferred_username ?? email ?? sub };
};

/**
 * Decode a JWT token so it can be compared with others and the name can be displayed
 * @param token to decode
 * @returns common claims
 */
export const decodeJWTWithNameAndEmail = (
  token: string,
): { iss: string; sub: string; aud: string; name: string; email: string } => {
  const [_header, body, _signature] = token.split(".");
  const { iss, sub, aud, name, email } = JSON.parse(atob(body));
  return { iss, sub, aud, name, email };
};

export const getMetadataString = (metadata: MetadataMapV2, key: string) => {
  const value = metadata.find((entry) => entry[0] === key)?.[1];
  return value && "String" in value ? value.String : undefined;
};

/**
 * Return the logo of the OpenID provider from the config.
 * Returns `undefined` if it's a google config or not found.
 * @param issuer
 * @returns {string | undefined} The string is an SVG string that must be embedded in the HTML.
 */
export const openIdLogo = (issuer: string): string | undefined => {
  const config = findConfig(issuer);
  if (nonNullish(config) && isOpenIdConfig(config)) {
    return config.logo;
  }
  // If it's a google config or not found, return `undefined`
  return undefined;
};

/**
 * Return the name of the OpenID provider from the config.
 * Returns `undefined` if it's a google config or not found.
 * @param issuer
 * @returns {string | undefined} The string is the name of the OpenID provider.
 */
export const openIdName = (issuer: string): string | undefined => {
  const config = findConfig(issuer);
  if (nonNullish(config) && isOpenIdConfig(config)) {
    return config.name;
  }
  // If it's a google config or not found, return `undefined`
  return undefined;
};
