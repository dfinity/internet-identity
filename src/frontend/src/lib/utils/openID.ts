import type {
  MetadataMapV2,
  OpenIdConfig,
} from "$lib/generated/internet_identity_types";
import { backendCanisterConfig } from "$lib/globals";
import { lookupSsoDomainForCredential } from "$lib/utils/ssoDomainStorage";
import { fromBase64URL, toBase64URL } from "$lib/utils/utils";
import { Principal } from "@icp-sdk/core/principal";
import {
  CallbackPopupClosedError,
  REDIRECT_CALLBACK_PATH,
  redirectInPopup,
} from "../../routes/(new-styling)/callback/utils";

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
    error instanceof CallbackPopupClosedError
  );
};

/**
 * Raised instead of the generic `OpenIdCredentialAlreadyRegistered` canister
 * error when the `(iss, sub, aud)` the user is trying to link is already
 * attached to the identity they're currently signed into. The UI shows a
 * more specific message ("…already linked to *this* identity") instead of
 * the generic "…linked to another identity" which would be misleading.
 */
export class OpenIdCredentialAlreadyLinkedHereError extends Error {
  constructor() {
    super("OpenID credential is already linked to this identity");
    this.name = "OpenIdCredentialAlreadyLinkedHereError";
  }
}

/**
 * Raised when an OAuth provider redirects back to II with an `error` (and
 * optional `error_description`) in the callback fragment — per RFC 6749
 * §4.1.2.1 / 4.2.2.1. Typical causes are the SSO app being misconfigured:
 *   • `unsupported_response_type` — the Okta/Auth0/etc. app doesn't allow
 *     the hybrid flow we request (`response_type=id_token code`).
 *   • `invalid_scope` — advertised scope wasn't actually granted.
 *   • `access_denied` — the user clicked "deny" on the consent screen.
 *
 * Carrying `error` and `errorDescription` separately (rather than just a
 * pre-formatted message) lets `mapSubmitError` in the SSO view produce a
 * UI string that points the user at the right knob to turn, instead of a
 * generic "No token received" that looks like a bug in II.
 */
export class OAuthProviderError extends Error {
  readonly error: string;
  readonly errorDescription?: string;
  constructor(error: string, errorDescription?: string) {
    const suffix =
      errorDescription !== undefined && errorDescription.length > 0
        ? `: ${errorDescription}`
        : "";
    super(`OAuth provider error: ${error}${suffix}`);
    this.name = "OAuthProviderError";
    this.error = error;
    this.errorDescription = errorDescription;
  }
}

/**
 * Create JWT request redirect flow URL
 * @param config of the OpenID provider
 * @param options for the JWT request
 */
export const createRedirectURL = (
  config: Omit<RequestConfig, "configURL">,
  options: RequestOptions,
): URL => {
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
  if (options.mediation === "required" && options.loginHint === undefined) {
    authURL.searchParams.set("prompt", "select_account");
  }
  if (options.mediation === "silent") {
    authURL.searchParams.set("prompt", "silent");
  }
  if (options.loginHint !== undefined) {
    authURL.searchParams.set("login_hint", options.loginHint);
  }

  return authURL;
};

/**
 * Parse the OAuth authorize callback URL and extract the `id_token`.
 *
 * Exported so `requestWithPopup` and tests can share a single source of
 * truth for how a callback fragment is interpreted. Throws:
 *   - `Error("Invalid state")` if the callback's `state` doesn't match
 *     `expectedState` (CSRF guard).
 *   - `OAuthProviderError` if the callback carries an `error=...`
 *     fragment (RFC 6749 §4.1.2.1 / 4.2.2.1) — checked BEFORE the
 *     `id_token` null-check so a misconfigured SSO app surfaces its
 *     own message instead of a generic "No token received" that looks
 *     like a bug in II.
 *   - `Error("No token received")` if neither `id_token` nor `error`
 *     was in the fragment (fallback for spec-violating providers).
 */
export const extractIdTokenFromCallback = (
  callback: string,
  expectedState: string,
): string => {
  const callbackURL = new URL(callback);
  const searchParams = new URLSearchParams(callbackURL.hash.slice(1));
  if (searchParams.get("state") !== expectedState) {
    throw new Error("Invalid state");
  }
  const error = searchParams.get("error");
  if (error !== null) {
    throw new OAuthProviderError(
      error,
      searchParams.get("error_description") ?? undefined,
    );
  }
  const id_token = searchParams.get("id_token");
  if (id_token === null) {
    throw new Error("No token received");
  }
  return id_token;
};

/**
 * Request JWT through redirect flow in a popup
 * @param config of the OpenID provider
 * @param options for the JWT request
 */
const requestWithPopup = async (
  config: Omit<RequestConfig, "configURL">,
  options: RequestOptions,
): Promise<string> => {
  const redirectURL = createRedirectURL(config, options);
  const callback = await redirectInPopup(redirectURL.href);
  const expectedState = redirectURL.searchParams.get("state");
  if (expectedState === null) {
    // `createRedirectURL` always sets `state`; reaching here means the
    // redirect URL was tampered with before it hit the popup.
    throw new Error("Missing state in redirect URL");
  }
  return extractIdTokenFromCallback(callback, expectedState);
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
    new Uint8Array(await window.crypto.subtle.digest("SHA-256", bytes)),
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
  return (
    config.configURL !== undefined &&
    config.configURL.length > 0 &&
    "IdentityCredential" in window
  );
};

/**
 * Build a concrete issuer URL from a configured issuer pattern and claims.
 *
 * Replaces any placeholder wrapped in curly braces, e.g. "{tid}", with the
 * corresponding value from the claims map. If any placeholder is missing in
 * the provided claims, the function returns `undefined` to indicate that a
 * valid issuer cannot be constructed.
 */
export const buildIssuerFromConfig = (
  configIssuer: string,
  metadata: MetadataMapV2,
): string | undefined => {
  if (!configIssuer.includes("{")) {
    return configIssuer;
  }
  let missing = false;
  const built = configIssuer.replace(/{([^}]+)}/g, (_match, name: string) => {
    const value = getMetadataString(metadata, name);
    if (typeof value === "string") return value;
    missing = true;
    return "";
  });
  return missing ? undefined : built;
};

/**
 * Extract claims from an issuer URL based on a configured issuer template.
 *
 * This function now only extracts the placeholder names present in the
 * configured issuer template.
 *
 * Example:
 *  template: "https://login.microsoftonline.com/{tid}/v2.0"
 *  returns:  ["tid"]
 */
export const extractIssuerTemplateClaims = (configIssuer: string): string[] => {
  // Detect placeholders of the form {name}
  const placeholderRegex = /{([^}]+)}/g;
  return Array.from(configIssuer.matchAll(placeholderRegex)).map((m) => m[1]);
};

/**
 * Compare an issuer URL against a config issuer pattern and claims.
 * If the config issuer contains placeholders in curly braces (e.g.,
 * "https://login.microsoftonline.com/{tid}/v2.0"),
 * extract the placeholders, create a new issuer URL with the claims values,
 * and perform a comparison.
 *
 * Otherwise, performs exact comparison.
 *
 * Exported for testing purposes.
 */
export const issuerMatches = (
  configIssuer: string,
  issuer: string,
  metadata: MetadataMapV2,
): boolean => buildIssuerFromConfig(configIssuer, metadata) === issuer;

/**
 * Find the OpenID configuration that issued a given credential.
 *
 * Resolution is strict-then-fallback:
 *
 * 1. **Strict `(issuer, aud)` match.** `aud` is the provider-assigned OAuth
 *    client_id and is the authoritative discriminator when two credentials
 *    share an issuer (e.g. direct-Google vs SSO-via-Google — same issuer,
 *    different client_id). If we find a config whose `client_id === aud`
 *    AND whose issuer template matches, return it.
 * 2. **Issuer-only fallback.** If no strict match, fall back to any config
 *    with a matching issuer. This covers:
 *      - Callers that don't track `aud` (e.g. `LastUsedIdentity`; see #3795).
 *      - Legacy credentials whose `aud` disagrees with the current
 *        `openid_configs` entry (client_id rotation, migration artifacts).
 *    SSO credentials would also match this fallback; {@link openIdName} and
 *    {@link openIdLogo} short-circuit via the localStorage SSO map before
 *    consulting `findConfig`, so the fallback doesn't mis-attribute SSO
 *    credentials that were linked on this device.
 *
 * Not relying on the feature flag ENABLE_GENERIC_OPEN_ID means that if we
 * enable and then disable the feature flag, users that used the generic
 * OpenID configurations can still sign in afterwards.
 *
 * @param issuer The issuer to find the configuration for.
 * @param aud    The OAuth client_id claim of the credential, if known.
 * @returns {OpenIdConfig | undefined} The configuration for the credential.
 */
export const findConfig = (
  issuer: string,
  aud: string | undefined,
  metadata: MetadataMapV2,
): OpenIdConfig | undefined => {
  const configs = backendCanisterConfig.openid_configs[0] ?? [];
  if (aud !== undefined) {
    const strict = configs.find(
      (config) =>
        config.client_id === aud &&
        issuerMatches(config.issuer, issuer, metadata),
    );
    if (strict !== undefined) return strict;
  }
  return configs.find((config) =>
    issuerMatches(config.issuer, issuer, metadata),
  );
};

/**
 * Pick the subset of OIDC scopes we actually request from what the provider
 * advertises. `openid` is required by the OIDC spec — nothing works without
 * it — so we always include it regardless of what the provider says it
 * supports. The other defaults (`profile`, `email`) are only included if the
 * provider advertises them.
 */
export const selectAuthScopes = (scopesSupported?: string[]): string[] => {
  const optional = ["profile", "email"];
  if (scopesSupported === undefined) {
    return ["openid", ...optional];
  }
  const filtered = scopesSupported.filter((s) => optional.includes(s));
  return ["openid", ...filtered];
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
    : await requestWithPopup(config, options);
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
  name?: string;
  email?: string;
  [key: string]: string | undefined;
} => {
  const [_header, body, _signature] = token.split(".");

  // JWT encodes the token using base64URL which is slightly different than base64.
  const payload = new TextDecoder().decode(fromBase64URL(body));

  const { iss, sub, aud, name, email, preferred_username, ...rest } =
    JSON.parse(payload);
  return {
    iss,
    sub,
    aud,
    // Login hint is usually preferred_username else fall back to email or even sub
    loginHint: preferred_username ?? email ?? sub,
    // Additional optional metadata claims
    name,
    email,
    ...rest,
  };
};

export const getMetadataString = (metadata: MetadataMapV2, key: string) => {
  const value = metadata.find((entry) => entry[0] === key)?.[1];
  return value && "String" in value ? value.String : undefined;
};

/**
 * Return the logo of the OpenID provider from the config.
 *
 * Returns `undefined` for credentials that this device remembers as having
 * been linked via SSO (so callers can render a generic SSO icon). For
 * everything else, defers to {@link findConfig} which does strict-then-
 * fallback matching so direct-provider credentials get their logo even
 * when `aud` disagrees with the current config.
 *
 * @returns {string | undefined} An SVG string to be embedded via `{@html}`.
 */
export const openIdLogo = (
  issuer: string,
  sub: string | undefined,
  aud: string | undefined,
  metadata: MetadataMapV2,
): string | undefined => {
  if (
    sub !== undefined &&
    aud !== undefined &&
    lookupSsoDomainForCredential({ iss: issuer, sub, aud }) !== undefined
  ) {
    return undefined;
  }
  const logo = findConfig(issuer, aud, metadata)?.logo;

  // To prevent rendering an element with the same id multiple times in the DOM,
  // we namespace all ids in the svg string using an unique suffix on each call.
  //
  // This is necessary because the logo is rendered in a list of identities and
  // multiple identities with the same provider can be present at the same time.
  return logo !== undefined ? namespaceIds(logo) : undefined;
};

/**
 * Safely namespaces all IDs inside an SVG string and updates all references,
 * to prevent collisions when rendering multiple SVGs with the same IDs in the DOM.
 *
 * @param input - Raw SVG string
 * @param suffix - Optional suffix to append to IDs (default: random UUID)
 * @returns Namespaced SVG string
 */
const namespaceIds = (
  input: string,
  suffix = globalThis.crypto.randomUUID(),
): string => {
  const parser = new DOMParser();
  const doc = parser.parseFromString(input, "text/html");

  const idMap: Record<string, string> = {};

  const elementsWithId = doc.querySelectorAll<HTMLElement | SVGElement>("[id]");
  elementsWithId.forEach((el) => {
    const oldId = el.id;
    const newId = `${oldId}-${suffix}`;
    el.id = newId;
    idMap[oldId] = newId;
  });

  const walker = doc.createTreeWalker(doc.body, NodeFilter.SHOW_ELEMENT);

  while (walker.nextNode()) {
    const el = walker.currentNode as Element;

    for (const attr of el.getAttributeNames()) {
      let value = el.getAttribute(attr);
      if (value === null) {
        continue;
      }

      for (const oldId in idMap) {
        const newId = idMap[oldId];

        value = value
          .replace(new RegExp(`url\\(#${oldId}\\)`, "g"), `url(#${newId})`)
          .replace(new RegExp(`^#${oldId}$`), `#${newId}`);
      }

      el.setAttribute(attr, value);
    }
  }

  return doc.body.innerHTML;
};

/**
 * Return a human-readable name for an OpenID credential.
 *
 * Resolution order:
 * 1. **SSO on this device** — if we remember the `discovery_domain` the
 *    user entered when linking (stored by {@link rememberSsoDomainForCredential}),
 *    return that domain. Runs BEFORE direct-provider lookup so SSO
 *    credentials don't get falsely attributed to their underlying IdP.
 * 2. **Direct provider** — {@link findConfig} resolves strict-then-fallback,
 *    so we get the provider's configured `name` (e.g. "Google") for both
 *    credentials whose `aud` matches the config and legacy credentials
 *    whose `aud` disagrees but whose issuer matches.
 * 3. `undefined` — caller should render a generic fallback.
 */
export const openIdName = (
  issuer: string,
  sub: string | undefined,
  aud: string | undefined,
  metadata: MetadataMapV2,
): string | undefined => {
  if (sub !== undefined && aud !== undefined) {
    const domain = lookupSsoDomainForCredential({ iss: issuer, sub, aud });
    if (domain !== undefined) return domain;
  }
  return findConfig(issuer, aud, metadata)?.name;
};
