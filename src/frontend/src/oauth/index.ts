import { redirectInPopup } from "$src/flows/redirect";
import { Principal } from "@dfinity/principal";
import { isNullish, nonNullish } from "@dfinity/utils";

export interface RequestConfig {
  // OAuth client ID
  clientId: string;
  // OAuth client secret, some OpenID clients require it for every flow,
  // as long as PKCE is used, exposing this client secret is not a risk.
  clientSecret?: string;
  // OAuth authentication URL
  authURL: string;
  // Optional, FedCM config URL
  configURL?: string;
}

export interface RequestOptions {
  // Principal of the identity that makes the canister call with the JWT
  principal: Principal;
  // Optional, account identifier (e.g. email) used as login hint
  loginHint?: string;
  // Optional, see: https://developers.google.com/privacy-sandbox/blog/fedcm-auto-reauthn#mediation-options
  mediation?: CredentialMediationRequirement;
}

export const GOOGLE_REQUEST_CONFIG: RequestConfig = {
  clientId:
    "45431994619-cbbfgtn7o0pp0dpfcg2l66bc4rcg7qbu.apps.googleusercontent.com",
  // clientSecret: "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",
  authURL: "https://accounts.google.com/o/oauth2/v2/auth",
  // configURL: "https://accounts.google.com/gsi/fedcm.json",
};

const requestWithCredentials = async (
  config: Omit<RequestConfig, "authURL">,
  options: Omit<RequestOptions, "principal">,
  nonce: ArrayBuffer
): Promise<string | undefined> => {
  try {
    const identityCredential = await navigator.credentials.get({
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      identity: {
        providers: [
          {
            configURL: config.configURL,
            clientId: config.clientId,
            nonce: toBase64URL(nonce),
            loginHint: options.loginHint,
          },
        ],
      },
      mediation: options.mediation,
    });

    if (
      identityCredential?.type === "identity" &&
      "token" in identityCredential &&
      typeof identityCredential.token === "string"
    ) {
      return identityCredential.token;
    }
  } catch (error) {
    if (error instanceof Error && error.name == "NotSupportedError") {
      // FedCM is not supported in this browser
      return;
    }
    throw error;
  }
};

const toBase64 = (bytes: ArrayBuffer): string =>
  btoa(String.fromCharCode(...new Uint8Array(bytes)));

const toBase64URL = (bytes: ArrayBuffer): string =>
  toBase64(bytes).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");

const createRequestWithRedirect = async (
  config: Omit<RequestConfig, "configURL">,
  options: Omit<RequestOptions, "principal">,
  nonce: ArrayBuffer
): Promise<() => Promise<string>> => {
  const state = new Uint8Array(12);
  window.crypto.getRandomValues(state);

  const codeVerifier = new Uint8Array(96);
  window.crypto.getRandomValues(codeVerifier);

  const codeChallenge = await window.crypto.subtle.digest(
    "SHA-256",
    new TextEncoder().encode(toBase64URL(codeVerifier))
  );

  const redirectURL = new URL(window.location.origin);
  redirectURL.pathname = "/callback";

  const authURL = new URL(config.authURL);
  authURL.searchParams.set("response_type", "id_token");
  authURL.searchParams.set("client_id", config.clientId);
  authURL.searchParams.set("redirect_uri", redirectURL.href);
  authURL.searchParams.set("scope", "openid profile email");
  authURL.searchParams.set("state", toBase64URL(state));
  authURL.searchParams.set("state", toBase64URL(state));
  authURL.searchParams.set("nonce", toBase64URL(nonce));
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

  return async () => {
    const callback = await redirectInPopup(authURL.href);
    const callbackURL = new URL(callback);
    const searchParams = new URLSearchParams(callbackURL.hash);
    const id_token = searchParams.get("id_token");
    if (searchParams.get("state") === toBase64URL(state)) {
      throw new Error("Invalid state");
    }
    if (isNullish(id_token)) {
      throw new Error("No token received");
    }
    return id_token;
  };
};

export const createRequestJWT = async (
  config: RequestConfig,
  options: RequestOptions
): Promise<() => Promise<{ jwt: string; salt: ArrayBuffer }>> => {
  const salt = new Uint8Array(32);
  window.crypto.getRandomValues(salt);

  const bytes = new Uint8Array(
    32 + options.principal.toUint8Array().byteLength
  );
  bytes.set(salt);
  bytes.set(options.principal.toUint8Array(), 32);
  const nonce = await window.crypto.subtle.digest("SHA-256", bytes);

  const redirect = await createRequestWithRedirect(config, options, nonce);
  return async (): Promise<{ jwt: string; salt: ArrayBuffer }> => {
    if (isNullish(config.configURL)) {
      // FedCM is not supported for this OpenID Provider
      return { jwt: await redirect(), salt };
    }
    let jwt = await requestWithCredentials(config, options, nonce);
    if (isNullish(jwt)) {
      // FedCM is not supported in this browser
      jwt = await redirect();
    }
    return { jwt, salt };
  };
};
