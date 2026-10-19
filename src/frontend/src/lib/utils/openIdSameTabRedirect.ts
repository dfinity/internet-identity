/**
 * Same-tab OpenID sign-in for the auth wizard.
 *
 * The wizard normally runs an OpenID provider in a popup (`requestWithPopup`
 * in `openID.ts`). On WebKit that does not work for Sign in with Apple:
 * Safari (and every browser on iOS and iPadOS, which are all WebKit) hands a
 * `window.open` to an Apple ID authorization URL over to the system's
 * Sign in with Apple dialog instead of opening a popup. The frontend is then
 * waiting on a popup that either never exists or is torn down without the
 * callback ever reaching it, and the sign-in ends as "Interaction canceled".
 *
 * A same-tab navigation goes through the same system dialog, and WebKit then
 * loads the IdP's response — the canister's POST /callback landing page — into
 * this very tab, with its sessionStorage intact. That is what the authorize
 * page's 1-click flow already relies on. This module gives the wizard the
 * same round trip:
 *
 * 1. {@link redirectToOpenIdInSameTab} stashes what the wizard needs to pick
 *    the sign-in back up (the expected CSRF `state`, which provider, which
 *    mode) and the in-app path to return to, then navigates to the IdP.
 * 2. The landing page sees the return-path marker, stashes the IdP's payload
 *    and navigates back to that path.
 * 3. On that page, {@link consumeOpenIdSameTabResume} validates the payload
 *    against the stashed `state`, resolves the provider, and hands the JWT to
 *    the wizard, which continues exactly as if the popup had delivered it.
 *
 * The session key (and so the nonce the JWT is bound to) survives the round
 * trip through `sessionStore`'s own sessionStorage persistence.
 */
import { get } from "svelte/store";
import { z } from "zod";
import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
import { backendCanisterConfig } from "$lib/globals";
import {
  APPLE_SAME_TAB_REDIRECT,
  OPENID_SAME_TAB_REDIRECT,
} from "$lib/state/featureFlags";
import { sessionStore } from "$lib/stores/session.store";
import type { AuthMode } from "$lib/flows/authFlow.svelte";
import {
  clearOpenIdRedirectMarkers,
  OPENID_CALLBACK_DATA_KEY,
  OPENID_RETURN_PATH_KEY,
  OPENID_WIZARD_RESUME_KEY,
} from "$lib/utils/openIdRedirectMarkers";
import {
  createRedirectURL,
  extractIdTokenFromCallback,
} from "$lib/utils/openID";

/** The host of Apple's OpenID authorization endpoint. */
export const APPLE_AUTH_HOST = "appleid.apple.com";

/**
 * How long a stashed resume stays valid. Generous enough for a slow IdP
 * round trip, short enough that a tab reopened days later does not replay a
 * sign-in the user has long forgotten about. The session key the JWT is
 * bound to expires after 5 minutes anyway.
 */
const RESUME_MAX_AGE_MS = 5 * 60 * 1000;

/**
 * Whether this browser runs on WebKit.
 *
 * Every browser on iOS and iPadOS is WebKit, whatever it calls itself, and an
 * iPad in desktop mode sends a Mac user agent with touch points as the only
 * tell. On other systems only Safari is: every other browser that carries the
 * `Safari/` token also names its own engine.
 */
export const isWebKitBrowser = (
  userAgent: string,
  maxTouchPoints: number,
): boolean => {
  if (/iPhone|iPad|iPod/.test(userAgent)) {
    return true;
  }
  if (/Macintosh|Mac OS X/.test(userAgent) && maxTouchPoints > 1) {
    return true;
  }
  return (
    /Safari\//.test(userAgent) &&
    /Version\//.test(userAgent) &&
    !/Chrome\/|Chromium\/|CriOS\/|Edg|OPR\/|FxiOS\/|SamsungBrowser\//.test(
      userAgent,
    )
  );
};

/**
 * Whether the wizard should run this provider as a same-tab redirect rather
 * than in a popup.
 *
 * `OPENID_SAME_TAB_REDIRECT` forces it for every provider and browser;
 * otherwise it is Sign in with Apple on WebKit, unless
 * `APPLE_SAME_TAB_REDIRECT` was switched off.
 */
export const shouldRedirectInSameTab = (
  authURL: string,
  {
    userAgent = navigator.userAgent,
    maxTouchPoints = navigator.maxTouchPoints,
  }: { userAgent?: string; maxTouchPoints?: number } = {},
): boolean => {
  if (get(OPENID_SAME_TAB_REDIRECT)) {
    return true;
  }
  if (!get(APPLE_SAME_TAB_REDIRECT)) {
    return false;
  }
  let host: string;
  try {
    host = new URL(authURL).hostname;
  } catch {
    return false;
  }
  return host === APPLE_AUTH_HOST && isWebKitBrowser(userAgent, maxTouchPoints);
};

const ResumeRecordSchema = z.object({
  state: z.string(),
  issuer: z.string(),
  clientId: z.string(),
  mode: z.enum(["signin", "signup", "both"]),
  startedAt: z.number(),
});
type ResumeRecord = z.infer<typeof ResumeRecordSchema>;

/** What the wizard needs to continue a same-tab sign-in on return. */
export interface OpenIdSameTabResume {
  config: OpenIdConfig;
  jwt: string;
  mode: AuthMode;
}

export interface RedirectToOpenIdInSameTabOptions {
  config: OpenIdConfig;
  mode: AuthMode;
  /** In-app path (with query, if any) to come back to. Defaults to "/". */
  returnPath?: string;
  /** Injected for tests; defaults to navigating this tab. */
  navigate?: (url: URL) => void;
}

/**
 * Leave for the IdP in this tab, to come back to `returnPath` with the
 * callback payload.
 *
 * Never resolves on the happy path: the document unloads. Callers that want
 * to keep a "busy" state up until then can await the returned promise.
 */
export const redirectToOpenIdInSameTab = ({
  config,
  mode,
  returnPath = "/",
  navigate = (url) => window.location.assign(url),
}: RedirectToOpenIdInSameTabOptions): Promise<never> => {
  const url = createRedirectURL(
    {
      clientId: config.client_id,
      authURL: config.auth_uri,
      authScope: config.auth_scope.join(" "),
    },
    { nonce: get(sessionStore).nonce, mediation: "required" },
  );
  const state = url.searchParams.get("state");
  if (state === null) {
    // `createRedirectURL` always sets one.
    throw new Error("Missing state in redirect URL");
  }
  const record: ResumeRecord = {
    state,
    issuer: config.issuer,
    clientId: config.client_id,
    mode,
    startedAt: Date.now(),
  };
  // One flow at a time: whatever another flow left behind would otherwise
  // misroute this one's callback.
  clearOpenIdRedirectMarkers();
  sessionStorage.setItem(OPENID_WIZARD_RESUME_KEY, JSON.stringify(record));
  sessionStorage.setItem(OPENID_RETURN_PATH_KEY, returnPath);
  navigate(url);
  return new Promise<never>(() => {});
};

/** Whether this tab holds a same-tab sign-in waiting to be resumed. */
export const hasPendingOpenIdSameTabResume = (): boolean => {
  try {
    return (
      sessionStorage.getItem(OPENID_WIZARD_RESUME_KEY) !== null &&
      sessionStorage.getItem(OPENID_CALLBACK_DATA_KEY) !== null
    );
  } catch {
    return false;
  }
};

/**
 * Take the same-tab sign-in waiting in this tab, if any.
 *
 * Single-use: the stash is cleared before anything is validated, so a
 * failure can't replay. Returns `undefined` when nothing is pending, when the
 * stash is stale or malformed, or when the provider is no longer configured.
 * Throws what `extractIdTokenFromCallback` throws (an `OAuthProviderError`
 * for a provider-reported error, an `Error` for a CSRF `state` mismatch or a
 * missing token) so the page can surface it.
 */
export const consumeOpenIdSameTabResume = ():
  OpenIdSameTabResume | undefined => {
  const rawRecord = sessionStorage.getItem(OPENID_WIZARD_RESUME_KEY);
  const rawPayload = sessionStorage.getItem(OPENID_CALLBACK_DATA_KEY);
  clearOpenIdRedirectMarkers();
  if (rawRecord === null || rawPayload === null) {
    return undefined;
  }
  let record: ResumeRecord;
  let payload: unknown;
  try {
    record = ResumeRecordSchema.parse(JSON.parse(rawRecord));
    payload = JSON.parse(rawPayload);
  } catch {
    return undefined;
  }
  if (Date.now() - record.startedAt > RESUME_MAX_AGE_MS) {
    return undefined;
  }
  const config = findSameTabConfig(record.issuer, record.clientId);
  if (config === undefined) {
    return undefined;
  }
  const jwt = extractIdTokenFromCallback(payload, record.state);
  return { config, jwt, mode: record.mode };
};

/**
 * The configured provider the redirect was started with. Matched on both
 * issuer and client id, since two configs can share an issuer; falls back to
 * the issuer alone in case the client id was rotated meanwhile.
 */
const findSameTabConfig = (
  issuer: string,
  clientId: string,
): OpenIdConfig | undefined => {
  const configs = backendCanisterConfig.openid_configs?.[0] ?? [];
  return (
    configs.find(
      (config) => config.issuer === issuer && config.client_id === clientId,
    ) ?? configs.find((config) => config.issuer === issuer)
  );
};
