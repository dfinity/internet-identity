import { derived, type Readable, writable } from "svelte/store";
import type { AccessLevel } from "$lib/utils/accessLevel";

export type AuthorizationFlow =
  | { type: "1-click-openid"; issuer: string }
  | { type: "1-click-sso"; domain: string }
  | { type: "regular" };

export type AuthorizationContext = {
  effectiveOrigin?: string;
  flow?: AuthorizationFlow;
  /** The session duration the app requested (`maxTimeToLive`, nanoseconds), or
   *  `undefined` when the app didn't specify one. Surfaced to the UI so the
   *  sign-in screen can offer durations up to this value — the app's request is
   *  the ceiling the user picks under. */
  maxTimeToLive?: bigint;
};

export type Authorized = {
  accountNumberPromise: Promise<bigint | undefined>;
  /** The access level the user granted: "read-only" means the session
   *  delegation will carry `permissions = "queries"`, which makes the
   *  Internet Computer reject update calls authenticated through it.
   *  Enforcement is protocol-level, not up to the app or canister. */
  accessLevel: AccessLevel;
  /** The session duration the user chose (nanoseconds), at most the app's
   *  requested `maxTimeToLive`. `undefined` when the flow has no duration
   *  picker (e.g. 1-click OpenID/SSO), in which case the app's requested value
   *  is used as-is. */
  maxTimeToLive?: bigint;
};

const contextInternal = writable<AuthorizationContext | undefined>();
const authorizedInternal = writable<Authorized | undefined>();

export const authorizationStore = {
  /** Called by the channel store once the effective origin is resolved. */
  setEffectiveOrigin: (effectiveOrigin: string): void => {
    contextInternal.update((context) => ({ ...context, effectiveOrigin }));
  },
  /** Called by the UI as soon as the auth method is chosen — lets consumers
   *  react to the flow type (e.g. OpenID vs passkey) without waiting for
   *  the whole authorization to complete. */
  setFlow: (flow: AuthorizationFlow): void => {
    contextInternal.update((context) => ({ ...context, flow }));
  },
  /** Called by the channel handler once the delegation request is parsed, so
   *  the UI can offer session durations up to the app's requested value. */
  setMaxTimeToLive: (maxTimeToLive: bigint | undefined): void => {
    contextInternal.update((context) => ({ ...context, maxTimeToLive }));
  },
  /** Called by the UI when the user authorizes with a specific account.
   *  Accepts a promise so the animation can start immediately while the
   *  account number resolves asynchronously. `accessLevel` is the access
   *  the user granted (see {@link Authorized.accessLevel}); always passed
   *  explicitly so call sites are self-describing. `maxTimeToLive` is the
   *  duration the user chose (see {@link Authorized.maxTimeToLive}); omitted
   *  by flows without a duration picker. */
  authorize: (
    accountNumberPromise: Promise<bigint | undefined>,
    accessLevel: AccessLevel,
    maxTimeToLive?: bigint,
  ): void => {
    authorizedInternal.set({
      accountNumberPromise,
      accessLevel,
      maxTimeToLive,
    });
  },
  subscribe: contextInternal.subscribe,
};

/** Derived store that guarantees effectiveOrigin is available. */
export const authorizationContextStore: Readable<
  AuthorizationContext & { effectiveOrigin: string }
> = derived(contextInternal, (context) => {
  if (context?.effectiveOrigin === undefined) {
    throw new Error("Authorization context is not available yet");
  }
  return { ...context, effectiveOrigin: context.effectiveOrigin };
});

/** Store that holds the authorization outcome once the user has authorized. */
export const authorizedStore: Readable<Authorized | undefined> = {
  subscribe: authorizedInternal.subscribe,
};
