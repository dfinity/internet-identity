import { derived, type Readable, writable } from "svelte/store";

export type AuthorizationFlow =
  | { type: "1-click-openid"; issuer: string }
  | { type: "1-click-sso"; domain: string }
  | { type: "regular" };

export type AuthorizationContext = {
  effectiveOrigin?: string;
  flow?: AuthorizationFlow;
};

export type Authorized = {
  accountNumberPromise: Promise<bigint | undefined>;
  /** Whether the user restricted this authorization to read-only access:
   *  the session delegation will carry `permissions = "queries"`, which
   *  makes the Internet Computer reject update calls authenticated through
   *  it. Enforcement is protocol-level, not up to the app or canister. */
  readOnly: boolean;
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
  /** Called by the UI when the user authorizes with a specific account.
   *  Accepts a promise so the animation can start immediately while the
   *  account number resolves asynchronously. `readOnly` restricts the
   *  session to read-only access (see {@link Authorized.readOnly}). */
  authorize: (
    accountNumberPromise: Promise<bigint | undefined>,
    readOnly = false,
  ): void => {
    authorizedInternal.set({ accountNumberPromise, readOnly });
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
