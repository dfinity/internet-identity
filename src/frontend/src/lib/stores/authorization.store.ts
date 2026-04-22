import { derived, type Readable, writable } from "svelte/store";

export type AuthorizationContext = {
  effectiveOrigin: string;
};

export type Authorized = {
  accountNumberPromise: Promise<bigint | undefined>;
};

const contextInternal = writable<AuthorizationContext | undefined>();
const authorizedInternal = writable<Authorized | undefined>();

export const authorizationStore = {
  /** Called by the channel store once the effective origin is resolved. */
  setContext: (effectiveOrigin: string): void => {
    contextInternal.set({ effectiveOrigin });
  },
  /** Called by the UI when the user authorizes with a specific account.
   *  Accepts a promise so the animation can start immediately while the
   *  account number resolves asynchronously. */
  authorize: (accountNumberPromise: Promise<bigint | undefined>): void => {
    authorizedInternal.set({ accountNumberPromise: accountNumberPromise });
  },
  subscribe: contextInternal.subscribe,
};

/** Derived store that guarantees context is available. */
export const authorizationContextStore: Readable<AuthorizationContext> =
  derived(contextInternal, (context) => {
    if (context === undefined) {
      throw new Error("Authorization context is not available yet");
    }
    return context;
  });

/** Store that holds the authorization outcome once the user has authorized. */
export const authorizedStore: Readable<Authorized | undefined> = {
  subscribe: authorizedInternal.subscribe,
};
