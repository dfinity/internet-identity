import { derived, type Readable, writable } from "svelte/store";

export type AuthorizationContext = {
  effectiveOrigin: string;
  authorized: boolean;
  accountNumber: bigint | undefined;
};

const internalStore = writable<AuthorizationContext | undefined>();

export const authorizationStore = {
  /** Called by the channel store once the effective origin is resolved. */
  setContext: (effectiveOrigin: string): void => {
    internalStore.set({
      effectiveOrigin,
      authorized: false,
      accountNumber: undefined,
    });
  },
  /** Called by the UI when the user authorizes with a specific account. */
  authorize: (accountNumber: bigint | undefined): void => {
    internalStore.update((context) => {
      if (context === undefined) {
        throw new Error("Authorization context has not been set yet");
      }
      return { ...context, authorized: true, accountNumber };
    });
  },
  subscribe: internalStore.subscribe,
};

/** Derived store that guarantees context is available. */
export const authorizationContextStore: Readable<AuthorizationContext> =
  derived(internalStore, (context) => {
    if (context === undefined) {
      throw new Error("Authorization context is not available yet");
    }
    return context;
  });
