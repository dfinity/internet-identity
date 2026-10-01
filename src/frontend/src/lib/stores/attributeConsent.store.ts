import { get, type Readable, writable } from "svelte/store";

/** A single available attribute option resolved from the canister. */
export interface AvailableAttribute {
  key: string;
  displayValue: string;
  rawValue: Uint8Array;
  omitScope: boolean;
}

/** Groups available attributes by their unscoped name for UI rendering.
 *  1 option = checkbox only, >1 options = checkbox + picker. */
export interface AttributeGroup {
  name: string;
  options: AvailableAttribute[];
}

export interface AttributeConsentContext {
  groups: AttributeGroup[];
  /** The published name for each `sso:<domain>` the groups carry, by domain. Resolved
   *  with the context so the first paint has them, rather than by the screen. */
  ssoNames: Record<string, string>;
  effectiveOrigin: string;
  requestedKeys: string[];
  recoveryAddresses: string[];
  verifiedAddresses: string[];
  openidAddresses: string[];
}

export interface AttributeConsent {
  attributes: AvailableAttribute[];
}

const contextInternal = writable<
  Promise<AttributeConsentContext> | undefined
>();
const consentInternal = writable<AttributeConsent | undefined>();
const resolvedInternal = writable(false);

export const attributeConsentStore = {
  /** Set a promise that resolves with the consent context once attributes
   *  are resolved. Clears any previous consent so stale state from a
   *  prior request can't be reused by the next one. */
  setContext: (context: Promise<AttributeConsentContext>): void => {
    consentInternal.set(undefined);
    resolvedInternal.set(false);
    contextInternal.set(context);
    // Settled either way: the flow holds the screen the user is on until this
    // context can be rendered, and a context that failed is no reason to hold it
    // any longer. Guarded against a later request having replaced this one.
    const settled = () => {
      if (get(contextInternal) === context) {
        resolvedInternal.set(true);
      }
    };
    void context.then(settled, settled);
  },
  setConsent: (consent: AttributeConsent): void => {
    consentInternal.set(consent);
  },
  /** Reset both stores — called by the channel handler once it's done with
   *  a request so the next request starts from a clean slate. */
  clear: (): void => {
    contextInternal.set(undefined);
    consentInternal.set(undefined);
    resolvedInternal.set(false);
  },
  subscribe: contextInternal.subscribe,
};

export const attributeConsentResultStore: Readable<
  AttributeConsent | undefined
> = {
  subscribe: consentInternal.subscribe,
};

/** Whether the current context has settled, so the screen can be rendered with
 *  what it asks about rather than as a skeleton. */
export const attributeConsentResolvedStore: Readable<boolean> = {
  subscribe: resolvedInternal.subscribe,
};
