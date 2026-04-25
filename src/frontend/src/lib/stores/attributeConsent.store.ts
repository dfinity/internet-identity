import { type Readable, writable } from "svelte/store";

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
  effectiveOrigin: string;
}

export interface AttributeConsent {
  attributes: AvailableAttribute[];
}

const contextInternal = writable<
  Promise<AttributeConsentContext> | undefined
>();
const consentInternal = writable<AttributeConsent | undefined>();

export const attributeConsentStore = {
  /** Set a promise that resolves with the consent context once attributes
   *  are resolved. Clears any previous consent so stale state from a
   *  prior request can't be reused by the next one. */
  setContext: (context: Promise<AttributeConsentContext>): void => {
    consentInternal.set(undefined);
    contextInternal.set(context);
  },
  setConsent: (consent: AttributeConsent): void => {
    consentInternal.set(consent);
  },
  /** Reset both stores — called by the channel handler once it's done with
   *  a request so the next request starts from a clean slate. */
  clear: (): void => {
    contextInternal.set(undefined);
    consentInternal.set(undefined);
  },
  subscribe: contextInternal.subscribe,
};

export const attributeConsentResultStore: Readable<
  AttributeConsent | undefined
> = {
  subscribe: consentInternal.subscribe,
};
