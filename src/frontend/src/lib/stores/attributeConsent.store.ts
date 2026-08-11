import { type Readable, writable } from "svelte/store";

/** A single available attribute option resolved from the canister. */
export interface AvailableAttribute {
  key: string;
  /** The option's user-visible text, and its identity for deduplication,
   *  `{#each}` keying and selection. For image-valued attributes this is a
   *  short content fingerprint rather than the value — the value there is a
   *  `data:` URL up to ~137 KB, which has no business being rendered as
   *  text, lowercased into a Map key, or interpolated into a DOM key. Those
   *  rows are rendered from {@link AvailableAttribute.imageSrc} instead. */
  displayValue: string;
  /** Set only for image-valued attributes (`profile_picture`): the `data:`
   *  URL to render. Its presence is what tells the consent UI to show a
   *  thumbnail in place of `displayValue`. */
  imageSrc?: string;
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
