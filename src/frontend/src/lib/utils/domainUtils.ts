import type { InternetIdentityInit } from "$lib/generated/internet_identity_types";

/**
 * Check if the current origin is in the list of related origins from the canister config
 * by comparing the URL origins (protocol, hostname, port)
 */
export const isOfficialOrigin = (
  origin: string,
  config: InternetIdentityInit,
): boolean => {
  const relatedOrigins = config.related_origins[0] ?? [];

  try {
    const currentURL = new URL(origin);

    return relatedOrigins.some((relatedOrigin) => {
      try {
        const relatedURL = new URL(relatedOrigin);
        return currentURL.origin === relatedURL.origin;
      } catch (e) {
        console.error(`Invalid URL in related origins: ${relatedOrigin}`, e);
        return false;
      }
    });
  } catch (e) {
    console.error(`Invalid current origin: ${origin}`, e);
    return false;
  }
};
