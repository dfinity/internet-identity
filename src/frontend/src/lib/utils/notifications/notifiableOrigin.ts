// The origin contract the canister keys consent by, checked before a screen asks for
// anything. A grant it would refuse is one the browser permission prompt and the device
// registration were spent on for nothing.

/**
 * Whether the canister will accept `origin`: a bare `https://host[:port]`, spelled the
 * way a browser serializes one.
 *
 * Mirrors `canonical_origin` in the canister. Comparing against `URL.origin` is the
 * same single comparison it makes, so credentials, path, query, fragment, a trailing
 * slash, an explicit `:443` and an uppercase host all fail here too.
 */
export const isNotifiableOrigin = (origin: string): boolean => {
  try {
    const url = new URL(origin);
    return url.protocol === "https:" && url.origin === origin;
  } catch {
    return false;
  }
};
