/**
 * URL validation for the notification tap-through (`/notify`).
 *
 * Both parameters arrive from a URL, so both are attacker-craftable: anyone can
 * paste `/notify?origin=…&to=…`. The page fails closed on anything these
 * functions reject.
 *
 * Extracted from the page so the rules can be tested directly — they are the
 * security boundary of the redirect, and a Svelte component's inline helpers
 * cannot be exercised without mounting it.
 */
import { remapToLegacyDomain } from "$lib/utils/iiConnection";

/**
 * Whether `host` is a loopback host, per the secure-context definition:
 * `localhost`, `*.localhost`, the `127.0.0.0/8` range, or `[::1]`.
 *
 * Same predicate as the ICRC-167 transport's. Duplicated rather than shared
 * because that one is transport-internal; a third copy would be the point to
 * consolidate into a shared helper.
 */
const isLoopbackHost = (host: string): boolean =>
  host === "localhost" ||
  host.endsWith(".localhost") ||
  host === "[::1]" ||
  /^127(?:\.\d{1,3}){3}$/.test(host);

/**
 * Whether `url` is a *secure context*, the way browsers decide it: `https`, or
 * `http` only on a loopback host.
 *
 * The scheme check is what makes the origin comparison below meaningful, not
 * defensive tidiness: `javascript:` and `data:` URLs both report their origin as
 * the *string* `"null"`, so two of them compare equal, pass the same-origin
 * test, and reach `location.href` — script execution on II's own origin.
 *
 * Loopback `http` is allowed so a locally-served dApp can be deep-linked during
 * development. It is safe for the same reason browsers treat it as a secure
 * context: loopback traffic never crosses a network, so there is no attacker
 * position from which to tamper with it. Unlike the ICRC-167 callback check
 * there is no `dev_csp` gate here, because nothing in this flow is CSP-bound —
 * the redirect is a top-level navigation, and no allow-list is fetched from the
 * target origin.
 *
 * @see https://w3c.github.io/webappsec-secure-contexts/
 */
const isSecureContextUrl = (url: URL): boolean =>
  url.protocol === "https:" ||
  (url.protocol === "http:" && isLoopbackHost(url.hostname));

/** Parses `raw` into an origin, or `undefined` when it isn't usable. */
export const parseOrigin = (raw: string | null): string | undefined => {
  if (raw === null || raw.length === 0) {
    return undefined;
  }
  try {
    const url = new URL(raw);
    return isSecureContextUrl(url) ? url.origin : undefined;
  } catch {
    return undefined;
  }
};

/**
 * Whether two origins are the same application.
 *
 * Not string equality, because II records consent against the *effective*
 * origin, which passes through `remapToLegacyDomain` — a canister served at
 * `<id>.icp0.io` is consented and attributed as `<id>.ic0.app`. A dApp's own
 * deep links use whichever domain the user is actually browsing, so comparing
 * the two verbatim rejects legitimate links.
 *
 * Normalising both sides is safe rather than loose: the remap only collapses the
 * boundary-node domains for an identical subdomain, and that subdomain is the
 * canister id, so two different canisters can never normalise to one origin.
 */
export const sameApp = (a: string, b: string): boolean =>
  remapToLegacyDomain(a) === remapToLegacyDomain(b);

/**
 * The URL to forward to: `raw` when it is on the sender's own app, or the
 * sender's origin when no target was given. `undefined` means refuse.
 */
export const resolveDestination = (
  origin: string,
  raw: string | null,
): string | undefined => {
  if (raw === null || raw.length === 0) {
    return origin;
  }
  let url: URL;
  try {
    url = new URL(raw);
  } catch {
    return undefined;
  }
  if (!isSecureContextUrl(url)) {
    return undefined;
  }
  return sameApp(url.origin, origin) ? url.href : undefined;
};
