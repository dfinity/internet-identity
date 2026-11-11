import { getPrimaryOrigin } from "$lib/globals";

// Paths that must keep running on the origin they were loaded from.
const KEEP_ON_CURRENT_ORIGIN = [
  // Legacy verifiable credentials
  "/vc-flow",
  // Support page, it links to itself on every related origin so passkey
  // behaviour can be diagnosed per domain
  "/self-service",
  // WebAuthn iframe used for migration
  "/iframe/webauthn",
  // OpenID callback, the redirect_uri is registered per origin
  "/callback",
];

let redirecting = false;

/**
 * Whether {@link redirectToPrimaryOrigin} has started a redirect, used to keep
 * the page hidden instead of flashing this origin's render before it commits.
 */
export const isRedirectingToPrimaryOrigin = (): boolean => redirecting;

/**
 * Send the page to the primary origin when it was loaded from a related one.
 *
 * Called from the client `init` hook, before any component mounts, so it runs
 * before the authorize layout establishes the ICRC-29 channel in `$effect.pre`.
 * That ordering is load bearing: the client learns the signer origin from the
 * handshake response, so a redirect before the handshake is invisible to it,
 * while a redirect after it leaves the client pinned to the previous origin
 * until its disconnect timeout closes the window.
 */
export const redirectToPrimaryOrigin = (): boolean => {
  const primaryOrigin = getPrimaryOrigin();
  if (primaryOrigin === undefined || window.location.origin === primaryOrigin) {
    return false;
  }
  const { pathname, search, hash } = window.location;
  // Legacy transport, outgoing leg: the dapp posts its request to this document
  // and the transport takes it to the primary origin itself.
  if (pathname === "/" && hash === "#authorize") {
    return false;
  }
  // Legacy transport, return leg: the primary origin bounced the response back
  // to this origin, and only this document can finish it — the redirect session
  // that re-wraps the delegation for the dapp lives in this origin's
  // `sessionStorage`, and the dapp is holding this window.
  const bounced = new URLSearchParams(hash.replace(/^#/, ""));
  if (
    bounced.has("redirect_message") &&
    bounced.get("redirect_origin") === primaryOrigin
  ) {
    return false;
  }
  if (KEEP_ON_CURRENT_ORIGIN.includes(pathname)) {
    return false;
  }
  const target = new URL(primaryOrigin);
  target.pathname = pathname;
  target.search = search;
  target.hash = hash;
  redirecting = true;
  window.location.replace(target.href);
  return true;
};
