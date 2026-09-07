/**
 * Helper function to compare two URL origins to check if they're the same.
 * This normalizes the URLs and compares their protocol, hostname, and port.
 *
 * @param urlA - The first URL to compare
 * @param urlB - The second URL to compare
 * @returns True if both origins are the same, false otherwise
 */
export const isSameOrigin = (urlA: string, urlB: string): boolean => {
  try {
    // Parse URLs to get access to their components
    const a = new URL(urlA);
    const b = new URL(urlB);

    // Compare protocol, hostname and port
    return a.origin === b.origin;
  } catch (error) {
    // If URL parsing fails, do a direct string comparison
    console.warn(`Failed to parse URLs for comparison: ${error}`);
    return urlA === urlB;
  }
};

/**
 * The label shown to the user for an origin that acts as a trust anchor — the
 * hostname badge next to app-provided (permissionless) metadata.
 *
 * Ordinary https origins on the default port collapse to their hostname
 * (`https://example.com` becomes `example.com`), which is the form users
 * recognise. Any component that would otherwise be hidden is kept: a
 * non-https scheme, or a non-default port. Those distinguish origins that
 * derive different principals, so `https://example.com` and
 * `https://example.com:8443` must never present the same anchor.
 *
 * @param origin Origin to label, e.g. a postMessage channel origin.
 */
export const originLabel = (origin: string): string => {
  try {
    const url = new URL(origin);
    return url.protocol === "https:" && url.port === ""
      ? url.hostname
      : url.origin;
  } catch {
    // Not parseable as a URL: show it verbatim rather than hiding it.
    return origin;
  }
};

/**
 * The canister gateway domains a canister is reachable on. `ic0.app` comes
 * first because it is the one principals are derived against: `remapToLegacyDomain`
 * collapses the other two onto it so a canister derives one principal
 * regardless of which domain the user arrived through.
 */
const GATEWAY_DOMAINS = ["ic0.app", "icp0.io", "icp.net"] as const;

/** The legacy gateway domain principals are derived against. */
export const LEGACY_GATEWAY_DOMAIN = GATEWAY_DOMAINS[0];

/**
 * Matches a canister gateway origin, capturing the canister subdomain (with an
 * optional `.raw` label) and the gateway domain it was reached through.
 *
 * Single source of truth for both directions of the mapping:
 * `remapToLegacyDomain` uses it to normalize onto {@link LEGACY_GATEWAY_DOMAIN},
 * and {@link gatewayOriginTwins} to invert that.
 */
export const GATEWAY_ORIGIN_REGEX =
  /^https:\/\/(?<subdomain>[\w-]+(?:\.raw)?)\.(?<domain>ic0\.app|icp0\.io|icp\.net)$/;

/**
 * The same canister's origins on the gateway domains other than the one
 * `origin` uses, in {@link GATEWAY_DOMAINS} order. Empty for anything that is
 * not a canister gateway origin (a custom domain, localhost).
 *
 * These all resolve to the same canister, so they are interchangeable for
 * resources that carry no principal-derivation semantics, and only for those.
 */
export const gatewayOriginTwins = (origin: string): string[] => {
  const groups = origin.match(GATEWAY_ORIGIN_REGEX)?.groups;
  if (groups === undefined) {
    return [];
  }
  const { subdomain, domain } = groups;
  return GATEWAY_DOMAINS.filter((candidate) => candidate !== domain).map(
    (candidate) => `https://${subdomain}.${candidate}`,
  );
};
