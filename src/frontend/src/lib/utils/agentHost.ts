/**
 * Where agent calls go, derived from the page or worker they are made in. A service
 * worker has a `location` but no `window`, so this takes one rather than reading it.
 */

/** The domain used for the http api */
const IC_API_DOMAIN = "icp-api.io";

/** What this needs of a `Location`, so a worker's own can be passed. */
export interface AgentLocation {
  hostname: string;
  host: string;
  protocol: string;
}

export const agentHost = (location: AgentLocation | undefined): string => {
  if (location === undefined) {
    // If there is no location, then most likely this is a non-browser environment. All bets
    // are off but we return something valid just in case.
    return "https://" + IC_API_DOMAIN;
  }

  // Match a hostname against an official gateway domain by exact equality or
  // by a dot boundary, so adversarial subdomains like `evil-ic0.app` are not
  // treated as the IC.
  const isGatewayDomain = (domain: string): boolean =>
    location.hostname === domain || location.hostname.endsWith(`.${domain}`);

  if (
    isGatewayDomain("icp0.io") ||
    isGatewayDomain("ic0.app") ||
    isGatewayDomain("icp.net") ||
    isGatewayDomain("internetcomputer.org")
  ) {
    // If this is a canister running on one of the official IC domains, then return the
    // official API endpoint
    return "https://" + IC_API_DOMAIN;
  }

  if (
    location.host === "127.0.0.1" /* typical development */ ||
    location.host ===
      "0.0.0.0" /* typical development, though no secure context (only usable with builds with WebAuthn disabled) */ ||
    location.hostname.endsWith(
      "localhost",
    ) /* local canisters from icx-proxy like rdmx6-....-foo.localhost */
  ) {
    // If this is a local deployment, then assume the api and assets are collocated
    // and use this asset (page)'s URL.
    return location.protocol + "//" + location.host;
  }

  // Otherwise assume it's a custom setup and use the host itself as API.
  return location.protocol + "//" + location.host;
};
