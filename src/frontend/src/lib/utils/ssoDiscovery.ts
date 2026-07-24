/**
 * SSO discovery for organization-based sign-in.
 *
 * The canister resolves an organization domain to its OIDC configuration and
 * caches the result; this module validates the domain, drives that resolution
 * through `discover_sso` / `discover_sso_query`, and shapes it for the auth UI.
 */
import { anonymousActor } from "$lib/globals";
import type { SsoDiscovery } from "$lib/generated/internet_identity_types";
import { MAX_POLL_ATTEMPTS, pollDelay } from "$lib/utils/openidPoll";

/** Resolved SSO configuration for a domain. */
export interface SsoDiscoveryResult {
  /**
   * The organization domain the user typed on the SSO screen. Carried through
   * so downstream code can label the resulting credential by the SSO
   * provenance rather than by the underlying IdP's issuer.
   */
  domain: string;
  /** The org's primary OIDC client. */
  clientId: string;
  /** The client to run the ceremony against for the target origin; equals {@link clientId} when no origin was passed. */
  resolvedClientId: string;
  /**
   * Human-readable name for the SSO, if the domain publishes one. Used by the
   * consent UI to render `sso:<domain>:<key>` attribute rows with a friendly
   * prefix (e.g. "DFINITY email:"); falls back to `domain` when absent.
   */
  name?: string;
  discovery: {
    issuer: string;
    authorization_endpoint: string;
    scopes_supported?: string[];
  };
}

/**
 * Raised when a domain's SSO configuration can't be resolved: the origin is
 * gated off (`origin-denied`) or the resolution didn't complete in time
 * (`timeout`).
 */
export class DomainNotConfiguredError extends Error {
  readonly reason: "timeout" | "origin-denied";

  constructor(reason: "timeout" | "origin-denied") {
    super(`SSO discovery failed (${reason})`);
    this.name = "DomainNotConfiguredError";
    this.reason = reason;
  }
}

const MAX_DOMAIN_LENGTH = 253;
const MAX_LABEL_LENGTH = 63;
const DOMAIN_REGEX =
  /^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)*$/;

/**
 * `localhost` / `127.0.0.1`, optionally followed by `:<port>`. IPv6 loopback
 * (`[::1]`, etc.) is intentionally not handled — the canister doesn't recognise
 * it either, and the e2e setup uses the hostname form.
 */
const isLoopbackHost = (host: string): boolean => {
  let url: URL;
  try {
    // Parse as a URL authority so the optional `:<port>` is split off for us
    // rather than by hand. Invalid input throws and is treated as non-loopback.
    url = new URL(`http://${host}`);
  } catch {
    return false;
  }
  // A bare `host[:port]` has no path/query/fragment; reject e.g. `localhost/x`.
  if (url.pathname !== "/" || url.search !== "" || url.hash !== "") {
    return false;
  }
  return url.hostname === "localhost" || url.hostname === "127.0.0.1";
};

/**
 * Validate domain input format (DNS name). Loopback hosts (`localhost` and
 * `127.0.0.1`, with or without a port) skip the DNS-format check so e2e tests
 * can use `localhost:11107` without widening the regex. The canister's
 * bare-authority check on the domain is the actual trust gate.
 *
 * @throws {Error} when `domain` is not a valid DNS name.
 */
export const validateDomain = (domain: string): string => {
  const trimmed = domain.trim().toLowerCase();
  if (trimmed.length === 0) {
    throw new Error("Domain cannot be empty");
  }
  if (trimmed.length > MAX_DOMAIN_LENGTH) {
    throw new Error(`Domain too long (max ${MAX_DOMAIN_LENGTH} characters)`);
  }
  if (isLoopbackHost(trimmed)) {
    return trimmed;
  }
  if (!DOMAIN_REGEX.test(trimmed)) {
    throw new Error("Invalid domain format");
  }
  const labels = trimmed.split(".");
  if (labels.length < 2) {
    throw new Error("Domain must have at least two labels");
  }
  for (const label of labels) {
    if (label.length > MAX_LABEL_LENGTH) {
      throw new Error(
        `Domain label too long (max ${MAX_LABEL_LENGTH} characters)`,
      );
    }
  }
  return trimmed;
};

const toResult = (discovery: SsoDiscovery): SsoDiscoveryResult => {
  // `resolved_client_id` is empty only when an origin was supplied and denied by `gate_all_apps`.
  const resolvedClientId = discovery.resolved_client_id[0];
  if (resolvedClientId === undefined) {
    throw new DomainNotConfiguredError("origin-denied");
  }
  return {
    domain: discovery.discovery_domain,
    clientId: discovery.client_id,
    resolvedClientId,
    name: discovery.name[0],
    discovery: {
      issuer: discovery.issuer,
      authorization_endpoint: discovery.authorization_endpoint,
      scopes_supported: discovery.scopes,
    },
  };
};

// Wrap the abort check in a function so each call returns a fresh `boolean`.
// Reading `signal?.aborted` inline narrows it to `false` for the rest of the
// iteration, and TypeScript doesn't re-widen across the `await`s — so a second
// inline check on the same `signal` is flagged as always-false (TS2367).
const isAborted = (signal?: AbortSignal): boolean => signal?.aborted === true;

/**
 * Resolve a domain's SSO configuration. Validates the domain, then polls
 * `get_sso_discovery_status` (query); on `Pending` it drives the fetch with
 * `discover_sso` (update) and polls again. An optional `signal` cancels the poll
 * (the input debounce drops a stale lookup when the user keeps typing).
 *
 * @throws {Error} when `domain` is invalid, or the lookup is aborted.
 * @throws {DomainNotConfiguredError} when the origin is denied (`origin-denied`)
 *   or the resolution times out.
 */
export const discoverSsoConfig = async (
  domain: string,
  signal?: AbortSignal,
  origin?: string,
): Promise<SsoDiscoveryResult> => {
  const validatedDomain = validateDomain(domain);
  const originArg: [] | [string] = origin !== undefined ? [origin] : [];

  for (let attempt = 0; attempt < MAX_POLL_ATTEMPTS; attempt++) {
    if (isAborted(signal)) {
      throw new Error("SSO discovery aborted");
    }
    // Read the discovery status via the cheap query.
    const status = await anonymousActor.get_sso_discovery_status({
      org_domain: validatedDomain,
      target_app_origin: originArg,
    });
    if ("Resolved" in status) {
      return toResult(status.Resolved);
    }
    // Re-check before the update: the query above may have spanned an abort,
    // and we don't want to drive a fetch for a lookup the user already dropped.
    if (isAborted(signal)) {
      throw new Error("SSO discovery aborted");
    }
    // Pending — drive the fetch with an update, then poll again. The sleep is
    // abortable so a mid-delay abort skips straight to the next iteration's
    // check instead of firing another query.
    await anonymousActor.discover_sso(validatedDomain);
    await pollDelay(signal);
  }

  throw new DomainNotConfiguredError("timeout");
};
