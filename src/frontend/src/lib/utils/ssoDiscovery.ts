/**
 * SSO discovery for organization-based sign-in.
 *
 * The canister resolves an organization domain to its OIDC configuration and
 * caches the result; this module validates the domain, starts that resolution
 * with `discover_sso`, reads it with `get_sso_discovery_status`, and shapes it
 * for the auth UI.
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
   * How long a sign-in through this domain stays valid, in nanoseconds, from the
   * domain's `session_max_age_seconds`, defaulting to eight hours.
   */
  sessionMaxAgeNs: bigint;
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
 * gated off (`origin-denied`), the canister reports the resolution failed
 * (`failed`), or it didn't complete in time (`timeout`).
 */
export class DomainNotConfiguredError extends Error {
  readonly reason: "timeout" | "origin-denied" | "failed";
  /**
   * For `failed`: when the canister fetches the domain again, so a retry before
   * then fails the same way. Absent when no retry can help.
   */
  readonly retryAfter?: Date;

  constructor(
    reason: "timeout" | "origin-denied" | "failed",
    retryAfter?: Date,
  ) {
    super(`SSO discovery failed (${reason})`);
    this.name = "DomainNotConfiguredError";
    this.reason = reason;
    this.retryAfter = retryAfter;
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
    sessionMaxAgeNs: discovery.session_max_age_ns,
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
 * Resolve a domain's SSO configuration. Validates the domain, calls
 * `discover_sso` (update) once, then polls `get_sso_discovery_status` (query)
 * until it resolves or fails. An optional `signal` cancels the poll (the input
 * debounce drops a stale lookup when the user keeps typing).
 *
 * @throws {Error} when `domain` is invalid, or the lookup is aborted.
 * @throws {DomainNotConfiguredError} when the origin is denied (`origin-denied`),
 *   the canister reports the resolution failed (`failed`), or it times out.
 */
export const discoverSsoConfig = async (
  domain: string,
  signal?: AbortSignal,
  origin?: string,
): Promise<SsoDiscoveryResult> => {
  const validatedDomain = validateDomain(domain);
  const originArg: [] | [string] = origin !== undefined ? [origin] : [];

  if (isAborted(signal)) {
    throw new Error("SSO discovery aborted");
  }
  // Not awaited: starts the fetch, or refreshes a stale result and fetches the
  // domain's keys while the user signs in at the IdP. The query reports progress.
  void anonymousActor.discover_sso(validatedDomain).catch(() => undefined);

  for (let attempt = 0; attempt < MAX_POLL_ATTEMPTS; attempt++) {
    if (isAborted(signal)) {
      throw new Error("SSO discovery aborted");
    }
    const status = await anonymousActor.get_sso_discovery_status({
      org_domain: validatedDomain,
      target_app_origin: originArg,
    });
    if ("Resolved" in status) {
      return toResult(status.Resolved);
    }
    if ("Failed" in status) {
      const retryAfterNs = status.Failed.retry_after[0];
      throw new DomainNotConfiguredError(
        "failed",
        retryAfterNs !== undefined
          ? new Date(Number(retryAfterNs / BigInt(1_000_000)))
          : undefined,
      );
    }
    // Pending. The sleep is abortable so a mid-delay abort skips straight to
    // the next iteration's check instead of firing another query.
    await pollDelay(signal);
  }

  throw new DomainNotConfiguredError("timeout");
};
