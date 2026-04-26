/**
 * Two-hop SSO discovery chain for organization-based sign-in.
 *
 * 1. Fetch `https://{domain}/.well-known/ii-openid-configuration`
 *    → returns `{ client_id, openid_configuration }`
 * 2. Fetch the standard OIDC discovery document from the `openid_configuration`
 *    URL → returns `authorization_endpoint`, `scopes_supported`, etc.
 *
 * Security — trust model:
 * - The caller must call `add_discoverable_oidc_config({ discovery_domain })`
 *   on the backend BEFORE invoking `discoverSsoConfig`. That call traps on
 *   the backend's canary allowlist (returned by
 *   `openid::generic::allowed_discovery_domains()`) for any domain an II
 *   admin hasn't approved, so by the time this module runs, the domain
 *   has been explicitly blessed by II.
 * - This module doesn't carry its own domain allowlist — the check lives
 *   on the canister, not in frontend code that the user's device could
 *   bypass.
 * - Once the first hop succeeds, whatever the domain owner publishes at
 *   `/.well-known/ii-openid-configuration` determines the IdP for the second
 *   hop. We don't maintain a second-hop allowlist — the org knows their own
 *   IdP better than II does, and an attacker who can tamper with a trusted
 *   domain's `.well-known` has already broken something more fundamental
 *   (the canary-allowed domain itself).
 *
 * Security — checks we still enforce:
 * - Domain input validated (DNS format, length limits) unless the host
 *   is loopback (`localhost` / `127.0.0.1`, with optional port), which
 *   is what makes e2e tests against `localhost:11107` work without
 *   widening the regex.
 * - All three URLs (ii-openid-configuration, provider discovery, auth
 *   endpoint) must be HTTPS — the only exception is loopback hosts,
 *   which can use HTTP since there's no real network in play. The
 *   canister's own `sso_discoverable_domains` allowlist remains the
 *   trust boundary; this module is only consulted after the backend
 *   has accepted the domain.
 * - Provider `issuer` hostname must match `openid_configuration` hostname
 *   exactly or as a true subdomain (prevents a tampered provider-discovery
 *   doc from bouncing auth off-host AFTER we've committed to a provider).
 * - Provider `authorization_endpoint` hostname must match the same.
 *
 * Rate limiting:
 * - Successful responses cached for 4 hours per hop.
 * - Per-domain rate limit: max 1 request per 10 minutes.
 * - Max 2 concurrent SSO discoveries globally.
 * - Exponential backoff (2s, 4s, 8s) for retryable errors, up to 3 attempts.
 * - Request timeouts: 5s for `ii-openid-configuration`, 10s for provider
 *   discovery. The abort timer is cleared in a `finally` so network errors
 *   don't leak armed timers.
 */

import { z } from "zod";

/**
 * Subset of fields from a standard OIDC discovery document we use. The
 * schema is the single source of truth; the exported type is derived from
 * it so anything reading from this module can't accidentally skip
 * validation.
 */
const OidcDiscoveryDocumentSchema = z.object({
  issuer: z.string().min(1),
  authorization_endpoint: z.string().min(1),
  scopes_supported: z.array(z.string()).optional(),
});
export type OidcDiscoveryDocument = z.infer<typeof OidcDiscoveryDocumentSchema>;

/** Result of the two-hop SSO discovery chain. */
export interface SsoDiscoveryResult {
  /**
   * The organization domain the user typed on the SSO screen — i.e. the
   * host of hop-1's `/.well-known/ii-openid-configuration`. Carried through
   * so downstream code can label the resulting credential by the SSO
   * provenance rather than by the underlying IdP's issuer.
   */
  domain: string;
  clientId: string;
  /**
   * Human-readable name for the SSO, as published by the domain at hop-1
   * in the optional `name` field. Used by the consent UI to render
   * `sso:<domain>:<key>` attribute rows with a friendly prefix
   * (e.g. "DFINITY email:"); falls back to `domain` when absent.
   */
  name?: string;
  discovery: OidcDiscoveryDocument;
}

/** Response shape of `https://{domain}/.well-known/ii-openid-configuration`.
 *
 * `name` is optional: legacy / minimal deployments don't publish one
 * and the FE falls back to the bare discovery domain for labelling. */
const IIOpenIdConfigurationSchema = z.object({
  client_id: z.string().min(1),
  openid_configuration: z.string().min(1),
  name: z.string().min(1).optional(),
});
type IIOpenIdConfiguration = z.infer<typeof IIOpenIdConfigurationSchema>;

/**
 * Raised when hop 1 (`/.well-known/ii-openid-configuration` on the user's
 * domain) fails in a way that signals the domain owner hasn't set up II
 * integration — 404, non-JSON response, missing fields, or unreachable.
 *
 * Surfaced to the UI so we can show a single user-friendly message
 * instead of leaking raw `fetch` / JSON parse errors.
 */
export class DomainNotConfiguredError extends Error {
  constructor(
    public readonly reason: "http-error" | "invalid-response" | "network",
    public readonly httpStatus?: number,
    public readonly detail?: string,
  ) {
    super(
      detail !== undefined
        ? `Domain not configured for II (${reason}): ${detail}`
        : `Domain not configured for II (${reason})`,
    );
    this.name = "DomainNotConfiguredError";
  }
}

const II_CONFIG_CACHE_TTL_MS = 4 * 60 * 60 * 1000; // 4 hours
const PROVIDER_CACHE_TTL_MS = 4 * 60 * 60 * 1000; // 4 hours
const RATE_LIMIT_MS = 10 * 60 * 1000; // 10 minutes per domain
const II_CONFIG_TIMEOUT_MS = 5_000;
const PROVIDER_TIMEOUT_MS = 10_000;
const MAX_CONCURRENT = 2;
const MAX_RETRIES = 3;
const BASE_BACKOFF_MS = 2000;

// Domain validation constants
const MAX_DOMAIN_LENGTH = 253;
const MAX_LABEL_LENGTH = 63;
const DOMAIN_REGEX =
  /^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)*$/;

interface IIConfigCacheEntry {
  config: IIOpenIdConfiguration;
  fetchedAt: number;
}

interface ProviderCacheEntry {
  document: OidcDiscoveryDocument;
  fetchedAt: number;
}

const iiConfigCache = new Map<string, IIConfigCacheEntry>();
const providerCache = new Map<string, ProviderCacheEntry>();
const lastFetchAttempt = new Map<string, number>();
let activeFetches = 0;

/**
 * True if `hostname` is exactly `expected` or a proper subdomain of it.
 * Using `endsWith(expected)` alone would incorrectly accept
 * `evilaccounts.google.com` when `expected` is `accounts.google.com`.
 */
const hostnameMatchesAllowed = (hostname: string, expected: string): boolean =>
  hostname === expected || hostname.endsWith(`.${expected}`);

/** Validate domain input format (DNS name).
 *
 * Loopback hosts (`localhost` and `127.0.0.1`, with or without a port)
 * skip the DNS-format check so e2e tests can register `localhost:11107`
 * without us having to widen the regex for every caller. Production
 * domains still go through the strict DNS validation path; the canister
 * `sso_discoverable_domains` allowlist remains the actual trust gate. */
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

/**
 * `localhost` / `127.0.0.1`, optionally followed by `:<port>`. IPv6
 * loopback (`[::1]`, etc.) is intentionally not handled — the backend
 * doesn't recognise it either, and the e2e setup uses the hostname
 * form. Mirrors the backend's `scheme_for_allowlisted_host` loopback
 * check.
 */
const isLoopbackHost = (host: string): boolean => {
  const bare = host.split(":", 1)[0]?.toLowerCase() ?? host;
  return bare === "localhost" || bare === "127.0.0.1";
};

/**
 * Scheme to use when fetching from `host`. Loopback gets `http` (the
 * e2e provider can't serve TLS); everything else gets `https`. There's
 * no admin-list gate here — the trust boundary is the canister's
 * `add_discoverable_oidc_config` call, which has already happened by
 * the time we reach this function.
 */
const schemeForHost = (host: string): "http" | "https" =>
  isLoopbackHost(host) ? "http" : "https";

/**
 * `https` is always accepted; `http` is accepted only for loopback
 * hosts.
 */
const validateProviderUrl = (url: string): URL => {
  const parsed = new URL(url);
  if (parsed.protocol === "https:") {
    return parsed;
  }
  if (parsed.protocol === "http:" && isLoopbackHost(parsed.hostname)) {
    return parsed;
  }
  throw new Error(
    `Provider URL must use HTTPS: ${parsed.protocol}//${parsed.hostname}`,
  );
};

/** Zod parse with a prefixed error message for easier user-visible output. */
const parseOrThrow = <T>(
  schema: z.ZodSchema<T>,
  data: unknown,
  context: string,
): T => {
  const result = schema.safeParse(data);
  if (result.success) {
    return result.data;
  }
  const first = result.error.issues[0];
  const path =
    first !== undefined && first.path.length > 0
      ? first.path.join(".")
      : "(root)";
  throw new Error(`${context}: ${first?.message ?? "invalid"} at ${path}`);
};

/** Validate the `ii-openid-configuration` response structure. */
const validateIIConfig = (data: unknown): IIOpenIdConfiguration => {
  const parsed = parseOrThrow(
    IIOpenIdConfigurationSchema,
    data,
    "ii-openid-configuration",
  );
  validateProviderUrl(parsed.openid_configuration);
  return parsed;
};

/**
 * Variant of the issuer/auth-endpoint scheme check that mirrors
 * {@link validateProviderUrl}: HTTPS always passes; HTTP passes only
 * for loopback hosts.
 */
const requireHttpsOrLoopback = (url: URL, label: string): void => {
  if (url.protocol === "https:") return;
  if (url.protocol === "http:" && isLoopbackHost(url.hostname)) return;
  throw new Error(`Provider ${label} must use HTTPS: ${url.toString()}`);
};

/** Validate an OIDC discovery document from the provider. */
const validateProviderDiscovery = (
  data: unknown,
  expectedHostname: string,
): OidcDiscoveryDocument => {
  const doc = parseOrThrow(
    OidcDiscoveryDocumentSchema,
    data,
    "Provider discovery",
  );

  // Issuer hostname must match the expected provider host exactly or as a
  // true subdomain — this blocks look-alike attacks.
  const issuerUrl = new URL(doc.issuer);
  requireHttpsOrLoopback(issuerUrl, "issuer");
  if (!hostnameMatchesAllowed(issuerUrl.hostname, expectedHostname)) {
    throw new Error(
      `Provider issuer hostname mismatch: expected ${expectedHostname} or a subdomain, got ${issuerUrl.hostname}`,
    );
  }

  // The authorization_endpoint is what we'll actually redirect the user to,
  // so constrain it to the same host as the issuer — HTTPS alone isn't
  // enough, a tampered discovery response could otherwise bounce auth to an
  // attacker-controlled host.
  const authUrl = new URL(doc.authorization_endpoint);
  requireHttpsOrLoopback(authUrl, "authorization endpoint");
  if (!hostnameMatchesAllowed(authUrl.hostname, expectedHostname)) {
    throw new Error(
      `Provider authorization endpoint hostname mismatch: expected ${expectedHostname} or a subdomain, got ${authUrl.hostname}`,
    );
  }

  // zod already validated scopes_supported as `string[]` (or undefined).
  return doc;
};

/**
 * Classify a raw error from hop 1 (`/.well-known/ii-openid-configuration`)
 * into a {@link DomainNotConfiguredError} variant so the UI can surface a
 * single friendly message.
 *
 * - `Fetch failed: <status> ...` from our {@link fetchWithRetry} → http-error
 *   with the parsed status.
 * - `SyntaxError` from `response.json()` → invalid-response (HTML served at
 *   200 is the most common cause, e.g. a CMS that SPA-routes every path).
 * - Anything else (timeouts, network failures, CORS, DNS) → network.
 */
const wrapHopOneError = (error: unknown): DomainNotConfiguredError => {
  // Duck-type (not `instanceof Error`): jsdom errors thrown from
  // `Response.json()` come from a separate realm, so `instanceof Error`
  // against the main-realm `Error` returns false even for genuine
  // SyntaxError instances. Reading `name`/`message` works across realms.
  const name = errorName(error);
  const message = errorMessage(error);
  if (message !== undefined) {
    const match = message.match(/^Fetch failed: (\d{3})\b/);
    if (match !== null) {
      return new DomainNotConfiguredError("http-error", parseInt(match[1], 10));
    }
  }
  if (name === "SyntaxError") {
    return new DomainNotConfiguredError("invalid-response");
  }
  return new DomainNotConfiguredError("network");
};

const errorName = (e: unknown): string | undefined =>
  typeof e === "object" &&
  e !== null &&
  typeof (e as { name?: unknown }).name === "string"
    ? (e as { name: string }).name
    : undefined;

const errorMessage = (e: unknown): string | undefined =>
  typeof e === "object" &&
  e !== null &&
  typeof (e as { message?: unknown }).message === "string"
    ? (e as { message: string }).message
    : undefined;

/**
 * Whether a failed HTTP response is transient enough that a retry might
 * succeed. Intentionally narrow: only server-time statuses (`408`,
 * `429`, `5xx`) are retried. Deterministic 4xx (`400`, `401`, `403`,
 * `404`, ...) fail fast — retrying a domain's `/.well-known/ii-openid-
 * configuration` that returns `404` three times just buys the user
 * 2s + 4s + 8s of waiting for the same answer.
 */
const isTransientHttpStatus = (status: number): boolean =>
  status === 408 || status === 429 || status >= 500;

/** Fetch JSON with timeout, retries on transient errors, and exponential backoff. */
const fetchWithRetry = async (
  url: string,
  timeoutMs: number,
): Promise<unknown> => {
  let lastError: unknown;
  for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
    if (attempt > 0) {
      const delay = BASE_BACKOFF_MS * Math.pow(2, attempt - 1);
      await new Promise((resolve) => setTimeout(resolve, delay));
    }
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeoutMs);
    // Default: treat unknown / network errors as transient and retry.
    // The try block overrides this with the HTTP-status classification
    // when the response itself was received.
    let retryable = true;
    try {
      const response = await fetch(url, { signal: controller.signal });
      if (response.ok) {
        return await response.json();
      }
      lastError = new Error(
        `Fetch failed: ${response.status} ${response.statusText}`,
      );
      retryable = isTransientHttpStatus(response.status);
    } catch (error) {
      if (error instanceof Error && error.name === "AbortError") {
        // Explicit timeout: surface the abort immediately; don't backoff
        // through retries when the first attempt already spent the full
        // budget.
        throw error;
      }
      // Network errors (TypeError from `fetch`) and other unknowns: leave
      // `retryable = true` as initialised.
      lastError = error;
    } finally {
      // Clear the abort timer on every exit path so a late abort can't
      // fire after the attempt is already done.
      clearTimeout(timeoutId);
    }
    // Break out of the retry loop on deterministic failures (4xx other
    // than 408 / 429) so the UI can surface them without backoff delay.
    if (!retryable) throw lastError;
  }
  throw lastError;
};

/**
 * Perform the two-hop SSO discovery chain for a given domain.
 *
 * The caller must have already confirmed the domain is registered in
 * `oidc_configs` — this function does no allowlist check of its own.
 *
 * @param domain - The organization domain (e.g., `dfinity.org`)
 * @returns The `client_id` and OIDC discovery document
 * @throws On validation failure, rate limit, timeout, or network error
 */
export const discoverSsoConfig = async (
  domain: string,
): Promise<SsoDiscoveryResult> => {
  const validatedDomain = validateDomain(domain);

  // Serve from cache if both hops are still fresh.
  const cachedIIConfig = iiConfigCache.get(validatedDomain);
  if (
    cachedIIConfig !== undefined &&
    Date.now() - cachedIIConfig.fetchedAt < II_CONFIG_CACHE_TTL_MS
  ) {
    const cachedProvider = providerCache.get(
      cachedIIConfig.config.openid_configuration,
    );
    if (
      cachedProvider !== undefined &&
      Date.now() - cachedProvider.fetchedAt < PROVIDER_CACHE_TTL_MS
    ) {
      return {
        domain: validatedDomain,
        clientId: cachedIIConfig.config.client_id,
        name: cachedIIConfig.config.name,
        discovery: cachedProvider.document,
      };
    }
  }

  // Per-domain rate limit.
  const lastAttempt = lastFetchAttempt.get(validatedDomain);
  if (lastAttempt !== undefined && Date.now() - lastAttempt < RATE_LIMIT_MS) {
    // Fall back to stale cache if we have it, rather than outright failing.
    if (cachedIIConfig !== undefined) {
      const cachedProvider = providerCache.get(
        cachedIIConfig.config.openid_configuration,
      );
      if (cachedProvider !== undefined) {
        return {
          domain: validatedDomain,
          clientId: cachedIIConfig.config.client_id,
          discovery: cachedProvider.document,
        };
      }
    }
    throw new Error(
      `Rate limited: SSO discovery for ${validatedDomain} was attempted too recently`,
    );
  }

  if (activeFetches >= MAX_CONCURRENT) {
    throw new Error("Too many concurrent SSO discovery requests");
  }

  lastFetchAttempt.set(validatedDomain, Date.now());
  activeFetches++;

  try {
    // Hop 1: fetch /.well-known/ii-openid-configuration from the org domain.
    // Raw fetch / JSON / validation errors are converted to
    // DomainNotConfiguredError so the UI can show one friendly message
    // regardless of how exactly the domain misbehaves.
    const iiConfigUrl = `${schemeForHost(validatedDomain)}://${validatedDomain}/.well-known/ii-openid-configuration`;
    let iiConfigData: unknown;
    try {
      iiConfigData = await fetchWithRetry(iiConfigUrl, II_CONFIG_TIMEOUT_MS);
    } catch (error) {
      throw wrapHopOneError(error);
    }
    let iiConfig: IIOpenIdConfiguration;
    try {
      iiConfig = validateIIConfig(iiConfigData);
    } catch (error) {
      // Response decoded but doesn't match the expected shape — from the
      // user's perspective the domain still isn't correctly configured.
      throw new DomainNotConfiguredError(
        "invalid-response",
        undefined,
        error instanceof Error ? error.message : undefined,
      );
    }

    iiConfigCache.set(validatedDomain, {
      config: iiConfig,
      fetchedAt: Date.now(),
    });

    // Hop 2: fetch the provider's standard OIDC discovery document.
    const cachedProviderDoc = providerCache.get(iiConfig.openid_configuration);
    if (
      cachedProviderDoc !== undefined &&
      Date.now() - cachedProviderDoc.fetchedAt < PROVIDER_CACHE_TTL_MS
    ) {
      return {
        domain: validatedDomain,
        clientId: iiConfig.client_id,
        name: iiConfig.name,
        discovery: cachedProviderDoc.document,
      };
    }

    const providerUrl = new URL(iiConfig.openid_configuration);
    const providerData = await fetchWithRetry(
      iiConfig.openid_configuration,
      PROVIDER_TIMEOUT_MS,
    );
    const providerDoc = validateProviderDiscovery(
      providerData,
      providerUrl.hostname,
    );

    providerCache.set(iiConfig.openid_configuration, {
      document: providerDoc,
      fetchedAt: Date.now(),
    });

    return {
      domain: validatedDomain,
      clientId: iiConfig.client_id,
      name: iiConfig.name,
      discovery: providerDoc,
    };
  } finally {
    activeFetches--;
  }
};

/** Clear all SSO discovery caches (for testing). */
export const clearSsoDiscoveryCache = (): void => {
  iiConfigCache.clear();
  providerCache.clear();
  lastFetchAttempt.clear();
  // If a test timed out mid-fetch, the pending promise's `finally` may not
  // have run yet and `activeFetches` could be stuck above 0, making the
  // next test trip the MAX_CONCURRENT guard. Reset it so each test starts
  // from a clean slate.
  activeFetches = 0;
};
