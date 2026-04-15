/**
 * Fetches and caches OIDC discovery documents from provider endpoints.
 *
 * Security:
 * - Only HTTPS discovery URLs are accepted
 * - Only known provider domains are allowed (allowlist)
 * - Discovery responses are validated for required fields
 * - Issuer in discovery document must match the expected provider domain
 *
 * Rate limiting:
 * - Successful responses are cached for 1 hour minimum
 * - Per-domain rate limit: max 1 request per 5 minutes
 * - Concurrent requests are limited to 3 simultaneous fetches
 * - Failed requests use exponential backoff
 * - Request timeout of 10 seconds
 */

/** Fields from the OIDC discovery document that we use. */
export interface OidcDiscoveryDocument {
  issuer: string;
  authorization_endpoint: string;
  scopes_supported?: string[];
}

/** Allowlist of domains permitted for OIDC discovery. */
const ALLOWED_DISCOVERY_DOMAINS = new Set([
  "accounts.google.com",
  "appleid.apple.com",
  "login.microsoftonline.com",
]);

const CACHE_TTL_MS = 60 * 60 * 1000; // 1 hour
const RATE_LIMIT_MS = 5 * 60 * 1000; // 5 minutes per domain
const REQUEST_TIMEOUT_MS = 10_000; // 10 seconds
const MAX_CONCURRENT = 3;
const MAX_RETRIES = 3;
const BASE_BACKOFF_MS = 1000;

interface CacheEntry {
  document: OidcDiscoveryDocument;
  fetchedAt: number;
}

const cache = new Map<string, CacheEntry>();
const lastFetchAttempt = new Map<string, number>();
let activeFetches = 0;

/** Validates that a discovery URL uses HTTPS and is on the allowlist. */
const validateDiscoveryUrl = (url: string): URL => {
  const parsed = new URL(url);
  if (parsed.protocol !== "https:") {
    throw new Error(
      `Discovery URL must use HTTPS: ${parsed.protocol}//${parsed.hostname}`,
    );
  }
  if (!ALLOWED_DISCOVERY_DOMAINS.has(parsed.hostname)) {
    throw new Error(
      `Discovery domain not allowed: ${parsed.hostname}`,
    );
  }
  return parsed;
};

/** Validates the structure and content of a discovery document. */
const validateDocument = (
  data: unknown,
  expectedHostname: string,
): OidcDiscoveryDocument => {
  if (typeof data !== "object" || data === null) {
    throw new Error("Discovery document is not a valid object");
  }
  const doc = data as Record<string, unknown>;

  if (typeof doc.issuer !== "string" || doc.issuer.length === 0) {
    throw new Error("Discovery document missing required field: issuer");
  }
  if (
    typeof doc.authorization_endpoint !== "string" ||
    doc.authorization_endpoint.length === 0
  ) {
    throw new Error(
      "Discovery document missing required field: authorization_endpoint",
    );
  }

  // Validate issuer matches expected provider domain
  const issuerUrl = new URL(doc.issuer);
  if (issuerUrl.protocol !== "https:") {
    throw new Error(`Issuer must use HTTPS: ${doc.issuer}`);
  }
  if (!issuerUrl.hostname.endsWith(expectedHostname)) {
    throw new Error(
      `Issuer hostname mismatch: expected *${expectedHostname}, got ${issuerUrl.hostname}`,
    );
  }

  // Validate authorization_endpoint uses HTTPS
  const authUrl = new URL(doc.authorization_endpoint);
  if (authUrl.protocol !== "https:") {
    throw new Error(
      `Authorization endpoint must use HTTPS: ${doc.authorization_endpoint}`,
    );
  }

  return {
    issuer: doc.issuer,
    authorization_endpoint: doc.authorization_endpoint,
    scopes_supported: Array.isArray(doc.scopes_supported)
      ? doc.scopes_supported.filter(
          (s: unknown): s is string => typeof s === "string",
        )
      : undefined,
  };
};

/**
 * Fetch an OIDC discovery document with timeout, rate limiting, and caching.
 *
 * @param discoveryUrl - The `.well-known/openid-configuration` URL
 * @returns The validated discovery document
 * @throws On validation failure, rate limit, timeout, or network error
 */
export const fetchDiscoveryDocument = async (
  discoveryUrl: string,
): Promise<OidcDiscoveryDocument> => {
  const parsed = validateDiscoveryUrl(discoveryUrl);
  const domain = parsed.hostname;

  // Check cache
  const cached = cache.get(discoveryUrl);
  if (cached !== undefined && Date.now() - cached.fetchedAt < CACHE_TTL_MS) {
    return cached.document;
  }

  // Per-domain rate limit
  const lastAttempt = lastFetchAttempt.get(domain);
  if (lastAttempt !== undefined && Date.now() - lastAttempt < RATE_LIMIT_MS) {
    // If we have a stale cache entry, return it rather than failing
    if (cached !== undefined) {
      return cached.document;
    }
    throw new Error(
      `Rate limited: discovery for ${domain} was attempted too recently`,
    );
  }

  // Concurrent request limit
  if (activeFetches >= MAX_CONCURRENT) {
    if (cached !== undefined) {
      return cached.document;
    }
    throw new Error("Too many concurrent discovery requests");
  }

  lastFetchAttempt.set(domain, Date.now());
  activeFetches++;

  try {
    const document = await fetchWithRetry(discoveryUrl, domain);
    cache.set(discoveryUrl, { document, fetchedAt: Date.now() });
    return document;
  } finally {
    activeFetches--;
  }
};

/** Fetch with exponential backoff retry. */
const fetchWithRetry = async (
  url: string,
  expectedHostname: string,
): Promise<OidcDiscoveryDocument> => {
  let lastError: unknown;
  for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
    if (attempt > 0) {
      const delay = BASE_BACKOFF_MS * Math.pow(2, attempt - 1);
      await new Promise((resolve) => setTimeout(resolve, delay));
    }
    try {
      const controller = new AbortController();
      const timeoutId = setTimeout(
        () => controller.abort(),
        REQUEST_TIMEOUT_MS,
      );
      const response = await fetch(url, { signal: controller.signal });
      clearTimeout(timeoutId);

      if (!response.ok) {
        throw new Error(
          `Discovery fetch failed: ${response.status} ${response.statusText}`,
        );
      }
      const data: unknown = await response.json();
      return validateDocument(data, expectedHostname);
    } catch (error) {
      lastError = error;
      // Don't retry abort errors or validation errors
      if (
        error instanceof Error &&
        (error.name === "AbortError" ||
          error.message.startsWith("Discovery document") ||
          error.message.startsWith("Issuer") ||
          error.message.startsWith("Authorization endpoint"))
      ) {
        throw error;
      }
    }
  }
  throw lastError;
};

/**
 * Look up a cached discovery document by its issuer URL.
 * Returns undefined if no cached document matches.
 */
export const findCachedDiscoveryByIssuer = (
  issuer: string,
): { discoveryUrl: string; document: OidcDiscoveryDocument } | undefined => {
  for (const [discoveryUrl, entry] of cache) {
    if (entry.document.issuer === issuer) {
      return { discoveryUrl, document: entry.document };
    }
  }
  return undefined;
};

/** Clear the discovery cache (useful for testing). */
export const clearDiscoveryCache = (): void => {
  cache.clear();
  lastFetchAttempt.clear();
};
