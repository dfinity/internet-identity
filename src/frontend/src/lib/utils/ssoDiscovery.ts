/**
 * Two-hop SSO discovery chain for organization-based sign-in.
 *
 * 1. Fetch `https://{domain}/.well-known/ii-openid-configuration`
 *    → returns { client_id, openid_configuration }
 * 2. Fetch the standard OIDC discovery from the openid_configuration URL
 *    → returns authorization_endpoint, scopes_supported, etc.
 *
 * Security:
 * - Domain input validated (DNS format, length limits)
 * - PoC allowlist: only "dfinity.org" accepted
 * - All endpoints must use HTTPS
 * - openid_configuration URL validated against known provider domains
 * - Response structure validated
 *
 * Rate limiting:
 * - Per-domain rate limit: max 1 request per 10 minutes
 * - Successful responses cached for 4 hours
 * - Request timeouts: 5s for ii-openid-configuration, 10s for provider discovery
 * - Max 2 concurrent SSO discovery requests globally
 * - Exponential backoff for failed requests (2s, 4s, 8s)
 * - Debouncing handled by the UI component (750ms minimum)
 */

import type { OidcDiscoveryDocument } from "$lib/utils/oidcDiscovery";

/** Result of the two-hop SSO discovery chain. */
export interface SsoDiscoveryResult {
  clientId: string;
  discovery: OidcDiscoveryDocument;
}

/** Response from the organization's ii-openid-configuration endpoint. */
interface IIOpenIdConfiguration {
  client_id: string;
  openid_configuration: string;
}

/**
 * PoC domain allowlist.
 * TEMPORARY: For proof-of-concept only, to be replaced with a configurable
 * allowlist in production.
 */
const ALLOWED_SSO_DOMAINS = new Set(["dfinity.org"]);

/** Known OIDC provider domains whose discovery endpoints we trust. */
const TRUSTED_PROVIDER_DOMAINS = new Set([
  "accounts.google.com",
  "appleid.apple.com",
  "login.microsoftonline.com",
  "dfinity.okta.com",
  "dev-dfinity.okta.com",
  "login.dfinity.org",
]);

const II_CONFIG_CACHE_TTL_MS = 4 * 60 * 60 * 1000; // 4 hours
const PROVIDER_CACHE_TTL_MS = 4 * 60 * 60 * 1000; // 4 hours
const RATE_LIMIT_MS = 10 * 60 * 1000; // 10 minutes per domain
const II_CONFIG_TIMEOUT_MS = 5_000; // 5 seconds
const PROVIDER_TIMEOUT_MS = 10_000; // 10 seconds
const MAX_CONCURRENT = 2;
const MAX_RETRIES = 3;
const BASE_BACKOFF_MS = 2000;

// Domain validation constants
const MAX_DOMAIN_LENGTH = 253;
const MAX_LABEL_LENGTH = 63;
const DOMAIN_REGEX = /^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)*$/;

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

/** Validate domain input format (DNS name). */
export const validateDomain = (domain: string): string => {
  const trimmed = domain.trim().toLowerCase();
  if (trimmed.length === 0) {
    throw new Error("Domain cannot be empty");
  }
  if (trimmed.length > MAX_DOMAIN_LENGTH) {
    throw new Error(`Domain too long (max ${MAX_DOMAIN_LENGTH} characters)`);
  }
  if (!DOMAIN_REGEX.test(trimmed)) {
    throw new Error("Invalid domain format");
  }
  // Validate individual label lengths
  const labels = trimmed.split(".");
  if (labels.length < 2) {
    throw new Error("Domain must have at least two labels");
  }
  for (const label of labels) {
    if (label.length > MAX_LABEL_LENGTH) {
      throw new Error(`Domain label too long (max ${MAX_LABEL_LENGTH} characters)`);
    }
  }
  return trimmed;
};

/** Check if domain is in the PoC allowlist. */
export const isDomainAllowed = (domain: string): boolean =>
  ALLOWED_SSO_DOMAINS.has(domain);

/** Validate a URL uses HTTPS and points to a trusted provider domain. */
const validateProviderUrl = (url: string): URL => {
  const parsed = new URL(url);
  if (parsed.protocol !== "https:") {
    throw new Error(
      `Provider URL must use HTTPS: ${parsed.protocol}//${parsed.hostname}`,
    );
  }
  let trusted = false;
  for (const trustedDomain of TRUSTED_PROVIDER_DOMAINS) {
    if (
      parsed.hostname === trustedDomain ||
      parsed.hostname.endsWith(`.${trustedDomain}`)
    ) {
      trusted = true;
      break;
    }
  }
  if (!trusted) {
    throw new Error(`Provider domain not trusted: ${parsed.hostname}`);
  }
  return parsed;
};

/** Validate the ii-openid-configuration response structure. */
const validateIIConfig = (data: unknown): IIOpenIdConfiguration => {
  if (typeof data !== "object" || data === null) {
    throw new Error("ii-openid-configuration response is not a valid object");
  }
  const obj = data as Record<string, unknown>;
  if (typeof obj.client_id !== "string" || obj.client_id.length === 0) {
    throw new Error(
      "ii-openid-configuration missing required field: client_id",
    );
  }
  if (
    typeof obj.openid_configuration !== "string" ||
    obj.openid_configuration.length === 0
  ) {
    throw new Error(
      "ii-openid-configuration missing required field: openid_configuration",
    );
  }
  // Validate that openid_configuration URL is HTTPS and points to a trusted provider
  validateProviderUrl(obj.openid_configuration);
  return {
    client_id: obj.client_id,
    openid_configuration: obj.openid_configuration,
  };
};

/** Validate an OIDC discovery document from the provider. */
const validateProviderDiscovery = (
  data: unknown,
  expectedHostname: string,
): OidcDiscoveryDocument => {
  if (typeof data !== "object" || data === null) {
    throw new Error("Provider discovery document is not a valid object");
  }
  const doc = data as Record<string, unknown>;
  if (typeof doc.issuer !== "string" || doc.issuer.length === 0) {
    throw new Error(
      "Provider discovery missing required field: issuer",
    );
  }
  if (
    typeof doc.authorization_endpoint !== "string" ||
    doc.authorization_endpoint.length === 0
  ) {
    throw new Error(
      "Provider discovery missing required field: authorization_endpoint",
    );
  }
  // Validate issuer uses HTTPS
  const issuerUrl = new URL(doc.issuer);
  if (issuerUrl.protocol !== "https:") {
    throw new Error(`Provider issuer must use HTTPS: ${doc.issuer}`);
  }
  // Validate issuer matches expected provider
  if (!issuerUrl.hostname.endsWith(expectedHostname)) {
    throw new Error(
      `Provider issuer hostname mismatch: expected *${expectedHostname}, got ${issuerUrl.hostname}`,
    );
  }
  // Validate authorization_endpoint uses HTTPS
  const authUrl = new URL(doc.authorization_endpoint);
  if (authUrl.protocol !== "https:") {
    throw new Error(
      `Provider authorization endpoint must use HTTPS: ${doc.authorization_endpoint}`,
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

/** Fetch with timeout, retries, and exponential backoff. */
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
    try {
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), timeoutMs);
      const response = await fetch(url, { signal: controller.signal });
      clearTimeout(timeoutId);
      if (!response.ok) {
        throw new Error(
          `Fetch failed: ${response.status} ${response.statusText}`,
        );
      }
      return await response.json();
    } catch (error) {
      lastError = error;
      // Don't retry abort errors or validation errors
      if (error instanceof Error && error.name === "AbortError") {
        throw error;
      }
    }
  }
  throw lastError;
};

/**
 * Perform the two-hop SSO discovery chain for a given domain.
 *
 * @param domain - The organization domain (e.g., "dfinity.org")
 * @returns The client_id and OIDC discovery document
 * @throws On validation failure, rate limit, timeout, or network error
 */
export const discoverSsoConfig = async (
  domain: string,
): Promise<SsoDiscoveryResult> => {
  const validatedDomain = validateDomain(domain);

  // Check domain allowlist before any network requests
  if (!isDomainAllowed(validatedDomain)) {
    throw new Error(`Domain not allowed for SSO: ${validatedDomain}`);
  }

  // Check ii-openid-configuration cache
  const cachedIIConfig = iiConfigCache.get(validatedDomain);
  if (
    cachedIIConfig !== undefined &&
    Date.now() - cachedIIConfig.fetchedAt < II_CONFIG_CACHE_TTL_MS
  ) {
    // Check provider discovery cache
    const cachedProvider = providerCache.get(
      cachedIIConfig.config.openid_configuration,
    );
    if (
      cachedProvider !== undefined &&
      Date.now() - cachedProvider.fetchedAt < PROVIDER_CACHE_TTL_MS
    ) {
      return {
        clientId: cachedIIConfig.config.client_id,
        discovery: cachedProvider.document,
      };
    }
  }

  // Per-domain rate limit
  const lastAttempt = lastFetchAttempt.get(validatedDomain);
  if (lastAttempt !== undefined && Date.now() - lastAttempt < RATE_LIMIT_MS) {
    // Return stale cached data if available
    if (cachedIIConfig !== undefined) {
      const cachedProvider = providerCache.get(
        cachedIIConfig.config.openid_configuration,
      );
      if (cachedProvider !== undefined) {
        return {
          clientId: cachedIIConfig.config.client_id,
          discovery: cachedProvider.document,
        };
      }
    }
    throw new Error(
      `Rate limited: SSO discovery for ${validatedDomain} was attempted too recently`,
    );
  }

  // Concurrent request limit
  if (activeFetches >= MAX_CONCURRENT) {
    throw new Error("Too many concurrent SSO discovery requests");
  }

  lastFetchAttempt.set(validatedDomain, Date.now());
  activeFetches++;

  try {
    // Hop 1: Fetch ii-openid-configuration
    const iiConfigUrl = `https://${validatedDomain}/.well-known/ii-openid-configuration`;
    const iiConfigData = await fetchWithRetry(iiConfigUrl, II_CONFIG_TIMEOUT_MS);
    const iiConfig = validateIIConfig(iiConfigData);

    iiConfigCache.set(validatedDomain, {
      config: iiConfig,
      fetchedAt: Date.now(),
    });

    // Hop 2: Fetch standard OIDC discovery from the provider
    // Check provider cache first
    const cachedProviderDoc = providerCache.get(iiConfig.openid_configuration);
    if (
      cachedProviderDoc !== undefined &&
      Date.now() - cachedProviderDoc.fetchedAt < PROVIDER_CACHE_TTL_MS
    ) {
      return {
        clientId: iiConfig.client_id,
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

    return { clientId: iiConfig.client_id, discovery: providerDoc };
  } finally {
    activeFetches--;
  }
};

/** Clear all SSO discovery caches (for testing). */
export const clearSsoDiscoveryCache = (): void => {
  iiConfigCache.clear();
  providerCache.clear();
  lastFetchAttempt.clear();
};
