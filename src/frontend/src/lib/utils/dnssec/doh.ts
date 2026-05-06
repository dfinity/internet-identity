/**
 * DoH wire-format client. Uses GET with a base64url-encoded query
 * to keep things simple and compatible with both Cloudflare's
 * `cloudflare-dns.com/dns-query` and Google's `dns.google/dns-query`
 * endpoints.
 *
 * Wire-format DoH (RFC 8484) is preferred over the JSON variant
 * because we need the *raw RDATA bytes* for DNSSEC verification —
 * the JSON form returns RDATA in presentation form and re-encoding
 * RRSIG signed-data from there is a fool's errand.
 */

import { buildDnsQuery, parseDnsMessage, type DnsMessage } from "./wire";

const DOH_ENDPOINTS: readonly string[] = [
  "https://cloudflare-dns.com/dns-query",
  "https://dns.google/dns-query",
];

/** Per-request timeout. Generous because DoH adds TLS + JSON overhead. */
const DOH_TIMEOUT_MS = 5_000;

/**
 * Encode `bytes` as base64url (RFC 4648 §5) without padding —
 * required by the DoH GET form.
 */
function base64UrlEncode(bytes: Uint8Array): string {
  let bin = "";
  for (let i = 0; i < bytes.length; i++) {
    bin += String.fromCharCode(bytes[i]);
  }
  return btoa(bin)
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/g, "");
}

/**
 * Issue a DoH wire-format query for `(qname, qtype)` and return the
 * parsed DNS response. Tries the configured providers in order;
 * resolves with the first one that returns a valid response with
 * RCODE=0 (NOERROR). Resolves with `undefined` if every provider
 * fails or returns NXDOMAIN — selector probing and chain walking
 * both treat that as "name doesn't exist".
 */
export async function dohQuery(
  qname: string,
  qtype: number,
): Promise<DnsMessage | undefined> {
  const queryBytes = buildDnsQuery(qname, qtype);
  const dnsParam = base64UrlEncode(queryBytes);

  for (const endpoint of DOH_ENDPOINTS) {
    // AbortController + setTimeout matches the rest of the FE
    // (`lib/utils/ssoDiscovery.ts`); we avoid `AbortSignal.timeout`
    // because the project still supports browsers without it.
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), DOH_TIMEOUT_MS);
    try {
      const url = `${endpoint}?dns=${dnsParam}`;
      const res = await fetch(url, {
        method: "GET",
        headers: { Accept: "application/dns-message" },
        signal: controller.signal,
      });
      if (!res.ok) {
        continue;
      }
      const ct = res.headers.get("Content-Type") ?? "";
      if (!ct.toLowerCase().includes("application/dns-message")) {
        // Some misconfigured endpoints fall back to JSON when the
        // GET form is malformed. Try the next provider rather than
        // attempting to recover.
        continue;
      }
      const buf = new Uint8Array(await res.arrayBuffer());
      const msg = parseDnsMessage(buf);
      // RCODE 3 = NXDOMAIN. Two providers might still disagree —
      // try the next one.
      if (msg.rcode === 3) {
        continue;
      }
      // RCODE 0 = NOERROR (the one we want). Anything else (SERVFAIL
      // etc) is also worth retrying against the next provider.
      if (msg.rcode !== 0) {
        continue;
      }
      return msg;
    } catch {
      // Network / timeout / parse error — try the next provider.
    } finally {
      clearTimeout(timeoutId);
    }
  }
  return undefined;
}
