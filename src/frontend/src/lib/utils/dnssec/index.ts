/**
 * Caller-side DNSSEC bundle assembly for the email-recovery flow.
 *
 * The canister supports two ways to source the DKIM (and DMARC) TXT
 * records for an inbound email:
 *
 * 1. **DNSSEC**: the FE supplies a `DnsProofBundle` carrying the
 *    DKIM TXT (and optionally DMARC TXT) plus a chain of DNSSEC
 *    RRSIGs / DNSKEYs back to the IANA root. The canister validates
 *    the chain synchronously at prepare time. No HTTPS outcalls.
 *
 * 2. **DoH allowlist**: the FE submits just the address + selector;
 *    the canister fetches the records via a 3-of-5 DoH provider
 *    quorum at email-arrival time, gated by an operator-set
 *    allowlist (Gmail, Outlook, iCloud, etc).
 *
 * This module covers path 1: it walks the DNSSEC chain from a
 * leaf RRset back to the IANA root via DoH wire-format queries
 * and returns the bundle ready to ship to the canister. If any
 * step fails (zone unsigned, DoH unreachable, malformed RRSIG),
 * `assembleBundle` resolves with `undefined` so the caller can
 * fall back to path 2.
 *
 * See `docs/ongoing/email-recovery.md` §7.4 for the spec.
 */

import type { DnsProofBundle } from "$lib/generated/internet_identity_types";

import { dohQuery } from "./doh";
import { walkChain } from "./chain";

/**
 * Common DKIM selector names, probed in parallel against a domain to
 * discover whichever one the provider is currently signing under.
 * Sourced from operational practice (the large mailbox providers'
 * published patterns plus a handful of platform conventions). Order
 * doesn't matter — we keep the first one whose TXT record returns
 * `v=DKIM1`.
 *
 * Adding a new candidate here is the right place to bring a new
 * provider online: the canister-side selector-validation rules
 * accept any DNS-label-shaped string, so it's data not code.
 */
const SELECTOR_CANDIDATES: readonly string[] = [
  // Google Workspace / Gmail use date-style selectors that rotate
  // every few months. Probing recent ones covers users who haven't
  // seen a key rotation in a while.
  "20230601",
  "20240101",
  "20240601",
  "20250101",
  "20250601",
  "google",
  // Microsoft 365 / Outlook
  "selector1",
  "selector2",
  // Apple iCloud
  "sig1",
  // Common Proton selectors
  "protonmail",
  "protonmail2",
  "protonmail3",
  // Generic / other
  "default",
  "dkim",
  "mail",
  "k1",
  "k2",
  "s1",
  "s2",
];

const TYPE_TXT = 16;

/**
 * Probe a list of common DKIM selectors against `domain` via DoH and
 * return the first one whose TXT record looks like a DKIM key
 * (starts with `v=DKIM1`). Returns `undefined` if none of the
 * candidates resolved.
 *
 * Used by the setup wizard to fill in the `selector` field of
 * `EmailRecoveryDnsInput` — the canister needs it to know which
 * `<selector>._domainkey.<domain>` TXT record to fetch when the
 * user's mail eventually arrives.
 */
export async function discoverSelector(
  domain: string,
): Promise<string | undefined> {
  const lower = domain.trim().toLowerCase();
  if (lower.length === 0) {
    return undefined;
  }

  // Parallel-probe every candidate. We resolve to the first match
  // rather than `Promise.race` so a single slow provider doesn't
  // hide a quick match elsewhere.
  const probes = SELECTOR_CANDIDATES.map(async (selector) => {
    const fqdn = `${selector}._domainkey.${lower}`;
    const txt = await fetchTxtPresentation(fqdn);
    return txt !== undefined && isDkimRecord(txt) ? selector : undefined;
  });
  const results = await Promise.all(probes);
  return results.find((s): s is string => s !== undefined);
}

/**
 * Assemble a fully-signed `DnsProofBundle` for
 * `<selector>._domainkey.<domain>` (and, when `wantDmarc` is true,
 * `_dmarc.<domain>` as a second leaf) by walking the DNSSEC chain
 * back to the IANA root.
 *
 * Returns `undefined` if any required record is missing, the chain
 * doesn't reach root, or the leaf zone isn't DNSSEC-signed. The
 * canister falls through to its DoH path in that case — that's the
 * intended fallback for unsigned mainstream mailbox providers
 * (Gmail, Outlook, iCloud as of writing).
 */
export async function assembleBundle(
  domain: string,
  selector: string,
  wantDmarc: boolean = true,
): Promise<DnsProofBundle | undefined> {
  const result = await walkChain(domain, selector, wantDmarc);
  return result?.bundle;
}

// ---------------------------------------------------------------------------
// Internals — selector discovery uses a tiny TXT extractor over the
// DoH wire-format response to keep one HTTP path. Not exported.
// ---------------------------------------------------------------------------

/**
 * Fetch the (concatenated) TXT record content at `name` via DoH
 * wire-format and return it as a UTF-8 string. Multi-string TXT
 * records are concatenated without separators (DKIM/DMARC consumers
 * expect that shape per RFC 6376 §3.6.2.2 / RFC 7489 §6.1).
 */
async function fetchTxtPresentation(name: string): Promise<string | undefined> {
  const msg = await dohQuery(name, TYPE_TXT);
  if (msg === undefined) {
    return undefined;
  }
  const txtRrs = msg.answers.filter(
    (rr) =>
      rr.type === TYPE_TXT &&
      rr.name.toLowerCase() === name.toLowerCase().replace(/\.$/, ""),
  );
  if (txtRrs.length === 0) {
    return undefined;
  }
  // TXT RDATA is one or more `<len><bytes>` character-strings.
  // Concatenate into a single string per record, then concatenate
  // records (rare to have multiple TXTs at one name; we just join
  // them).
  let out = "";
  for (const rr of txtRrs) {
    let cursor = 0;
    while (cursor < rr.rdata.length) {
      const len = rr.rdata[cursor];
      cursor++;
      const end = cursor + len;
      if (end > rr.rdata.length) {
        // Malformed — bail.
        return undefined;
      }
      for (let i = cursor; i < end; i++) {
        out += String.fromCharCode(rr.rdata[i]);
      }
      cursor = end;
    }
  }
  return out;
}

/**
 * Cheap shape check: a DKIM TXT record starts with `v=DKIM1`,
 * possibly with leading WSP. Used to filter out unrelated TXT
 * records that happen to live at a `<candidate>._domainkey.<domain>`
 * name (rare in practice, but providers do put junk there).
 */
function isDkimRecord(txt: string): boolean {
  return /^\s*v\s*=\s*DKIM1\b/i.test(txt);
}
