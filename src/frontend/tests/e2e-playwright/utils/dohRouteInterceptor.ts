/**
 * Playwright DoH route interceptor.
 *
 * Replaces real DoH lookups (Cloudflare / Google) with wire-format
 * answers built from a `DnsProofBundle` produced by
 * `dnssecTestSigner.buildChain`. This lets the FE's DNSSEC walker
 * (`src/frontend/src/lib/utils/dnssec/chain.ts`) traverse the test
 * chain end-to-end without touching the network — and without the
 * test having to know which specific (qname, qtype) pairs the walker
 * issues.
 *
 * The walker probes for:
 *   - TXT  `<selector>._domainkey.<domain>`   (DKIM leaf)
 *   - TXT  `_dmarc.<domain>`                  (optional DMARC leaf)
 *   - DNSKEY `<domain>`                       (leaf-zone DNSKEY)
 *   - DS  `<domain>`                          (signed by parent)
 *   - DNSKEY `.`                              (root DNSKEY)
 *
 * The match table is built once from the bundle and indexed by
 * `(name-lowercased, qtype)`. Anything else returns NXDOMAIN.
 */

import type { Page } from "@playwright/test";
import type {
  DnsProofBundle,
  Rrsig,
  SignedRRset,
} from "$lib/generated/internet_identity_types";

// `rtype` numbers come from the bundle's `SignedRRset.rtype` field
// (already correct for TXT / DNSKEY / DS). The interceptor only
// needs to construct RRSIG records itself, so just RRSIG and CLASS_IN
// are referenced directly.
const TYPE_RRSIG = 46;
const CLASS_IN = 1;

const DOH_HOSTS = ["cloudflare-dns.com", "dns.google"];

interface MatchEntry {
  /** Owner name in wire form, length-prefixed labels + null. */
  nameBytes: Uint8Array;
  /** Owner name lowercased, no trailing dot, "" for root. */
  nameLower: string;
  rtype: number;
  ttl: number;
  /** Raw RDATA values for each RR in the RRset. */
  rdatas: Uint8Array[];
  rrsig: Rrsig;
}

/**
 * Build the (name, qtype) → match table from a DnsProofBundle.
 *
 * Each leaf, each delegation link's child_dnskey/child_ds, and the
 * root DNSKEY become one entry each.
 */
function buildMatchEntry(rrset: SignedRRset): MatchEntry {
  const nameBytes = toBytes(rrset.name);
  return {
    nameBytes,
    nameLower: decodeNameLower(nameBytes),
    rtype: rrset.rtype,
    ttl: rrset.ttl,
    rdatas: rrset.rdata.map(toBytes),
    rrsig: rrset.rrsig,
  };
}

function buildMatchTable(bundle: DnsProofBundle): MatchEntry[] {
  const entries: MatchEntry[] = [];
  for (const leaf of bundle.leaf) entries.push(buildMatchEntry(leaf));
  for (const link of bundle.chain) {
    entries.push(buildMatchEntry(link.child_dnskey));
    entries.push(buildMatchEntry(link.child_ds));
  }
  entries.push(buildMatchEntry(bundle.root_dnskey));
  return entries;
}

/**
 * Install the DoH route handler on `page`. Idempotent per page.
 *
 * The DoH layer doesn't care about bundle semantics — it just needs
 * to be able to answer queries for any RRset the FE is going to
 * walk. With the two-phase flow the FE walks both the DMARC leaf
 * (at prepare time) and the DKIM leaf (post-email), so the
 * interceptor accepts a list of extra leaves on top of the
 * skeleton bundle's chain.
 */
export async function installDohInterceptor(
  page: Page,
  bundle: DnsProofBundle,
  extraLeaves: SignedRRset[] = [],
): Promise<void> {
  const table = buildMatchTable(bundle);
  for (const leaf of extraLeaves) {
    table.push(buildMatchEntry(leaf));
  }
  for (const host of DOH_HOSTS) {
    const pattern = `https://${host}/dns-query**`;
    await page.route(pattern, async (route) => {
      try {
        const url = new URL(route.request().url());
        const dnsParam = url.searchParams.get("dns");
        if (dnsParam === null) {
          await route.fulfill({ status: 400, body: "missing dns param" });
          return;
        }
        const query = base64UrlDecode(dnsParam);
        const parsed = parseDnsQuery(query);
        const match = lookup(table, parsed.qnameLower, parsed.qtype);
        const respBytes = buildDnsResponse(parsed, match);
        await route.fulfill({
          status: 200,
          contentType: "application/dns-message",
          body: Buffer.from(respBytes),
        });
      } catch (err) {
        await route.fulfill({
          status: 500,
          body: `interceptor error: ${err instanceof Error ? err.message : String(err)}`,
        });
      }
    });
  }
}

// ---------------------------------------------------------------------
// DoH wire-format read/write
// ---------------------------------------------------------------------

interface ParsedQuery {
  id: number;
  qname: Uint8Array;
  qnameLower: string;
  qtype: number;
}

function parseDnsQuery(buf: Uint8Array): ParsedQuery {
  if (buf.length < 12) throw new Error("DoH query shorter than header");
  const id = (buf[0] << 8) | buf[1];
  const qdcount = (buf[4] << 8) | buf[5];
  if (qdcount !== 1) throw new Error(`expected 1 question, got ${qdcount}`);
  let cursor = 12;
  const start = cursor;
  // Parse qname (no compression in queries).
  while (true) {
    if (cursor >= buf.length) throw new Error("truncated qname");
    const len = buf[cursor];
    cursor += 1;
    if (len === 0) break;
    if ((len & 0xc0) !== 0) throw new Error("compression in qname");
    cursor += len;
  }
  const qname = buf.subarray(start, cursor).slice();
  if (cursor + 4 > buf.length) throw new Error("truncated question fields");
  const qtype = (buf[cursor] << 8) | buf[cursor + 1];
  return { id, qname, qnameLower: decodeNameLower(qname), qtype };
}

function lookup(
  table: MatchEntry[],
  qname: string,
  qtype: number,
): MatchEntry | undefined {
  return table.find((e) => e.nameLower === qname && e.rtype === qtype);
}

/**
 * Build a DoH response. `match === undefined` is encoded as NXDOMAIN
 * (RCODE 3) so the FE's walker treats the name as nonexistent —
 * matches `dohQuery`'s "RCODE 3 → undefined" path.
 */
function buildDnsResponse(
  query: ParsedQuery,
  match: MatchEntry | undefined,
): Uint8Array {
  const id = query.id;
  // Flags: QR=1, AA=1, RD=1, RA=1; AD=1 if signed; RCODE=NOERROR/NXDOMAIN.
  // RFC 6840 §5.8 — AD (Authentic Data) is set by validating resolvers
  // when the response is DNSSEC-validated. The walker doesn't actually
  // check AD (it validates the signatures itself), but mirroring real
  // behaviour avoids surprises.
  const ad = match !== undefined ? 0x20 : 0x00;
  const rcode = match !== undefined ? 0 : 3;
  const flagsHi = 0x84; // QR=1, OP=0, AA=1, TC=0, RD=1
  const flagsLo = 0x80 | ad | rcode; // RA=1 + AD + rcode

  // Question section is echoed back unchanged.
  const question = new Uint8Array(query.qname.length + 4);
  question.set(query.qname, 0);
  question[query.qname.length] = (query.qtype >> 8) & 0xff;
  question[query.qname.length + 1] = query.qtype & 0xff;
  question[query.qname.length + 2] = 0x00;
  question[query.qname.length + 3] = CLASS_IN;

  let answers: Uint8Array = new Uint8Array(0);
  let ancount = 0;
  if (match !== undefined) {
    const records: Uint8Array[] = [];
    // The RRset itself.
    for (const rdata of match.rdatas) {
      records.push(buildRR(match.nameBytes, match.rtype, match.ttl, rdata));
    }
    // The covering RRSIG.
    const rrsigRdata = encodeRrsigRdata(match.rrsig);
    records.push(buildRR(match.nameBytes, TYPE_RRSIG, match.ttl, rrsigRdata));
    answers = concatBytes(records);
    ancount = records.length;
  }

  // Header.
  const header = new Uint8Array(12);
  header[0] = (id >> 8) & 0xff;
  header[1] = id & 0xff;
  header[2] = flagsHi;
  header[3] = flagsLo;
  header[4] = 0x00;
  header[5] = 0x01; // QDCOUNT
  header[6] = (ancount >> 8) & 0xff;
  header[7] = ancount & 0xff;
  // NSCOUNT, ARCOUNT = 0
  return concatBytes([header, question, answers]);
}

function buildRR(
  nameBytes: Uint8Array,
  rtype: number,
  ttl: number,
  rdata: Uint8Array,
): Uint8Array {
  const out = new Uint8Array(nameBytes.length + 10 + rdata.length);
  out.set(nameBytes, 0);
  let off = nameBytes.length;
  out[off++] = (rtype >> 8) & 0xff;
  out[off++] = rtype & 0xff;
  out[off++] = (CLASS_IN >> 8) & 0xff;
  out[off++] = CLASS_IN & 0xff;
  out[off++] = (ttl >> 24) & 0xff;
  out[off++] = (ttl >> 16) & 0xff;
  out[off++] = (ttl >> 8) & 0xff;
  out[off++] = ttl & 0xff;
  out[off++] = (rdata.length >> 8) & 0xff;
  out[off++] = rdata.length & 0xff;
  out.set(rdata, off);
  return out;
}

/** Inverse of `parseRrsigRdata` — see RFC 4034 §3.1 wire format. */
function encodeRrsigRdata(rrsig: Rrsig): Uint8Array {
  const signerName = toBytes(rrsig.signer_name);
  const signature = toBytes(rrsig.signature);
  const out = new Uint8Array(18 + signerName.length + signature.length);
  out[0] = (rrsig.type_covered >> 8) & 0xff;
  out[1] = rrsig.type_covered & 0xff;
  out[2] = rrsig.algorithm;
  out[3] = rrsig.labels;
  out[4] = (rrsig.original_ttl >>> 24) & 0xff;
  out[5] = (rrsig.original_ttl >>> 16) & 0xff;
  out[6] = (rrsig.original_ttl >>> 8) & 0xff;
  out[7] = rrsig.original_ttl & 0xff;
  out[8] = (rrsig.expiration >>> 24) & 0xff;
  out[9] = (rrsig.expiration >>> 16) & 0xff;
  out[10] = (rrsig.expiration >>> 8) & 0xff;
  out[11] = rrsig.expiration & 0xff;
  out[12] = (rrsig.inception >>> 24) & 0xff;
  out[13] = (rrsig.inception >>> 16) & 0xff;
  out[14] = (rrsig.inception >>> 8) & 0xff;
  out[15] = rrsig.inception & 0xff;
  out[16] = (rrsig.key_tag >> 8) & 0xff;
  out[17] = rrsig.key_tag & 0xff;
  out.set(signerName, 18);
  out.set(signature, 18 + signerName.length);
  return out;
}

// ---------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------

function decodeNameLower(wire: Uint8Array): string {
  const labels: string[] = [];
  let cursor = 0;
  while (cursor < wire.length) {
    const len = wire[cursor];
    if (len === 0) break;
    if ((len & 0xc0) !== 0) throw new Error("compression in name bytes");
    const start = cursor + 1;
    const end = start + len;
    if (end > wire.length) throw new Error("name runs past buffer");
    let label = "";
    for (let j = start; j < end; j++) label += String.fromCharCode(wire[j]);
    labels.push(label.toLowerCase());
    cursor = end;
  }
  return labels.join(".");
}

function base64UrlDecode(s: string): Uint8Array {
  const pad = (4 - (s.length % 4)) % 4;
  const b64 = s.replace(/-/g, "+").replace(/_/g, "/") + "=".repeat(pad);
  const bin = atob(b64);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}

function toBytes(v: Uint8Array | number[]): Uint8Array {
  return v instanceof Uint8Array ? v : Uint8Array.from(v);
}

function concatBytes(parts: Uint8Array[]): Uint8Array<ArrayBuffer> {
  const total = parts.reduce((acc, p) => acc + p.length, 0);
  const out = new Uint8Array(total);
  let off = 0;
  for (const p of parts) {
    out.set(p, off);
    off += p.length;
  }
  return out;
}
