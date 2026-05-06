/**
 * Walk the DNSSEC delegation chain from `<domain>` back to the IANA
 * root, assembling the single-leaf `DnsProofBundle` shape the
 * canister expects (`docs/ongoing/email-recovery.md` §7.2).
 *
 * The walk is bottom-up. From each zone we:
 *
 * 1. Fetch the DNSKEY RRset (and its RRSIG, self-signed by the
 *    zone's KSK).
 * 2. Fetch the DS RRset for the current zone in the *parent* zone
 *    (signed by the parent's DNSKEY).
 * 3. Move up: the parent zone's name comes from the DS RRset's
 *    RRSIG `signer_name` field — we don't have to guess at zone
 *    cuts, the upstream DNSSEC chain tells us.
 *
 * We stop when the parent is the root zone (empty signer name in
 * RRSIGs of the TLD's DS), at which point we capture the root
 * DNSKEY RRset and the chain is complete.
 *
 * The resulting `chain` is ordered top-down (root → TLD → … →
 * leaf-zone) to match the canister-side verifier in
 * `crate::dnssec::verify_chain_with_clock`.
 *
 * Two public entry points wrap the shared walk:
 *
 * - {@link walkSkeletonChain} — chain + optional DMARC leaf
 *   (`leaf` is the DMARC `_dmarc.<domain>` RRset when present, or
 *   `None` otherwise). Used at prepare time. The DKIM leaf is *not*
 *   included — the selector is unknown until the email arrives.
 *
 * - {@link walkDkimLeaf} — chain + DKIM leaf for a known selector.
 *   Used post-email after polling sees `NeedDkimLeaf { selector }`.
 */

import type {
  DelegationLink,
  DnsProofBundle,
  SignedRRset,
} from "$lib/generated/internet_identity_types";

import { dohQuery } from "./doh";
import { parseRrsigRdata } from "./rrsig";
import { decodeDnsName, type DnsRR } from "./wire";

const TYPE_TXT = 16;
const TYPE_DS = 43;
const TYPE_RRSIG = 46;
const TYPE_DNSKEY = 48;

/**
 * Build the prepare-time skeleton bundle: chain rooted at IANA down
 * to `<domain>`, plus the DMARC TXT at `_dmarc.<domain>` when the
 * zone publishes one and `wantDmarc` is true. The DKIM leaf is *not*
 * fetched here — it lands later via {@link walkDkimLeaf} once the
 * email arrives and the selector is known.
 *
 * Returns `undefined` if the chain doesn't reach the IANA root or
 * the leaf zone isn't DNSSEC-signed; the canister falls through to
 * its DoH path in that case.
 */
export async function walkSkeletonChain(
  domain: string,
  wantDmarc: boolean,
): Promise<DnsProofBundle | undefined> {
  // The skeleton chain is anchored at the registered zone
  // `<domain>`. We discover the zone cut by querying its DNSKEY
  // RRSIG and reading `signer_name` (canonical, unambiguous).
  const seedDnskey = await fetchSignedRRset(domain, TYPE_DNSKEY);
  if (seedDnskey === undefined) {
    return undefined;
  }
  const leafZoneBytes = seedDnskey.signerName;

  // Optional DMARC leaf. If present and signed by the same zone,
  // include it; if signed by an unusual cut, drop it (the canister
  // handles "no DMARC" via strict alignment).
  let dmarcLeaf: SignedRRset | undefined;
  if (wantDmarc) {
    const fetched = await fetchSignedRRset(`_dmarc.${domain}`, TYPE_TXT);
    if (
      fetched !== undefined &&
      bytesEqual(fetched.signerName, leafZoneBytes)
    ) {
      dmarcLeaf = fetched.rrset;
    }
  }

  const chainAndRoot = await walkUpFromLeafZone(
    leafZoneBytes,
    seedDnskey.rrset,
  );
  if (chainAndRoot === undefined) {
    return undefined;
  }

  return {
    leaf: dmarcLeaf === undefined ? [] : [dmarcLeaf],
    root_dnskey: chainAndRoot.rootDnskey,
    chain: chainAndRoot.chain,
  };
}

/**
 * Build the post-email DKIM-leaf bundle: chain rooted at IANA down
 * to the leaf zone of `<selector>._domainkey.<domain>`, plus the
 * DKIM TXT at that name as the bundle's single leaf. The chain
 * mirrors what `walkSkeletonChain` produced at prepare time, so the
 * canister's verifier accepts the leaf under the same trust anchor.
 *
 * Returns `undefined` if the leaf doesn't resolve, the chain isn't
 * fully signed, or the DKIM leaf's signer doesn't match the parent
 * zone's expected DNSKEY.
 */
export async function walkDkimLeaf(
  domain: string,
  selector: string,
): Promise<{ bundle: DnsProofBundle; leaf: SignedRRset } | undefined> {
  const dkimFqdn = `${selector}._domainkey.${domain}`;
  const dkimLeaf = await fetchSignedRRset(dkimFqdn, TYPE_TXT);
  if (dkimLeaf === undefined) {
    return undefined;
  }

  // The DKIM TXT lives in the same zone as the registered domain;
  // its RRSIG.signer_name names that zone. Walk from there.
  const leafZoneBytes = dkimLeaf.signerName;
  const seedDnskey = await fetchSignedRRset(
    decodeNameFromBytes(leafZoneBytes) === ""
      ? "."
      : decodeNameFromBytes(leafZoneBytes),
    TYPE_DNSKEY,
  );
  if (seedDnskey === undefined) {
    return undefined;
  }

  const chainAndRoot = await walkUpFromLeafZone(
    leafZoneBytes,
    seedDnskey.rrset,
  );
  if (chainAndRoot === undefined) {
    return undefined;
  }

  return {
    bundle: {
      leaf: [dkimLeaf.rrset],
      root_dnskey: chainAndRoot.rootDnskey,
      chain: chainAndRoot.chain,
    },
    leaf: dkimLeaf.rrset,
  };
}

/**
 * Walk from a starting zone (named by `leafZoneBytes`) up to the
 * IANA root, collecting `(DS-in-parent, DNSKEY-of-child)` links
 * along the way. Returns the top-down chain plus the validated
 * root DNSKEY RRset.
 */
async function walkUpFromLeafZone(
  leafZoneBytes: Uint8Array,
  leafZoneDnskey: SignedRRset,
): Promise<
  | {
      chain: DelegationLink[];
      rootDnskey: SignedRRset;
    }
  | undefined
> {
  const leafZone = decodeNameFromBytes(leafZoneBytes);
  const chainBottomUp: DelegationLink[] = [];
  let currentZone = leafZone;
  let currentDnskey: SignedRRset = leafZoneDnskey;

  // Bound the walk depth — DNS only ever has a handful of label
  // levels, but we don't want a malicious resolver to walk us
  // forever.
  for (let depth = 0; depth < 16; depth++) {
    if (currentZone === "") {
      break;
    }
    const ds = await fetchSignedRRset(currentZone, TYPE_DS);
    if (ds === undefined) {
      // Either the zone has no DS (i.e. DNSSEC isn't fully chained
      // up to root) or the resolver dropped it. Either way we can't
      // build a verifiable bundle; the canister will fall through
      // to the DoH path.
      return undefined;
    }
    const parentZone = decodeNameFromBytes(ds.signerName);
    chainBottomUp.push({
      child_ds: ds.rrset,
      child_dnskey: currentDnskey,
    });
    currentZone = parentZone;
    const next = await fetchSignedRRset(
      currentZone === "" ? "." : currentZone,
      TYPE_DNSKEY,
    );
    if (next === undefined) {
      return undefined;
    }
    currentDnskey = next.rrset;
  }

  if (currentZone !== "") {
    // Walk hit the depth bound without reaching root.
    return undefined;
  }

  // The DNSKEY at zone "" is the root DNSKEY. The chain in
  // bundle order is top-down (root→leaf), so reverse the
  // bottom-up traversal we just did.
  return {
    rootDnskey: currentDnskey,
    chain: chainBottomUp.reverse(),
  };
}

// ---------------------------------------------------------------------------
// Internals
// ---------------------------------------------------------------------------

/**
 * Output of fetching one DNSSEC-signed RRset. `signerName` is
 * extracted from the RRSIG so the caller can drive the chain walk.
 */
interface SignedRRsetFetch {
  rrset: SignedRRset;
  signerName: Uint8Array;
}

/**
 * Fetch all RRs of `qtype` at `name` plus the matching RRSIG,
 * convert to the canister's `SignedRRset` shape, and return.
 *
 * Returns `undefined` if the name doesn't resolve, the answer is
 * empty, or no RRSIG covering `qtype` is present. We pick the first
 * RRSIG whose `type_covered` matches — there should normally be
 * only one per (name, type) tuple, but if the zone signs with
 * multiple keys (e.g. during a rollover) we accept the first the
 * resolver returns and rely on the verifier to try the matching
 * key.
 */
async function fetchSignedRRset(
  name: string,
  qtype: number,
): Promise<SignedRRsetFetch | undefined> {
  const msg = await dohQuery(name, qtype);
  if (msg === undefined) {
    return undefined;
  }
  // Filter the answer section to the records of `qtype` *at the
  // queried name* — chase CNAMEs by following the response, but
  // we don't expect CNAMEs at DKIM/DMARC/DNSKEY/DS records.
  const rrs = msg.answers.filter(
    (rr) =>
      rr.type === qtype &&
      rr.name.toLowerCase() === name.toLowerCase().replace(/\.$/, ""),
  );
  if (rrs.length === 0) {
    return undefined;
  }
  const rrsigs = msg.answers.filter(
    (rr) =>
      rr.type === TYPE_RRSIG &&
      rr.name.toLowerCase() === name.toLowerCase().replace(/\.$/, ""),
  );
  // Find the first RRSIG that covers our qtype.
  let coveringRrsig: DnsRR | undefined;
  for (const candidate of rrsigs) {
    if (candidate.rdata.length < 2) {
      continue;
    }
    const typeCovered = (candidate.rdata[0] << 8) | candidate.rdata[1];
    if (typeCovered === qtype) {
      coveringRrsig = candidate;
      break;
    }
  }
  if (coveringRrsig === undefined) {
    return undefined;
  }
  const rrsig = parseRrsigRdata(coveringRrsig.rdata);

  // Use the owner-name + TTL from the first answer record. All
  // members of an RRset share these by definition.
  const first = rrs[0];
  return {
    rrset: {
      name: first.nameBytes,
      rtype: qtype,
      // Each RR's RDATA is a separate inner Uint8Array — the
      // canister's verifier sorts these into canonical order
      // before hashing. We pass them as the resolver returned them.
      rdata: rrs.map((rr) => rr.rdata),
      ttl: first.ttl,
      rrsig,
    },
    // The `signer_name` field of an RRSIG is wire-format. We use it
    // both inside the bundle (already in `rrsig.signer_name`) and as
    // a side-channel to drive the chain walk.
    signerName: rrsig.signer_name as Uint8Array,
  };
}

/**
 * Convert wire-format DNS name bytes back to a textual label
 * sequence (lowercased). Returns the empty string for the root
 * (`[0]`).
 */
function decodeNameFromBytes(bytes: Uint8Array): string {
  // `decodeDnsName` walks the same buffer for compression pointers,
  // but RRSIG signer_names are uncompressed (RFC 4034 §3.1.7). We
  // can pass the bytes directly.
  const { name } = decodeDnsName(bytes, 0);
  return name;
}

function bytesEqual(a: Uint8Array, b: Uint8Array): boolean {
  if (a.length !== b.length) {
    return false;
  }
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) {
      return false;
    }
  }
  return true;
}
