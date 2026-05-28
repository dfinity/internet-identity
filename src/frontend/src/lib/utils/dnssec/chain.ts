/**
 * Walk DNSSEC for the email-recovery flow's two ingress points:
 *
 * - {@link walkSkeletonChain} — at prepare time, fetches the chain
 *   from `<domain>` back to the IANA root and (optionally) the DMARC
 *   TXT at `_dmarc.<domain>`. The DKIM resolution is *not* in this
 *   bundle — its selector is unknown until the email arrives.
 *
 * - {@link walkDkimResolution} — post-email, once polling sees
 *   `NeedDkimLeaf { selector }`. Resolves
 *   `<selector>._domainkey.<domain>` end-to-end, following any
 *   CNAMEs (Proton, Tutanota, M365 custom domains, …), and returns
 *   the signed hop sequence plus any *additional* delegation chains
 *   for zones that weren't already covered by the prepare bundle.
 *
 * Bundle shape (`docs/ongoing/email-recovery.md` §7.2):
 *
 *   DnsProofBundle {
 *     root_dnskey: SignedRRset
 *     chains:      DelegationChain[]   // one per signing zone
 *     hops:        SignedRRset[]       // CNAME, …, final TXT
 *   }
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
 * Each chain is ordered top-down (root → TLD → … → leaf-zone) to
 * match the canister-side verifier in
 * `crate::dnssec::verify_chain_with_clock`.
 */

import type {
  DelegationChain,
  DelegationLink,
  DnsProofBundle,
  SignedRRset,
} from "$lib/generated/internet_identity_types";

import { dohQuery } from "./doh";
import { parseRrsigRdata } from "./rrsig";
import { decodeDnsName, type DnsRR } from "./wire";

const TYPE_TXT = 16;
const TYPE_CNAME = 5;
const TYPE_DS = 43;
const TYPE_RRSIG = 46;
const TYPE_DNSKEY = 48;

/**
 * Maximum CNAME hops accepted in a DKIM resolution. Mirrors the
 * canister-side `MAX_CNAME_HOPS = 4` cap. Real-world DKIM CNAME
 * chains observed across providers max out at two; the cap is
 * generous headroom while still rejecting oversized resolutions.
 */
const MAX_CNAME_HOPS = 4;

/**
 * Build the prepare-time skeleton bundle: chain rooted at IANA down
 * to `<domain>`, plus the DMARC TXT at `_dmarc.<domain>` when the
 * zone publishes one and `wantDmarc` is true. The DKIM resolution
 * is *not* fetched here — it lands later via {@link walkDkimResolution}
 * once the email arrives and the selector is known.
 *
 * Returns `undefined` if the chain doesn't reach the IANA root or
 * the leaf zone isn't DNSSEC-signed; the canister falls through to
 * its DoH path in that case.
 */
export async function walkSkeletonChain(
  domain: string,
  wantDmarc: boolean,
): Promise<DnsProofBundle | undefined> {
  const seedDnskey = await fetchSignedRRset(domain, TYPE_DNSKEY);
  if (seedDnskey === undefined) {
    return undefined;
  }
  const leafZoneBytes = seedDnskey.signerName;

  // Optional DMARC hop. If present and signed by the same zone,
  // include it; if signed by an unusual cut, drop it (the canister
  // handles "no DMARC" via strict alignment).
  const hops: SignedRRset[] = [];
  if (wantDmarc) {
    const fetched = await fetchSignedRRset(`_dmarc.${domain}`, TYPE_TXT);
    if (
      fetched !== undefined &&
      bytesEqual(fetched.signerName, leafZoneBytes)
    ) {
      hops.push(fetched.rrset);
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
    root_dnskey: chainAndRoot.rootDnskey,
    chains: [{ links: chainAndRoot.chain }],
    hops,
  };
}

/**
 * Resolve `<selector>._domainkey.<domain>` end-to-end via DNSSEC,
 * following any CNAMEs along the way. Returns the signed hop
 * sequence plus delegation chains for any signing zones touched
 * by the resolution that weren't already covered by the prepare
 * bundle (the apex zone is implicitly cached canister-side).
 *
 * Returns `undefined` if any hop is unsigned (CNAME chain crosses
 * into unsigned territory — the live.com case), the resolution
 * exceeds {@link MAX_CNAME_HOPS}, or any required zone chain isn't
 * fully signed up to root. The caller should then fall through to
 * a no-bundle prepare so the canister can pick the DoH path.
 */
export async function walkDkimResolution(
  domain: string,
  selector: string,
): Promise<
  { hops: SignedRRset[]; extraChains: DelegationChain[] } | undefined
> {
  const dkimFqdn = `${selector}._domainkey.${domain}`;

  // Walk the CNAME chain at `dkimFqdn`, accumulating one signed
  // RRset per hop until we land on the final TXT.
  const hops = await walkCnameChain(dkimFqdn);
  if (hops === undefined) {
    return undefined;
  }

  // Collect every distinct signing zone the hops live under.
  // The apex (`<domain>`) is already covered by the prepare
  // bundle's chain, so skip it; for every other zone, walk a
  // delegation chain to root.
  const apexZoneCanonical = canonicalizeNameString(domain);
  const seenSigners = new Set<string>();
  const extraChains: DelegationChain[] = [];
  for (const hop of hops) {
    const signerCanonical = canonicalizeNameString(
      decodeNameFromBytes(hop.rrsig.signer_name as Uint8Array),
    );
    if (signerCanonical === apexZoneCanonical) {
      continue;
    }
    if (seenSigners.has(signerCanonical)) {
      continue;
    }
    seenSigners.add(signerCanonical);
    const chain = await walkChainForZone(hop.rrsig.signer_name as Uint8Array);
    if (chain === undefined) {
      return undefined;
    }
    extraChains.push({ links: chain });
  }

  return { hops, extraChains };
}

/**
 * Walk a CNAME chain anchored at `name` for as long as each hop
 * resolves and is DNSSEC-signed by its respective zone. Returns
 * the signed hops in resolution order, ending in a TXT.
 *
 * `undefined` on:
 * - Any hop that doesn't resolve.
 * - Any hop missing an RRSIG (CNAME chain crossed into unsigned
 *   territory — `live.com` style; the canister can't authenticate
 *   this end-to-end and the FE falls through to DoH).
 * - Loops, oversize chains, or a chain that ends in something
 *   other than a TXT.
 */
async function walkCnameChain(
  name: string,
): Promise<SignedRRset[] | undefined> {
  const hops: SignedRRset[] = [];
  const visited = new Set<string>();
  let current = name;
  for (let i = 0; i < MAX_CNAME_HOPS; i++) {
    const canon = canonicalizeNameString(current);
    if (visited.has(canon)) {
      return undefined;
    }
    visited.add(canon);

    // Each iteration: try TXT first; if the resolver returns a
    // CNAME for this name, follow it. This mirrors what a
    // recursive resolver does internally — except we need each
    // hop's RRSIG individually so we query one hop at a time.
    const fetched = await fetchSignedRRsetEither(current, [
      TYPE_TXT,
      TYPE_CNAME,
    ]);
    if (fetched === undefined) {
      return undefined;
    }
    hops.push(fetched.rrset);
    if (fetched.rrset.rtype === TYPE_TXT) {
      return hops;
    }
    if (fetched.rrset.rtype === TYPE_CNAME) {
      const target = decodeFirstCnameTarget(fetched.rrset);
      if (target === undefined) {
        return undefined;
      }
      current = target;
      continue;
    }
    return undefined;
  }
  // Chain longer than the cap — the canister would reject anyway.
  return undefined;
}

/**
 * Walk the DNSSEC delegation chain from `zoneNameBytes` (in wire
 * format) back to the IANA root, returning the top-down link list
 * (root → … → zone). The caller is responsible for fetching the
 * zone's DNSKEY RRset itself; this function only fills in the
 * delegation links.
 */
async function walkChainForZone(
  zoneNameBytes: Uint8Array,
): Promise<DelegationLink[] | undefined> {
  const zoneName = decodeNameFromBytes(zoneNameBytes);
  // Seed: fetch the zone's DNSKEY RRset (signed by its KSK) and
  // walk up from there.
  const seedDnskey = await fetchSignedRRset(
    zoneName === "" ? "." : zoneName,
    TYPE_DNSKEY,
  );
  if (seedDnskey === undefined) {
    return undefined;
  }
  const result = await walkUpFromLeafZone(zoneNameBytes, seedDnskey.rrset);
  if (result === undefined) {
    return undefined;
  }
  return result.chain;
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

  for (let depth = 0; depth < 16; depth++) {
    if (currentZone === "") {
      break;
    }
    const ds = await fetchSignedRRset(currentZone, TYPE_DS);
    if (ds === undefined) {
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
    return undefined;
  }

  return {
    rootDnskey: currentDnskey,
    chain: chainBottomUp.reverse(),
  };
}

// ---------------------------------------------------------------------------
// Internals
// ---------------------------------------------------------------------------

interface SignedRRsetFetch {
  rrset: SignedRRset;
  signerName: Uint8Array;
}

/**
 * Fetch all RRs of `qtype` at `name` plus the matching RRSIG,
 * convert to the canister's `SignedRRset` shape, and return.
 */
function fetchSignedRRset(
  name: string,
  qtype: number,
): Promise<SignedRRsetFetch | undefined> {
  return fetchSignedRRsetEither(name, [qtype]);
}

/**
 * Like {@link fetchSignedRRset} but accepts multiple acceptable
 * RTYPEs. Used by {@link walkCnameChain} to ask "either TXT or
 * CNAME at this name" in a single round trip — DoH responds with
 * whichever the zone publishes.
 *
 * Returns the matched RRset (its `rtype` reflects what was
 * actually returned) plus the RRSIG's `signer_name`. Returns
 * `undefined` if neither type resolved or no covering RRSIG was
 * present.
 */
async function fetchSignedRRsetEither(
  name: string,
  acceptedTypes: number[],
): Promise<SignedRRsetFetch | undefined> {
  // Query for the *first* preferred type. Recursive resolvers
  // return any covering CNAME chain in the answer section even
  // when the queried RTYPE doesn't match the final RR — so a TXT
  // query for a name that's actually a CNAME comes back with the
  // CNAME RR right alongside the TXT.
  const msg = await dohQuery(name, acceptedTypes[0]);
  if (msg === undefined) {
    return undefined;
  }

  // Look for an answer of any accepted type, at the queried name.
  // The first hit wins — `acceptedTypes` is in priority order.
  const lowerName = name.toLowerCase().replace(/\.$/, "");
  let matched: DnsRR | undefined;
  for (const rtype of acceptedTypes) {
    const found = msg.answers.find(
      (rr) => rr.type === rtype && rr.name.toLowerCase() === lowerName,
    );
    if (found !== undefined) {
      matched = found;
      break;
    }
  }
  if (matched === undefined) {
    return undefined;
  }

  const matchedType = matched.type;
  const rrs = msg.answers.filter(
    (rr) => rr.type === matchedType && rr.name.toLowerCase() === lowerName,
  );
  const rrsigs = msg.answers.filter(
    (rr) => rr.type === TYPE_RRSIG && rr.name.toLowerCase() === lowerName,
  );
  let coveringRrsig: DnsRR | undefined;
  for (const candidate of rrsigs) {
    if (candidate.rdata.length < 2) {
      continue;
    }
    const typeCovered = (candidate.rdata[0] << 8) | candidate.rdata[1];
    if (typeCovered === matchedType) {
      coveringRrsig = candidate;
      break;
    }
  }
  if (coveringRrsig === undefined) {
    return undefined;
  }
  const rrsig = parseRrsigRdata(coveringRrsig.rdata);

  const first = rrs[0];
  return {
    rrset: {
      name: first.nameBytes,
      rtype: matchedType,
      rdata: rrs.map((rr) => rr.rdata),
      ttl: first.ttl,
      rrsig,
    },
    signerName: rrsig.signer_name as Uint8Array,
  };
}

/**
 * Decode the target name out of a CNAME RRset's RDATA. CNAME
 * RDATA is a single wire-format DNS name. Returns the decoded
 * dotted-string (lowercased, no trailing dot — same shape we
 * pass to `dohQuery`).
 */
function decodeFirstCnameTarget(rrset: SignedRRset): string | undefined {
  if (rrset.rdata.length !== 1) {
    return undefined;
  }
  const target = decodeNameFromBytes(rrset.rdata[0] as Uint8Array);
  if (target === "") {
    return undefined;
  }
  return target;
}

/**
 * Convert wire-format DNS name bytes back to a textual label
 * sequence (lowercased). Returns the empty string for the root
 * (`[0]`).
 */
function decodeNameFromBytes(bytes: Uint8Array): string {
  const { name } = decodeDnsName(bytes, 0);
  return name;
}

/**
 * Canonical-form name comparison helper: lowercase, no trailing
 * dot. Matches what the canister-side verifier compares against
 * (it canonicalises wire-format owner names per RFC 4034 §6.2).
 */
function canonicalizeNameString(name: string): string {
  return name.toLowerCase().replace(/\.$/, "");
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
