/**
 * Walk the DNSSEC delegation chain from a leaf RRset back to the
 * IANA root, assembling the `DnsProofBundle` shape the canister
 * expects (`docs/ongoing/email-recovery.md` §7.2).
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
 * `crate::dnssec::verify_with_clock`.
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
 * Result of `walkChain` — exactly the shape the canister method
 * `email_recovery_credential_prepare_add` accepts. `leaves` holds
 * the DKIM TXT and (when present) the DMARC TXT.
 */
export interface AssembledBundle {
  bundle: DnsProofBundle;
  /** True if the DMARC leaf was found and included in `bundle.leaves`. */
  hasDmarc: boolean;
}

/**
 * Build the bundle. Returns `undefined` if any of the required
 * records is missing or the chain doesn't reach the IANA root —
 * the canister falls through to its DoH path in that case.
 *
 * `domain` is the registered email domain (e.g. `example.com`).
 * `selector` is the DKIM selector (typically discovered via
 * `discoverSelector`). `wantDmarc` requests the DMARC TXT at
 * `_dmarc.<domain>` be included as a second leaf.
 */
export async function walkChain(
  domain: string,
  selector: string,
  wantDmarc: boolean,
): Promise<AssembledBundle | undefined> {
  // 1. Fetch the leaf DKIM TXT + RRSIG. The signer_name of the
  //    DKIM RRSIG is our leaf zone — there's no ambiguity about
  //    where the zone cut is, the RRSIG names it explicitly.
  const dkimFqdn = `${selector}._domainkey.${domain}`;
  const dkimLeaf = await fetchSignedRRset(dkimFqdn, TYPE_TXT);
  if (dkimLeaf === undefined) {
    return undefined;
  }

  const leaves: SignedRRset[] = [dkimLeaf.rrset];
  let hasDmarc = false;

  if (wantDmarc) {
    const dmarcFqdn = `_dmarc.${domain}`;
    const dmarcLeaf = await fetchSignedRRset(dmarcFqdn, TYPE_TXT);
    if (dmarcLeaf !== undefined) {
      // Both leaves must be signed by the same zone — they have to
      // be (DMARC and DKIM both live under the same registered
      // domain) but verify defensively. If the upstream zone has
      // an unusual cut, drop the DMARC leaf rather than build an
      // invalid bundle; the canister's verifier handles "no DMARC"
      // via strict alignment.
      const sameSigner = bytesEqual(
        dkimLeaf.signerName,
        dmarcLeaf.signerName,
      );
      if (sameSigner) {
        leaves.push(dmarcLeaf.rrset);
        hasDmarc = true;
      }
    }
  }

  const leafZone = decodeNameFromBytes(dkimLeaf.signerName);

  // 2. Walk from the leaf zone up to root, collecting DNSKEY +
  //    DS pairs.
  const chainBottomUp: DelegationLink[] = [];
  let currentZone = leafZone;
  let currentDnskey = await fetchSignedRRset(
    currentZone === "" ? "." : currentZone,
    TYPE_DNSKEY,
  );
  if (currentDnskey === undefined) {
    return undefined;
  }

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
      child_dnskey: currentDnskey.rrset,
    });
    currentZone = parentZone;
    currentDnskey = await fetchSignedRRset(
      currentZone === "" ? "." : currentZone,
      TYPE_DNSKEY,
    );
    if (currentDnskey === undefined) {
      return undefined;
    }
  }

  if (currentZone !== "") {
    // Walk hit the depth bound without reaching root.
    return undefined;
  }

  // 3. The DNSKEY at zone "" is the root DNSKEY. The chain in
  //    bundle order is top-down (root→leaf), so reverse the
  //    bottom-up traversal we just did.
  return {
    bundle: {
      leaves,
      root_dnskey: currentDnskey.rrset,
      chain: chainBottomUp.reverse(),
    },
    hasDmarc,
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
      rr.type === qtype && rr.name.toLowerCase() === name.toLowerCase().replace(/\.$/, ""),
  );
  if (rrs.length === 0) {
    return undefined;
  }
  const rrsigs = msg.answers.filter(
    (rr) => rr.type === TYPE_RRSIG && rr.name.toLowerCase() === name.toLowerCase().replace(/\.$/, ""),
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
