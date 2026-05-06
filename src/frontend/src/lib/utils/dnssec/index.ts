/**
 * Browser-side DNSSEC utilities for the email-recovery wizards.
 *
 * The two-phase flow has two callers (see
 * `docs/ongoing/email-recovery.md` §8.4):
 *
 * - **`assembleSkeleton(domain, wantDmarc)`** — at prepare time,
 *   walks the chain from the IANA root down to `<domain>` and
 *   (optionally) attaches the DMARC TXT at `_dmarc.<domain>`. The
 *   DKIM leaf is *not* fetched here — the active selector lives
 *   inside the eventual email's `DKIM-Signature: s=` tag, so the
 *   FE can't pre-fetch it. Returns `undefined` if the chain doesn't
 *   reach root or the leaf zone isn't DNSSEC-signed; the canister
 *   then falls through to its DoH path.
 *
 * - **`assembleDkimLeaf(domain, selector)`** — once polling sees
 *   `NeedDkimLeaf { selector }`, the FE knows the authoritative
 *   selector (the one in the email itself) and walks DNSSEC for
 *   `<selector>._domainkey.<domain>`. The chain anchored at prepare
 *   time covers the zone; this call only needs to fetch the leaf
 *   RRset and its RRSIG. Returns the bundle to submit to the
 *   `email_recovery_submit_dkim_leaf` canister method.
 */

import type { DnsProofBundle, SignedRRset } from "$lib/generated/internet_identity_types";

import { walkSkeletonChain, walkDkimLeaf } from "./chain";

/**
 * Walk the DNSSEC chain rooted at IANA down to `<domain>`, plus the
 * DMARC TXT leaf at `_dmarc.<domain>` when `wantDmarc` is true and
 * the zone publishes one.
 *
 * Returns `undefined` if the chain isn't fully signed up to root.
 * The canister-side prepare path then falls through to its DoH
 * allowlist, which is the intended fallback for unsigned mainstream
 * mailbox providers (Gmail, Outlook, iCloud as of writing).
 */
export function assembleSkeleton(
  domain: string,
  wantDmarc: boolean = true,
): Promise<DnsProofBundle | undefined> {
  return walkSkeletonChain(domain, wantDmarc);
}

/**
 * Walk DNSSEC for the single DKIM TXT leaf at
 * `<selector>._domainkey.<domain>`. The chain is included in the
 * returned bundle so the canister can re-anchor; the canister
 * actually re-uses the cached chain DNSKEY from prepare time and
 * just admits the leaf RRset, but bundling the chain keeps the
 * shape uniform with the prepare-time bundle.
 *
 * Returns `undefined` if the leaf doesn't resolve or the chain
 * isn't fully signed.
 */
export function assembleDkimLeaf(
  domain: string,
  selector: string,
): Promise<{ bundle: DnsProofBundle; leaf: SignedRRset } | undefined> {
  return walkDkimLeaf(domain, selector);
}
