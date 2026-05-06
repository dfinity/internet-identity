/**
 * Browser-side DNSSEC utilities for the email-recovery wizards.
 *
 * The two-phase flow has two callers (see
 * `docs/ongoing/email-recovery.md` §8.4):
 *
 * - **`assembleSkeleton(domain, wantDmarc)`** — at prepare time,
 *   walks the chain from the IANA root down to `<domain>` and
 *   (optionally) attaches the DMARC TXT at `_dmarc.<domain>`. The
 *   DKIM resolution is *not* fetched here — the active selector
 *   lives inside the eventual email's `DKIM-Signature: s=` tag, so
 *   the FE can't pre-fetch it. Returns `undefined` if the chain
 *   doesn't reach root or the leaf zone isn't DNSSEC-signed; the
 *   canister then falls through to its DoH path.
 *
 * - **`assembleDkimResolution(domain, selector)`** — once polling
 *   sees `NeedDkimLeaf { selector }`, the FE knows the
 *   authoritative selector (the one in the email itself) and
 *   resolves `<selector>._domainkey.<domain>` end-to-end via
 *   DNSSEC, following CNAMEs. Returns `{ hops, extraChains }`
 *   suitable for `email_recovery_submit_dkim_leaf`. For the
 *   Gmail-style same-zone case `hops` has one TXT and
 *   `extraChains` is empty; for the Proton/Tutanota-style
 *   cross-zone CNAME case `hops` is `[CNAME, …, TXT]` and
 *   `extraChains` carries one delegation chain per signing zone
 *   touched that wasn't already covered by the prepare bundle.
 */

import type {
  DelegationChain,
  DnsProofBundle,
  SignedRRset,
} from "$lib/generated/internet_identity_types";

import { walkSkeletonChain, walkDkimResolution } from "./chain";

/**
 * Walk the DNSSEC chain rooted at IANA down to `<domain>`, plus the
 * DMARC TXT leaf at `_dmarc.<domain>` when `wantDmarc` is true and
 * the zone publishes one.
 *
 * Returns `undefined` if the chain isn't fully signed up to root.
 * The canister-side prepare path then falls through to its DoH
 * allowlist, which is the intended fallback for unsigned mainstream
 * mailbox providers (Gmail, Outlook, iCloud, Yahoo, live.com — the
 * last one's apex is signed but its DKIM CNAMEs into unsigned
 * territory; see design doc §7.6).
 */
export function assembleSkeleton(
  domain: string,
  wantDmarc: boolean = true,
): Promise<DnsProofBundle | undefined> {
  return walkSkeletonChain(domain, wantDmarc);
}

/**
 * Resolve `<selector>._domainkey.<domain>` end-to-end via DNSSEC,
 * following CNAMEs. Returns the signed hop sequence plus delegation
 * chains for any signing zones the resolution crossed into that
 * weren't already covered by the prepare bundle.
 *
 * Returns `undefined` if any hop is unsigned (CNAME chain crosses
 * into unsigned territory — the live.com case), the resolution
 * exceeds the 4-hop cap, or any required zone chain isn't fully
 * signed up to root.
 */
export function assembleDkimResolution(
  domain: string,
  selector: string,
): Promise<
  { hops: SignedRRset[]; extraChains: DelegationChain[] } | undefined
> {
  return walkDkimResolution(domain, selector);
}
