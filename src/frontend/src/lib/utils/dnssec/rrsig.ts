/**
 * Parse the RDATA of an RRSIG record (RFC 4034 §3.1) into the
 * Candid `Rrsig` shape the canister-side verifier consumes.
 *
 * RRSIG RDATA wire layout:
 *
 * ```text
 *   0  1  2  3
 *  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 *  |  type covered |  alg | labels|  original TTL                |
 *  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 *  |  signature expiration         |  signature inception         |
 *  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 *  |  key tag      | signer's name (variable, wire-format)        |
 *  +--+--+--+--+--+
 *  | signature (variable, remainder of RDATA)                     |
 *  +--+--+--+--+--+--+--+--+...
 * ```
 *
 * Names inside RRSIG RDATA do *not* use message compression per RFC
 * 4034 §3.1.7 — they're plain length-prefixed labels. So we walk
 * the signer name byte-by-byte without needing the surrounding DNS
 * message.
 */

import type { Rrsig } from "$lib/generated/internet_identity_types";

export function parseRrsigRdata(rdata: Uint8Array): Rrsig {
  if (rdata.length < 18) {
    throw new Error(
      `RRSIG RDATA too short: ${rdata.length} bytes (need ≥ 18)`,
    );
  }
  const typeCovered = (rdata[0] << 8) | rdata[1];
  const algorithm = rdata[2];
  const labels = rdata[3];
  const originalTtl =
    ((rdata[4] << 24) >>> 0) |
    ((rdata[5] << 16) >>> 0) |
    ((rdata[6] << 8) >>> 0) |
    (rdata[7] >>> 0);
  const expiration =
    ((rdata[8] << 24) >>> 0) |
    ((rdata[9] << 16) >>> 0) |
    ((rdata[10] << 8) >>> 0) |
    (rdata[11] >>> 0);
  const inception =
    ((rdata[12] << 24) >>> 0) |
    ((rdata[13] << 16) >>> 0) |
    ((rdata[14] << 8) >>> 0) |
    (rdata[15] >>> 0);
  const keyTag = (rdata[16] << 8) | rdata[17];

  // Walk the signer name. Per RFC 4034 §3.1.7 it's uncompressed —
  // length-prefixed labels terminated by a zero octet.
  let cursor = 18;
  while (cursor < rdata.length) {
    const len = rdata[cursor];
    if ((len & 0xc0) !== 0) {
      throw new Error(
        "RRSIG signer name must not use compression (RFC 4034 §3.1.7)",
      );
    }
    cursor++;
    if (len === 0) {
      break;
    }
    cursor += len;
    if (cursor > rdata.length) {
      throw new Error("RRSIG signer name runs past end of RDATA");
    }
  }
  const signerName = rdata.subarray(18, cursor).slice();
  const signature = rdata.subarray(cursor).slice();

  return {
    type_covered: typeCovered,
    algorithm,
    labels,
    original_ttl: originalTtl >>> 0,
    expiration: expiration >>> 0,
    inception: inception >>> 0,
    key_tag: keyTag,
    signer_name: signerName,
    signature,
  };
}

/**
 * Compute the key tag of a DNSKEY RDATA per RFC 4034 Appendix B.
 * For non-revoked algorithms (everything we use) it's the standard
 * 16-bit one's-complement-style checksum over the RDATA bytes.
 */
export function dnsKeyTag(rdata: Uint8Array): number {
  let acc = 0;
  for (let i = 0; i < rdata.length; i++) {
    acc += (i & 1) === 0 ? rdata[i] << 8 : rdata[i];
    acc = acc >>> 0;
  }
  acc += (acc >>> 16) & 0xffff;
  return acc & 0xffff;
}
