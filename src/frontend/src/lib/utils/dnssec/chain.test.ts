import { describe, it, expect } from "vitest";
import { selectSupportedCoveringRrsig } from "./chain";
import { dnsKeyTag } from "./rrsig";
import type { DnsRR } from "./wire";

const TYPE_RRSIG = 46;
const TYPE_TXT = 16;
const TYPE_DNSKEY = 48;
const FLAG_ZONE = 0x0100;
const FLAG_SEP = 0x0001;

/**
 * Build a minimal RRSIG `DnsRR` carrying just the two fields the
 * selection reads: the covered type (RDATA bytes 0-1) and the
 * algorithm (RDATA byte 2). The rest of the RRSIG RDATA is irrelevant
 * to selection, so we pad with a single zero byte.
 */
const rrsig = (typeCovered: number, algorithm: number): DnsRR => ({
  name: "example.com",
  nameBytes: new Uint8Array([0]),
  type: TYPE_RRSIG,
  class_: 1,
  ttl: 3600,
  rdata: new Uint8Array([
    (typeCovered >> 8) & 0xff,
    typeCovered & 0xff,
    algorithm,
    0,
  ]),
});

describe("selectSupportedCoveringRrsig", () => {
  it("picks the supported algorithm when a zone double-signs (mailbox.org case)", () => {
    // mailbox.org publishes RSA/SHA-1 (7) first, then RSA/SHA-512 (10).
    const sigs = [rrsig(TYPE_TXT, 7), rrsig(TYPE_TXT, 10)];
    const chosen = selectSupportedCoveringRrsig(sigs, TYPE_TXT);
    expect(chosen?.rdata[2]).toBe(10);
  });

  it("returns undefined when every covering RRSIG is an unsupported algorithm", () => {
    // SHA-1-only zone (algs 5 and 7). The walk should abort so the
    // wizard falls through to the DoH path.
    const sigs = [rrsig(TYPE_TXT, 5), rrsig(TYPE_TXT, 7)];
    expect(selectSupportedCoveringRrsig(sigs, TYPE_TXT)).toBeUndefined();
  });

  it("accepts each supported algorithm (8, 10, 13, 15)", () => {
    for (const alg of [8, 10, 13, 15]) {
      expect(
        selectSupportedCoveringRrsig([rrsig(TYPE_TXT, alg)], TYPE_TXT)
          ?.rdata[2],
      ).toBe(alg);
    }
  });

  it("ignores RRSIGs that cover a different type", () => {
    // A supported-algorithm RRSIG, but covering DNSKEY (48), not the
    // requested TXT.
    expect(
      selectSupportedCoveringRrsig([rrsig(48, 13)], TYPE_TXT),
    ).toBeUndefined();
  });

  it("prefers the first supported RRSIG when several are present", () => {
    // Two supported algorithms offered; selection takes the first.
    const sigs = [rrsig(TYPE_TXT, 13), rrsig(TYPE_TXT, 8)];
    expect(selectSupportedCoveringRrsig(sigs, TYPE_TXT)?.rdata[2]).toBe(13);
  });

  it("skips a malformed (too-short) RRSIG RDATA", () => {
    const short: DnsRR = {
      name: "example.com",
      nameBytes: new Uint8Array([0]),
      type: TYPE_RRSIG,
      class_: 1,
      ttl: 3600,
      rdata: new Uint8Array([0, 16]), // only the covered-type bytes
    };
    expect(
      selectSupportedCoveringRrsig([short, rrsig(TYPE_TXT, 8)], TYPE_TXT)
        ?.rdata[2],
    ).toBe(8);
  });
});

/**
 * Build a DNSKEY `DnsRR`. The selection reads the Flags field (RDATA
 * bytes 0-1, for the SEP bit) and the algorithm (RDATA byte 3), and
 * computes the key tag over the whole RDATA; `marker` perturbs the
 * public-key bytes so distinct keys get distinct tags.
 */
const dnskey = (flags: number, algorithm: number, marker: number): DnsRR => ({
  name: "example.com",
  nameBytes: new Uint8Array([0]),
  type: TYPE_DNSKEY,
  class_: 1,
  ttl: 3600,
  // flags(2) | protocol(3) | algorithm(1) | pubkey(2, marker-derived)
  rdata: new Uint8Array([
    (flags >> 8) & 0xff,
    flags & 0xff,
    3,
    algorithm,
    marker,
    marker ^ 0xff,
  ]),
});

/**
 * Build an RRSIG `DnsRR` covering `typeCovered` with `algorithm`, whose
 * `key_tag` (RDATA bytes 16-17, RFC 4034 §3.1.6) is `keyTag`. 19 bytes:
 * the 18-byte fixed header + a single root signer-name octet.
 */
const rrsigForKey = (
  typeCovered: number,
  algorithm: number,
  keyTag: number,
): DnsRR => {
  const rdata = new Uint8Array(19);
  rdata[0] = (typeCovered >> 8) & 0xff;
  rdata[1] = typeCovered & 0xff;
  rdata[2] = algorithm;
  rdata[16] = (keyTag >> 8) & 0xff;
  rdata[17] = keyTag & 0xff;
  return {
    name: "example.com",
    nameBytes: new Uint8Array([0]),
    type: TYPE_RRSIG,
    class_: 1,
    ttl: 3600,
    rdata,
  };
};

describe("selectSupportedCoveringRrsig — DNSKEY RRset must be KSK-signed", () => {
  it("bundles the KSK (SEP) self-signature over a ZSK's, regardless of order", () => {
    // The canister authenticates the DNSKEY RRset under the DS-pinned
    // KSK (RFC 4035 §5.2), so even when the ZSK's RRSIG is returned
    // first we must bundle the SEP key's.
    const ksk = dnskey(FLAG_ZONE | FLAG_SEP, 13, 0xaa);
    const zsk = dnskey(FLAG_ZONE, 13, 0xbb);
    const kskTag = dnsKeyTag(ksk.rdata);
    expect(kskTag).not.toBe(dnsKeyTag(zsk.rdata));
    const sigs = [
      rrsigForKey(TYPE_DNSKEY, 13, dnsKeyTag(zsk.rdata)), // ZSK first
      rrsigForKey(TYPE_DNSKEY, 13, kskTag), // KSK second
    ];
    const chosen = selectSupportedCoveringRrsig(sigs, TYPE_DNSKEY, [ksk, zsk]);
    expect(chosen).toBeDefined();
    expect(((chosen!.rdata[16] << 8) | chosen!.rdata[17]) >>> 0).toBe(kskTag);
  });

  it("returns undefined when only a ZSK signed the DNSKEY RRset", () => {
    // No SEP-key signature to bundle → abandon DNSSEC, fall back to DoH.
    const ksk = dnskey(FLAG_ZONE | FLAG_SEP, 13, 0xaa);
    const zsk = dnskey(FLAG_ZONE, 13, 0xbb);
    const sigs = [rrsigForKey(TYPE_DNSKEY, 13, dnsKeyTag(zsk.rdata))];
    expect(
      selectSupportedCoveringRrsig(sigs, TYPE_DNSKEY, [ksk, zsk]),
    ).toBeUndefined();
  });

  it("returns undefined when the only SEP key signed with an unsupported algorithm (mailbox.org RSA-SHA1 KSK)", () => {
    // KSK is RSA-SHA1 (7, unsupported); ZSK is a supported algorithm
    // but is not DS-pinnable. Nothing usable → DoH fallback.
    const ksk = dnskey(FLAG_ZONE | FLAG_SEP, 7, 0xaa);
    const zsk = dnskey(FLAG_ZONE, 10, 0xbb);
    const sigs = [
      rrsigForKey(TYPE_DNSKEY, 7, dnsKeyTag(ksk.rdata)),
      rrsigForKey(TYPE_DNSKEY, 10, dnsKeyTag(zsk.rdata)),
    ];
    expect(
      selectSupportedCoveringRrsig(sigs, TYPE_DNSKEY, [ksk, zsk]),
    ).toBeUndefined();
  });

  it("does not match an RRSIG whose key_tag collides with a SEP key under a different algorithm", () => {
    // key_tag is a 16-bit checksum, so cross-algorithm collisions are
    // possible. The canister matches DNSKEY candidates by (algorithm,
    // key_tag), so selecting by key_tag alone could bundle an RRSIG
    // that never verifies. A supported-algorithm RRSIG sharing the
    // KSK's tag but under a different algorithm must NOT be chosen.
    const ksk = dnskey(FLAG_ZONE | FLAG_SEP, 13, 0xaa);
    const tag = dnsKeyTag(ksk.rdata);
    const wrongAlg = rrsigForKey(TYPE_DNSKEY, 8, tag); // colliding tag, alg 8 ≠ 13
    expect(
      selectSupportedCoveringRrsig([wrongAlg], TYPE_DNSKEY, [ksk]),
    ).toBeUndefined();
    // The KSK's own signature (matching algorithm and tag) IS selected.
    const right = rrsigForKey(TYPE_DNSKEY, 13, tag);
    expect(
      selectSupportedCoveringRrsig([wrongAlg, right], TYPE_DNSKEY, [ksk]),
    ).toBe(right);
  });
});
