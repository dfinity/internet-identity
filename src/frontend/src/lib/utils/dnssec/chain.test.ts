import { describe, it, expect } from "vitest";
import { selectSupportedCoveringRrsig } from "./chain";
import type { DnsRR } from "./wire";

const TYPE_RRSIG = 46;
const TYPE_TXT = 16;

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
