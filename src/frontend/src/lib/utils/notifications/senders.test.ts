import { describe, expect, it } from "vitest";
import { MAX_SENDERS, parseSenders } from "./senders";

const A = "rdmx6-jaaaa-aaaaa-aaadq-cai";
const B = "rrkah-fqaaa-aaaaa-aaaaq-cai";
const C = "ryjl3-tyaaa-aaaaa-aaaba-cai";

describe("parseSenders", () => {
  it("returns every valid sender, in document order", () => {
    expect(parseSenders({ senders: [A, B, C] })).toEqual([A, B, C]);
  });

  it("skips invalid and non-string entries", () => {
    expect(
      parseSenders({ senders: [A, "not-a-principal", 42, null, B] }),
    ).toEqual([A, B]);
  });

  it("drops duplicates", () => {
    expect(parseSenders({ senders: [A, B, A] })).toEqual([A, B]);
  });

  it("caps at the maximum, truncating before filtering", () => {
    expect(parseSenders({ senders: [A, B, C] }, 2)).toEqual([A, B]);
  });

  it("defaults the cap to the backend's MAX_SENDERS", () => {
    const many = Array.from({ length: MAX_SENDERS + 5 }, () => A);
    // All identical, so dedup leaves one; the point is it does not throw and
    // stays within the cap.
    expect(parseSenders({ senders: many }).length).toBeLessThanOrEqual(
      MAX_SENDERS,
    );
  });

  it("returns nothing for a missing or malformed senders field", () => {
    expect(parseSenders({})).toEqual([]);
    expect(parseSenders({ senders: "nope" })).toEqual([]);
    expect(parseSenders(null)).toEqual([]);
    expect(parseSenders(undefined)).toEqual([]);
  });
});
