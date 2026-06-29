import { describe, it, expect, beforeEach, vi } from "vitest";
import { lastSharedEmailsStore } from "./last-shared-emails.store";

vi.mock("$app/environment", () => ({ browser: true }));

const anchorA = BigInt("111");
const anchorB = BigInt("222");
const dappX = "https://x.example";
const dappY = "https://y.example";

describe("lastSharedEmailsStore", () => {
  beforeEach(() => {
    localStorage.clear();
    lastSharedEmailsStore.reset();
  });

  it("returns undefined for unknown (anchor, origin)", () => {
    expect(lastSharedEmailsStore.get(anchorA, dappX)).toBeUndefined();
  });

  it("round-trips a single entry", () => {
    lastSharedEmailsStore.set(anchorA, dappX, "alice@example.com");
    expect(lastSharedEmailsStore.get(anchorA, dappX)).toBe("alice@example.com");
  });

  it("keeps per-origin entries independent for the same anchor", () => {
    lastSharedEmailsStore.set(anchorA, dappX, "alice@example.com");
    lastSharedEmailsStore.set(anchorA, dappY, "bob@example.com");
    expect(lastSharedEmailsStore.get(anchorA, dappX)).toBe("alice@example.com");
    expect(lastSharedEmailsStore.get(anchorA, dappY)).toBe("bob@example.com");
  });

  it("keeps per-anchor entries independent for the same origin", () => {
    lastSharedEmailsStore.set(anchorA, dappX, "alice@example.com");
    lastSharedEmailsStore.set(anchorB, dappX, "carol@example.com");
    expect(lastSharedEmailsStore.get(anchorA, dappX)).toBe("alice@example.com");
    expect(lastSharedEmailsStore.get(anchorB, dappX)).toBe("carol@example.com");
  });

  it("overwrites a previous entry for the same (anchor, origin)", () => {
    lastSharedEmailsStore.set(anchorA, dappX, "alice@example.com");
    lastSharedEmailsStore.set(anchorA, dappX, "alice.work@example.com");
    expect(lastSharedEmailsStore.get(anchorA, dappX)).toBe(
      "alice.work@example.com",
    );
  });

  it("persists across reads via localStorage", () => {
    lastSharedEmailsStore.set(anchorA, dappX, "alice@example.com");
    const raw = localStorage.getItem("ii-last-shared-emails");
    expect(raw).not.toBeNull();
    const parsed = JSON.parse(raw!);
    // Stored shape: { data: { <anchor>: { <origin>: email } }, version: 1 }
    expect(parsed.data[anchorA.toString()][dappX]).toBe("alice@example.com");
  });
});
