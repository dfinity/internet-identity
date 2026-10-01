import { describe, expect, it } from "vitest";
import { get } from "svelte/store";
import {
  attributeConsentResolvedStore,
  attributeConsentStore,
  type AttributeConsentContext,
} from "./attributeConsent.store";

const context = (): AttributeConsentContext => ({
  groups: [],
  effectiveOrigin: "https://app.example",
  requestedKeys: [],
  recoveryAddresses: [],
  verifiedAddresses: [],
  openidAddresses: [],
});

describe("attributeConsentResolvedStore", () => {
  it("is unresolved while the context is still being read", () => {
    attributeConsentStore.setContext(new Promise(() => undefined));
    expect(get(attributeConsentResolvedStore)).toBe(false);
    attributeConsentStore.clear();
  });

  it("resolves once the context is in hand", async () => {
    const pending = Promise.resolve(context());
    attributeConsentStore.setContext(pending);
    await pending;
    expect(get(attributeConsentResolvedStore)).toBe(true);
    attributeConsentStore.clear();
  });

  /** The flow keeps the screen the user is on until this resolves, so a context that
   *  failed has to stop the waiting rather than extend it. */
  it("resolves on a context that failed", async () => {
    const failing = Promise.reject(new Error("could not read the attributes"));
    attributeConsentStore.setContext(failing);
    await failing.catch(() => undefined);
    expect(get(attributeConsentResolvedStore)).toBe(true);
    attributeConsentStore.clear();
  });

  /** A second request replaces the first. The first's answer arriving afterwards
   *  must not report the new one as ready to paint. */
  it("ignores an earlier context settling after it was replaced", async () => {
    let settleFirst: (value: AttributeConsentContext) => void = () => undefined;
    const first = new Promise<AttributeConsentContext>((resolve) => {
      settleFirst = resolve;
    });
    attributeConsentStore.setContext(first);
    attributeConsentStore.setContext(new Promise(() => undefined));

    settleFirst(context());
    await first;

    expect(get(attributeConsentResolvedStore)).toBe(false);
    attributeConsentStore.clear();
  });

  it("is unresolved again once cleared", async () => {
    const pending = Promise.resolve(context());
    attributeConsentStore.setContext(pending);
    await pending;
    attributeConsentStore.clear();
    expect(get(attributeConsentResolvedStore)).toBe(false);
  });
});
