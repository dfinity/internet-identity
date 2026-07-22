import { get } from "svelte/store";
import { waitForConsentOrRestart } from "./consentRestart";
import {
  type Authorized,
  authorizationStore,
  authorizedStore,
} from "$lib/stores/authorization.store";
import { attributeConsentStore } from "$lib/stores/attributeConsent.store";

describe("waitForConsentOrRestart", () => {
  const authorizeBaseline = (): Authorized => {
    authorizationStore.authorize(Promise.resolve(BigInt(1)), "full-access");
    return get(authorizedStore)!;
  };

  beforeEach(() => {
    attributeConsentStore.clear();
    authorizationStore.reset();
  });

  it("resolves with the consent once the user commits their choice", async () => {
    const pending = waitForConsentOrRestart(authorizeBaseline());

    attributeConsentStore.setConsent({ attributes: [] });

    await expect(pending).resolves.toEqual({
      type: "consent",
      consent: { attributes: [] },
    });
  });

  it("restarts when the authorization is cleared (identity switch → account selection)", async () => {
    const pending = waitForConsentOrRestart(authorizeBaseline());

    authorizationStore.reset();

    await expect(pending).resolves.toEqual({ type: "restart" });
  });

  it("restarts when the authorization is replaced by a new entry", async () => {
    const pending = waitForConsentOrRestart(authorizeBaseline());

    authorizationStore.authorize(Promise.resolve(BigInt(2)), "read-only");

    await expect(pending).resolves.toEqual({ type: "restart" });
  });

  it("stays pending while the baseline authorization is unchanged", async () => {
    const pending = waitForConsentOrRestart(authorizeBaseline());

    const sentinel = Symbol("pending");
    await expect(
      Promise.race([pending, Promise.resolve(sentinel)]),
    ).resolves.toBe(sentinel);
  });
});
