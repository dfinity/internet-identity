import type { DeviceData } from "$lib/generated/internet_identity_types";
import { DOMAIN_COMPATIBILITY } from "$lib/state/featureFlags";
import { domainWarning } from "$lib/legacy/flows/manage";

const recoveryPhrase: DeviceData = {
  alias: "Recovery Phrase",
  origin: [],
  protection: { unprotected: null },
  // eslint-disable-next-line
  pubkey: undefined as any,
  key_type: { seed_phrase: null },
  purpose: { recovery: null },
  credential_id: [],
  metadata: [],
};

const authenticator: DeviceData = {
  alias: "My Authenticator",
  origin: [],
  protection: { unprotected: null },
  // eslint-disable-next-line
  pubkey: undefined as any,
  key_type: { unknown: null },
  purpose: { authentication: null },
  credential_id: [],
  metadata: [],
};

describe("recovery phrases don't have origin warnings", () => {
  describe("on legacy domain", () => {
    beforeEach(() => {
      vi.clearAllMocks();
      vi.stubGlobal("location", {
        origin: "https://identity.ic0.app",
      });
      // domainWarning is used only when DOMAIN_COMPATIBILITY is false
      DOMAIN_COMPATIBILITY.getFeatureFlag()?.set(false);
    });

    it("returns undefined for recovery phrase", () => {
      expect(domainWarning(recoveryPhrase)).toBe(undefined);
      expect(
        domainWarning({ ...recoveryPhrase, origin: ["https://elsewhere"] }),
      ).toBe(undefined);
    });

    it("no origin is not warning", () => {
      expect(domainWarning({ ...authenticator, origin: [] })).toBe(undefined);
    });

    it("legacy origin in not warning", () => {
      expect(
        domainWarning({
          ...authenticator,
          origin: ["https://identity.ic0.app"],
        }),
      ).toBeUndefined();
    });

    test("bad origin is warning", () => {
      expect(
        domainWarning({ ...authenticator, origin: ["https://elsewhere"] }),
      ).toBeDefined();
    });
  });

  describe("on internetcomputer.org", () => {
    beforeEach(() => {
      vi.clearAllMocks();
      vi.stubGlobal("location", {
        origin: "https://identity.internetcomputer.org",
      });
      // domainWarning is used only when DOMAIN_COMPATIBILITY is false
      DOMAIN_COMPATIBILITY.getFeatureFlag()?.set(false);
    });

    it("undefined for recovery phrase", () => {
      expect(domainWarning(recoveryPhrase)).toBe(undefined);
      expect(
        domainWarning({ ...recoveryPhrase, origin: ["https://elsewhere"] }),
      ).toBe(undefined);
    });

    it("no origin is warning", () => {
      expect(domainWarning({ ...authenticator, origin: [] })).toBeDefined();
    });

    it("legacy origin is warning", () => {
      expect(
        domainWarning({
          ...authenticator,
          origin: ["https://identity.ic0.app"],
        }),
      ).toBeDefined();
    });

    it("bad origin is warning", () => {
      expect(
        domainWarning({ ...authenticator, origin: ["https://elsewhere"] }),
      ).toBeDefined();
    });
  });
});
