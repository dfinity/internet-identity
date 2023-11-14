import { DeviceData } from "$generated/internet_identity_types";
import { domainWarning } from "$src/flows/manage";

function onOrigin(origin: string, fn: () => void) {
  const oldOrigin = window.origin;
  Object.defineProperty(window, "origin", {
    writable: true,
    value: origin,
  });

  fn();
  Object.defineProperty(window, "origin", {
    writable: true,
    value: oldOrigin,
  });
}

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

test("recovery phrases don't have origin warnings", () => {
  onOrigin("https://identity.ic0.app", () => {
    expect(domainWarning(recoveryPhrase)).toBe(undefined);
    expect(
      domainWarning({ ...recoveryPhrase, origin: ["https://elsewhere"] })
    ).toBe(undefined);
  });

  onOrigin("https://identity.internetcomputer.org", () => {
    expect(domainWarning(recoveryPhrase)).toBe(undefined);
    expect(
      domainWarning({ ...recoveryPhrase, origin: ["https://elsewhere"] })
    ).toBe(undefined);
  });
});

test("no origin on legacy domain is not warning", () => {
  onOrigin("https://identity.ic0.app", () => {
    expect(domainWarning({ ...authenticator, origin: [] })).toBe(undefined);
  });
});

test("bad origin on legacy domain is warning", () => {
  onOrigin("https://identity.ic0.app", () => {
    expect(
      domainWarning({ ...authenticator, origin: ["https://elsewhere"] })
    ).toBeDefined();
  });
});

test("no origin on official domain is warning", () => {
  onOrigin("https://identity.internetcomputer.org", () => {
    expect(domainWarning({ ...authenticator, origin: [] })).toBeDefined();
  });
});

test("legacy origin on official domain is warning", () => {
  onOrigin("https://identity.internetcomputer.org", () => {
    expect(
      domainWarning({ ...authenticator, origin: ["https://identity.ic0.app"] })
    ).toBeDefined();
  });
});

test("bad origin on official domain is warning", () => {
  onOrigin("https://identity.internetcomputer.org", () => {
    expect(
      domainWarning({ ...authenticator, origin: ["https://elsewhere"] })
    ).toBeDefined();
  });
});
