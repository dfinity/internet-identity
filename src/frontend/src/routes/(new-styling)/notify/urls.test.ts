import { beforeAll, describe, expect, it, vi } from "vitest";

// `urls.ts` imports the real `remapToLegacyDomain`, whose module graph reaches
// the feature-flag store and touches `localStorage` at import time. Stub it and
// import dynamically so the module under test is the real one rather than a
// reimplementation of the rule being tested.
const fakeStorage = (): Storage => {
  const entries = new Map<string, string>();
  return {
    get length() {
      return entries.size;
    },
    clear: () => entries.clear(),
    getItem: (key: string) => entries.get(key) ?? null,
    key: (index: number) => [...entries.keys()][index] ?? null,
    removeItem: (key: string) => entries.delete(key),
    setItem: (key: string, value: string) => entries.set(key, value),
  };
};

let parseOrigin: typeof import("./urls").parseOrigin;
let resolveDestination: typeof import("./urls").resolveDestination;
let sameApp: typeof import("./urls").sameApp;

beforeAll(async () => {
  vi.stubGlobal("localStorage", fakeStorage());
  const mod = await import("./urls");
  parseOrigin = mod.parseOrigin;
  resolveDestination = mod.resolveDestination;
  sameApp = mod.sameApp;
});

const APP = "https://app.example";

describe("parseOrigin", () => {
  it("accepts an https origin", () => {
    expect(parseOrigin("https://app.example/some/path")).toBe(APP);
  });

  it("rejects a missing or empty value", () => {
    expect(parseOrigin(null)).toBeUndefined();
    expect(parseOrigin("")).toBeUndefined();
  });

  it("rejects something that isn't a URL", () => {
    expect(parseOrigin("not a url")).toBeUndefined();
  });

  it("rejects http on a remote host", () => {
    // Tamperable in transit, so not a secure context.
    expect(parseOrigin("http://app.example")).toBeUndefined();
  });

  it("accepts http on loopback, including a *.localhost subdomain", () => {
    expect(parseOrigin("http://localhost:8000")).toBe("http://localhost:8000");
    expect(parseOrigin("http://frontend.local.localhost:8000")).toBe(
      "http://frontend.local.localhost:8000",
    );
    expect(parseOrigin("http://127.0.0.1:4943")).toBe("http://127.0.0.1:4943");
  });

  it("rejects javascript: and data:, which would otherwise pass as origin null", () => {
    // Both report origin === "null", so without the scheme check two of them
    // compare equal and reach location.href — script execution on II's origin.
    expect(parseOrigin("javascript:alert(1)")).toBeUndefined();
    expect(
      parseOrigin("data:text/html,<script>alert(1)</script>"),
    ).toBeUndefined();
  });
});

describe("sameApp", () => {
  it("treats a canister's modern and legacy domains as one app", () => {
    // Consent is recorded against the effective origin, which is remapped to
    // ic0.app; the dApp's own links use the domain the user is browsing.
    expect(
      sameApp(
        "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io",
        "https://vt36r-2qaaa-aaaad-aad5a-cai.ic0.app",
      ),
    ).toBe(true);
    expect(
      sameApp(
        "https://vt36r-2qaaa-aaaad-aad5a-cai.icp.net",
        "https://vt36r-2qaaa-aaaad-aad5a-cai.ic0.app",
      ),
    ).toBe(true);
  });

  it("does not collapse two different canisters", () => {
    expect(
      sameApp(
        "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io",
        "https://aaaaa-2qaaa-aaaad-aad5a-cai.icp0.io",
      ),
    ).toBe(false);
  });

  it("does not treat an unrelated origin as the same app", () => {
    expect(sameApp(APP, "https://evil.example")).toBe(false);
  });
});

describe("resolveDestination", () => {
  it("falls back to the sender's origin when no target is given", () => {
    expect(resolveDestination(APP, null)).toBe(APP);
    expect(resolveDestination(APP, "")).toBe(APP);
  });

  it("honours a deep link on the sender's own origin", () => {
    expect(resolveDestination(APP, "https://app.example/thread/42")).toBe(
      "https://app.example/thread/42",
    );
  });

  it("honours a deep link on the sender's other domain form", () => {
    // The case that made real notifications land on "Nothing to open".
    const consented = "https://vt36r-2qaaa-aaaad-aad5a-cai.ic0.app";
    const deepLink = "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io/#markets/ICP";

    expect(resolveDestination(consented, deepLink)).toBe(deepLink);
  });

  it("refuses a target on another origin", () => {
    expect(resolveDestination(APP, "https://evil.example/x")).toBeUndefined();
  });

  it("refuses a javascript: target even when the origin also parses as null", () => {
    expect(resolveDestination("null", "javascript:alert(1)")).toBeUndefined();
  });

  it("honours a loopback deep link, so a locally served dApp works", () => {
    const origin = "http://frontend.local.localhost:8000";

    expect(resolveDestination(origin, `${origin}/#markets/ICP-ICPUSD`)).toBe(
      `${origin}/#markets/ICP-ICPUSD`,
    );
  });

  it("refuses a remote http target", () => {
    expect(
      resolveDestination("http://app.example", "http://app.example/x"),
    ).toBeUndefined();
  });
});
