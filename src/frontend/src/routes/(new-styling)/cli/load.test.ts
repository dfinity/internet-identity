import { describe, expect, it } from "vitest";
import { load, type CliParams } from "./+page";

// A valid base64url DER session public key, and the loopback callback the CLI
// listens on. Nothing secret travels in the fragment.
const PUBLIC_KEY = "cHVibGljLWtleQ";
const NONCE = "bm9uY2U";

const fragment = (domain?: string): string => {
  const params = new URLSearchParams();
  params.set("public_key", PUBLIC_KEY);
  params.set("callback", "http://127.0.0.1:8000/callback");
  params.set("nonce", NONCE);
  if (domain !== undefined) {
    params.set("domain", domain);
  }
  return params.toString();
};

// `load` is synchronous here; the `PageLoad` signature widens the return to
// MaybePromise<void | ...>, so narrow it back for the assertions.
const loadParams = (fragmentString: string): CliParams =>
  (
    load({
      url: new URL(`https://id.ai/cli#${fragmentString}`),
    } as Parameters<typeof load>[0]) as { params: CliParams }
  ).params;

const appOriginOf = (domain?: string): string | undefined => {
  const params = loadParams(fragment(domain));
  expect(params.kind).toBe("valid");
  return params.kind === "valid" ? params.appOrigin : undefined;
};

const expectInvalid = (domain: string): void => {
  expect(loadParams(fragment(domain)).kind).toBe("invalid");
};

describe("/cli load: app origin parsing", () => {
  it("reads a bare hostname as an https origin", () => {
    expect(appOriginOf("oisy.com")).toBe("https://oisy.com");
  });

  it("normalises a mixed-case hostname", () => {
    expect(appOriginOf("OiSy.CoM")).toBe("https://oisy.com");
  });

  it("keeps an explicit https origin", () => {
    expect(appOriginOf("https://oisy.com")).toBe("https://oisy.com");
  });

  it("keeps the port of a bare hostname", () => {
    expect(appOriginOf("oisy.com:8443")).toBe("https://oisy.com:8443");
  });

  it("accepts a trailing slash", () => {
    expect(appOriginOf("https://oisy.com/")).toBe("https://oisy.com");
  });

  it("canonicalises an explicit default port away", () => {
    expect(appOriginOf("https://oisy.com:443")).toBe("https://oisy.com");
    expect(appOriginOf("oisy.com:443")).toBe("https://oisy.com");
    expect(appOriginOf("http://localhost:80")).toBe("http://localhost");
  });

  it("is generic mode when the domain is absent", () => {
    expect(appOriginOf()).toBeUndefined();
  });

  it("is generic mode when the domain is empty", () => {
    expect(appOriginOf("")).toBeUndefined();
  });

  it("rejects a path, query or fragment", () => {
    expectInvalid("oisy.com/app");
    expectInvalid("https://oisy.com/app");
    expectInvalid("https://oisy.com?a=b");
    expectInvalid("https://oisy.com#a");
  });

  it("rejects userinfo", () => {
    expectInvalid("https://user@oisy.com");
    expectInvalid("user:pass@oisy.com");
  });

  it("rejects a scheme that is not http(s)", () => {
    expectInvalid("ftp://oisy.com");
    expectInvalid("javascript://oisy.com");
  });

  it("rejects an unparseable domain", () => {
    expectInvalid("https://");
    expectInvalid(":8000");
  });
});

describe("/cli load: http app origins", () => {
  it("accepts a local app served over http on a non-default port", () => {
    expect(appOriginOf("http://frontend.local.localhost:8000")).toBe(
      "http://frontend.local.localhost:8000",
    );
  });

  it("accepts http on localhost and the loopback literal", () => {
    expect(appOriginOf("http://localhost:5173")).toBe("http://localhost:5173");
    expect(appOriginOf("http://127.0.0.1:4943")).toBe("http://127.0.0.1:4943");
  });

  it("rejects http on a host that is not loopback", () => {
    expectInvalid("http://oisy.com");
    expectInvalid("http://notlocalhost.com");
  });
});
