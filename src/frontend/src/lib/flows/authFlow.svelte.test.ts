import { describe, it, expect, vi, beforeEach } from "vitest";

vi.mock("$app/environment", () => ({ browser: true }));
vi.mock("$app/navigation", () => ({ goto: vi.fn() }));

const sessionStoreValue = vi.hoisted(() => {
  let value = {
    nonce: "test-nonce",
    salt: new Uint8Array(32),
    actor: {} as Record<string, unknown>,
  };
  const subs = new Set<(v: typeof value) => void>();
  return {
    subscribe: (fn: (v: typeof value) => void) => {
      subs.add(fn);
      fn(value);
      return () => {
        subs.delete(fn);
      };
    },
    set: (v: typeof value) => {
      value = v;
      subs.forEach((fn) => fn(v));
    },
    get: () => value,
  };
});

const authenticationStoreValue = vi.hoisted(() => {
  let value: unknown = undefined;
  const subs = new Set<(v: unknown) => void>();
  return {
    subscribe: (fn: (v: unknown) => void) => {
      subs.add(fn);
      fn(value);
      return () => {
        subs.delete(fn);
      };
    },
    set: (v: unknown) => {
      value = v;
      subs.forEach((fn) => fn(v));
    },
    get: () => value,
  };
});

vi.mock("$lib/stores/session.store", () => ({
  sessionStore: sessionStoreValue,
}));

vi.mock("$lib/stores/authentication.store", () => ({
  authenticatedStore: authenticationStoreValue,
  authenticationStore: {
    set: vi.fn(),
    subscribe: (fn: (v: unknown) => void) => {
      fn(undefined);
      return () => {};
    },
  },
}));

vi.mock("$lib/stores/last-used-identities.store", () => ({
  lastUsedIdentitiesStore: {
    addLastUsedIdentity: vi.fn(),
    subscribe: (fn: (v: unknown) => void) => {
      fn({ identities: {}, selected: undefined });
      return () => {};
    },
  },
}));

vi.mock("$lib/globals", () => ({
  canisterId: "test-canister-id",
  frontendCanisterConfig: { dummy_auth: [] },
  backendCanisterConfig: { openid_configs: [[]] },
}));

vi.mock("$lib/utils/analytics/authenticationV2Funnel", () => ({
  AuthenticationV2Events: {},
  authenticationV2Funnel: {
    trigger: vi.fn(),
    addProperties: vi.fn(),
    init: vi.fn(),
    close: vi.fn(),
  },
}));

vi.mock("$lib/utils/authentication", () => ({
  authenticateWithJWT: vi.fn(),
  authenticateWithPasskey: vi.fn(),
  authenticateWithSession: vi.fn(),
}));

vi.mock("$lib/utils/openID", () => ({
  requestJWT: vi.fn(),
  decodeJWT: vi.fn((jwt: string) => {
    const parts = jwt.split(".");
    if (parts.length === 3) {
      const payload = JSON.parse(
        atob(parts[1]!.replace(/-/g, "+").replace(/_/g, "/")),
      );
      return {
        iss: payload.iss ?? "https://accounts.google.com",
        sub: payload.sub ?? "user123",
        aud: payload.aud ?? "client123",
        loginHint: payload.email ?? payload.sub ?? "user123",
        name: payload.name,
        email: payload.email,
      };
    }
    return {
      iss: "https://accounts.google.com",
      sub: "user123",
      aud: "client123",
      loginHint: "user@example.com",
      name: "Test User",
      email: "user@example.com",
    };
  }),
  extractIssuerTemplateClaims: vi.fn(() => []),
  selectAuthScopes: vi.fn(() => ["openid", "profile", "email"]),
  isOpenIdCancelError: vi.fn(() => false),
}));

vi.mock("$lib/legacy/features", () => ({
  features: { DUMMY_AUTH: false },
}));

vi.mock("$lib/utils/utils", () => ({
  isCanisterError: vi.fn(() => false),
  throwCanisterError: vi.fn((v: unknown) => v),
}));

vi.mock("$lib/utils/time", () => ({
  nanosToMillis: vi.fn((n: bigint) => Number(n) / 1_000_000),
}));

import { AuthFlow } from "./authFlow.svelte";
import { requestJWT } from "$lib/utils/openID";
import { authenticateWithJWT } from "$lib/utils/authentication";
import { isCanisterError } from "$lib/utils/utils";
import type { OpenIdConfig } from "$lib/generated/internet_identity_types";

const makeOpenIdConfig = (
  overrides: Partial<{
    issuer: string;
    client_id: string;
    name: string;
  }> = {},
): OpenIdConfig => ({
  issuer: "https://accounts.google.com",
  client_id: "client123",
  name: "Google",
  auth_uri: "https://accounts.google.com/o/oauth2/v2/auth",
  auth_scope: ["openid", "profile", "email"],
  logo: "<svg></svg>",
  jwks_uri: "https://www.googleapis.com/oauth2/v3/certs",
  fedcm_uri: [],
  email_verification: [],
  seed_jwks: [],
  ...overrides,
});

const makeJwt = (payload: Record<string, unknown>) => {
  const header = btoa(JSON.stringify({ alg: "RS256" }))
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=/g, "");
  const body = btoa(JSON.stringify(payload))
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=/g, "");
  return `${header}.${body}.signature`;
};

describe("AuthFlow.mode", () => {
  it("defaults to 'both'", () => {
    const flow = new AuthFlow();
    expect(flow.mode).toBe("both");
  });

  it("setMode changes the mode", () => {
    const flow = new AuthFlow();
    flow.setMode("signin");
    expect(flow.mode).toBe("signin");
    flow.setMode("signup");
    expect(flow.mode).toBe("signup");
  });
});

describe("AuthFlow disambiguation — openIdNotConnected", () => {
  const jwt = makeJwt({
    iss: "https://accounts.google.com",
    sub: "user123",
    aud: "client123",
    name: "Test User",
    email: "user@example.com",
  });

  beforeEach(() => {
    vi.mocked(requestJWT).mockResolvedValue(jwt);
    vi.mocked(authenticateWithJWT).mockRejectedValue({ type: "NoSuchAnchor" });
    vi.mocked(isCanisterError).mockReturnValue(true);
  });

  it("sets view to openIdNotConnected when mode=signin and result=signUp", async () => {
    const flow = new AuthFlow();
    flow.setMode("signin");

    const config = makeOpenIdConfig();
    const resultPromise = flow.continueWithOpenId(config);

    await new Promise((r) => setTimeout(r, 50));

    expect(flow.view).toBe("openIdNotConnected");
    expect(flow.configIssuer).toBe("https://accounts.google.com");
    expect(flow.userName).toBe("Test User");
    expect(flow.userEmail).toBe("user@example.com");

    void resultPromise;
  });

  it("cancelOpenIdDisambiguation clears parked state and returns to chooseMethod", async () => {
    const flow = new AuthFlow();
    flow.setMode("signin");

    const config = makeOpenIdConfig();
    void flow.continueWithOpenId(config);

    await new Promise((r) => setTimeout(r, 50));
    expect(flow.view).toBe("openIdNotConnected");

    flow.cancelOpenIdDisambiguation();

    expect(flow.view).toBe("chooseMethod");
    expect(flow.configIssuer).toBeUndefined();
    expect(flow.userName).toBeUndefined();
    expect(flow.userEmail).toBeUndefined();
  });
});

describe("AuthFlow disambiguation — openIdAlreadyLinked", () => {
  const jwt = makeJwt({
    iss: "https://accounts.google.com",
    sub: "user123",
    aud: "client123",
    name: "Test User",
    email: "user@example.com",
  });

  beforeEach(() => {
    vi.mocked(requestJWT).mockResolvedValue(jwt);
    vi.mocked(isCanisterError).mockReturnValue(false);
    vi.mocked(authenticateWithJWT).mockResolvedValue({
      identity: {} as never,
      identityNumber: BigInt(12345),
    });
    authenticationStoreValue.set({
      actor: {
        get_anchor_info: vi.fn(() =>
          Promise.resolve({
            name: ["Test User"],
            openid_credentials: [[]],
            created_at: [],
          }),
        ),
      },
      identity: {},
      identityNumber: BigInt(12345),
    });
  });

  it("sets view to openIdAlreadyLinked when mode=signup and result=signIn", async () => {
    const flow = new AuthFlow();
    flow.setMode("signup");

    const config = makeOpenIdConfig();
    const resultPromise = flow.continueWithOpenId(config);

    await new Promise((r) => setTimeout(r, 50));

    expect(flow.view).toBe("openIdAlreadyLinked");
    expect(flow.configIssuer).toBe("https://accounts.google.com");

    void resultPromise;
  });

  it("cancelOpenIdDisambiguation from alreadyLinked state returns to chooseMethod", async () => {
    const flow = new AuthFlow();
    flow.setMode("signup");

    void flow.continueWithOpenId(makeOpenIdConfig());
    await new Promise((r) => setTimeout(r, 50));

    expect(flow.view).toBe("openIdAlreadyLinked");

    flow.cancelOpenIdDisambiguation();

    expect(flow.view).toBe("chooseMethod");
  });
});

describe("AuthFlow.requestMethodSwitch / confirmMethodSwitch / cancelMethodSwitch", () => {
  it("requestMethodSwitch sets view to confirmMethodSwitch with pending state", () => {
    const flow = new AuthFlow();
    flow.requestMethodSwitch(BigInt(42), "passkey");

    expect(flow.view).toBe("confirmMethodSwitch");
    expect(flow.pendingMethodSwitch).toEqual({
      previousIdentityNumber: BigInt(42),
      newMethod: "passkey",
      providerIssuer: undefined,
    });
  });

  it("requestMethodSwitch stores providerIssuer for openid method", () => {
    const flow = new AuthFlow();
    flow.requestMethodSwitch(
      BigInt(99),
      "openid",
      "https://accounts.google.com",
    );

    expect(flow.pendingMethodSwitch?.providerIssuer).toBe(
      "https://accounts.google.com",
    );
  });

  it("cancelMethodSwitch clears pending state and returns to chooseMethod", () => {
    const flow = new AuthFlow();
    flow.requestMethodSwitch(BigInt(42), "passkey");

    flow.cancelMethodSwitch();

    expect(flow.view).toBe("chooseMethod");
    expect(flow.pendingMethodSwitch).toBeUndefined();
  });

  it("confirmMethodSwitch for passkey advances to setupOrUseExistingPasskey", () => {
    const flow = new AuthFlow();
    flow.requestMethodSwitch(BigInt(42), "passkey");

    flow.confirmMethodSwitch();

    expect(flow.view).toBe("setupOrUseExistingPasskey");
    expect(flow.pendingMethodSwitch).toBeUndefined();
  });

  it("confirmMethodSwitch for sso advances to signInWithSso", () => {
    const flow = new AuthFlow();
    flow.requestMethodSwitch(BigInt(42), "sso");

    flow.confirmMethodSwitch();

    expect(flow.view).toBe("signInWithSso");
    expect(flow.pendingMethodSwitch).toBeUndefined();
  });

  it("confirmMethodSwitch for openid returns to chooseMethod (picker drives the openid flow)", () => {
    const flow = new AuthFlow();
    flow.requestMethodSwitch(
      BigInt(42),
      "openid",
      "https://accounts.google.com",
    );

    flow.confirmMethodSwitch();

    expect(flow.view).toBe("chooseMethod");
    expect(flow.pendingMethodSwitch).toBeUndefined();
  });

  it("confirmMethodSwitch throws when no pending switch", () => {
    const flow = new AuthFlow();
    expect(() => flow.confirmMethodSwitch()).toThrow(
      "No pending method switch",
    );
  });
});
