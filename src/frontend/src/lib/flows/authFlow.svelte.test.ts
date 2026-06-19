import { describe, it, expect, vi, beforeEach } from "vitest";

vi.mock("$app/environment", () => ({ browser: true }));
vi.mock("$app/navigation", () => ({ goto: vi.fn() }));

// Function declarations hoist to the top of the module scope, so they're
// callable from `vi.hoisted` factories. This keeps the per-store mock
// setup small while letting tests share the shape across files.

interface SessionStoreMockValue {
  nonce: string;
  salt: Uint8Array;
  actor: Record<string, unknown>;
}

function createSubscribableStore<T>(initial: T) {
  let value = initial;
  const subs = new Set<(v: T) => void>();
  return {
    subscribe: (fn: (v: T) => void) => {
      subs.add(fn);
      fn(value);
      return () => {
        subs.delete(fn);
      };
    },
    set: (v: T) => {
      value = v;
      subs.forEach((fn) => fn(value));
    },
  };
}

function createSessionStoreMock() {
  const initial: SessionStoreMockValue = {
    nonce: "test-nonce",
    salt: new Uint8Array(32),
    actor: {},
  };
  return { ...createSubscribableStore(initial), reset: () => {} };
}

function createAuthenticationStoreMock() {
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
    set: (v: unknown): Promise<void> => {
      value = v;
      subs.forEach((fn) => fn(value));
      return Promise.resolve();
    },
  };
}

function createAuthenticatedStoreMock() {
  return createSubscribableStore<{ actor: Record<string, unknown> }>({
    actor: {},
  });
}

function createLastUsedIdentitiesStoreMock() {
  const store = createSubscribableStore<{
    identities: Record<string, unknown>;
    selected?: unknown;
  }>({ identities: {} });
  return {
    ...store,
    addLastUsedIdentity: () => {},
    restoreIdentity: vi.fn(),
    selectIdentity: () => {},
  };
}

const sessionStoreValue = vi.hoisted(() => createSessionStoreMock());
const authenticationStoreValue = vi.hoisted(() =>
  createAuthenticationStoreMock(),
);
const authenticatedStoreValue = vi.hoisted(() =>
  createAuthenticatedStoreMock(),
);
const lastUsedIdentitiesStoreValue = vi.hoisted(() =>
  createLastUsedIdentitiesStoreMock(),
);

vi.mock("$lib/stores/session.store", () => ({
  sessionStore: sessionStoreValue,
}));

vi.mock("$lib/stores/authentication.store", () => ({
  authenticationStore: authenticationStoreValue,
  authenticatedStore: authenticatedStoreValue,
}));

vi.mock("$lib/stores/last-used-identities.store", () => ({
  lastUsedIdentitiesStore: lastUsedIdentitiesStoreValue,
}));

vi.mock("$lib/globals", () => ({
  backendCanisterConfig: { openid_configs: [[]] },
  frontendCanisterConfig: { dummy_auth: [[undefined]] },
  canisterId: "rdmx6-jaaaa-aaaaa-aaadq-cai",
}));

vi.mock("$lib/legacy/features", () => ({
  features: { DUMMY_AUTH: false },
}));

vi.mock("$lib/utils/analytics/authenticationV2Funnel", () => ({
  AuthenticationV2Events: {
    SelectMethodScreen: "SelectMethodScreen",
    ContinueWithPasskeyScreen: "ContinueWithPasskeyScreen",
    UseExistingPasskey: "UseExistingPasskey",
    EnterNameScreen: "EnterNameScreen",
    StartWebauthnCreation: "StartWebauthnCreation",
    ContinueWithOpenID: "ContinueWithOpenID",
    LoginWithOpenID: "LoginWithOpenID",
    RegisterWithOpenID: "RegisterWithOpenID",
    SuccessfulPasskeyRegistration: "SuccessfulPasskeyRegistration",
    SuccessfulOpenIDRegistration: "SuccessfulOpenIDRegistration",
  },
  authenticationV2Funnel: {
    trigger: vi.fn(),
    addProperties: vi.fn(),
    init: vi.fn(),
    close: vi.fn(),
  },
}));

const requestJWTMock = vi.hoisted(() => vi.fn());
const decodeJWTMock = vi.hoisted(() => vi.fn());

vi.mock("$lib/utils/openID", () => ({
  requestJWT: requestJWTMock,
  decodeJWT: decodeJWTMock,
  selectAuthScopes: (scopes: string[]) => scopes,
  extractIssuerTemplateClaims: () => [],
}));

const authenticateWithJWTMock = vi.hoisted(() => vi.fn());

vi.mock("$lib/utils/authentication", () => ({
  authenticateWithJWT: authenticateWithJWTMock,
  authenticateWithPasskey: vi.fn(),
  authenticateWithSession: vi.fn(),
}));

vi.mock("$lib/utils/utils", () => ({
  isCanisterError: (error: unknown): boolean =>
    typeof error === "object" && error !== null && "type" in error,
  throwCanisterError: <T>(v: T) => v,
}));

import { AuthFlow } from "./authFlow.svelte";
import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import type { OpenIdConfig } from "$lib/generated/internet_identity_types";

describe("AuthFlow — mode", () => {
  it("defaults to 'both'", () => {
    const flow = new AuthFlow({ trackLastUsed: false });
    expect(flow.mode).toBe("both");
  });

  it("setMode updates the current mode", () => {
    const flow = new AuthFlow({ trackLastUsed: false });
    flow.setMode("signin");
    expect(flow.mode).toBe("signin");
    flow.setMode("signup");
    expect(flow.mode).toBe("signup");
  });
});

describe("AuthFlow — method-switch disambiguation", () => {
  let flow: AuthFlow;

  beforeEach(() => {
    flow = new AuthFlow({ trackLastUsed: false });
  });

  const previousIdentity: LastUsedIdentity = {
    identityNumber: BigInt(42),
    authMethod: {
      passkey: { credentialId: new Uint8Array([1, 2, 3]) },
    },
    lastUsedTimestampMillis: 0,
  };

  it("requestMethodSwitch parks state and transitions view", () => {
    flow.requestMethodSwitch({
      previousIdentity,
      newMethod: "passkey",
      signedInIdentityNumber: BigInt(99),
    });
    expect(flow.view).toBe("confirmMethodSwitch");
    expect(flow.pendingMethodSwitch).toEqual({
      previousIdentity,
      newMethod: "passkey",
      signedInIdentityNumber: BigInt(99),
    });
  });

  it("requestMethodSwitch preserves optional provider info for openid", () => {
    flow.requestMethodSwitch({
      previousIdentity,
      newMethod: "openid",
      signedInIdentityNumber: BigInt(99),
      providerIssuer: "https://accounts.google.com",
      providerName: "Google",
    });
    expect(flow.pendingMethodSwitch?.providerIssuer).toBe(
      "https://accounts.google.com",
    );
    expect(flow.pendingMethodSwitch?.providerName).toBe("Google");
  });

  it("confirmMethodSwitch returns the signed-in identity number and clears state", () => {
    flow.requestMethodSwitch({
      previousIdentity,
      newMethod: "sso",
      signedInIdentityNumber: BigInt(123),
    });
    const result = flow.confirmMethodSwitch();
    expect(result).toBe(BigInt(123));
    expect(flow.view).toBe("chooseMethod");
    expect(flow.pendingMethodSwitch).toBeUndefined();
  });

  it("confirmMethodSwitch throws when no pending switch", () => {
    expect(() => flow.confirmMethodSwitch()).toThrow();
  });

  it("cancelMethodSwitch clears state and returns to chooseMethod", () => {
    flow.requestMethodSwitch({
      previousIdentity,
      newMethod: "passkey",
      signedInIdentityNumber: BigInt(99),
    });
    flow.cancelMethodSwitch();
    expect(flow.view).toBe("chooseMethod");
    expect(flow.pendingMethodSwitch).toBeUndefined();
  });

  it("cancelMethodSwitch does not touch the store (writes are deferred)", () => {
    const restoreSpy = lastUsedIdentitiesStoreValue.restoreIdentity;
    restoreSpy.mockClear();
    flow.requestMethodSwitch({
      previousIdentity,
      newMethod: "openid",
      signedInIdentityNumber: BigInt(99),
    });
    flow.cancelMethodSwitch();
    expect(restoreSpy).not.toHaveBeenCalled();
  });

  it("cancelMethodSwitch without a pending switch does not touch the store", () => {
    const restoreSpy = lastUsedIdentitiesStoreValue.restoreIdentity;
    restoreSpy.mockClear();
    flow.cancelMethodSwitch();
    expect(restoreSpy).not.toHaveBeenCalled();
  });
});

describe("AuthFlow — OpenID disambiguation cleanup", () => {
  let flow: AuthFlow;

  beforeEach(() => {
    flow = new AuthFlow({ trackLastUsed: false });
  });

  it("confirmOpenIdSignUp throws when no JWT is parked", async () => {
    await expect(flow.confirmOpenIdSignUp()).rejects.toThrow(
      "No pending OpenID or SSO sign-up",
    );
  });

  it("confirmOpenIdSignIn throws when no pending sign-in", () => {
    expect(() => flow.confirmOpenIdSignIn()).toThrow(
      "No pending OpenID sign-in",
    );
  });

  it("cancelOpenIdDisambiguation returns to chooseMethod without throwing", () => {
    flow.cancelOpenIdDisambiguation();
    expect(flow.view).toBe("chooseMethod");
    expect(flow.configIssuer).toBeUndefined();
    expect(flow.userName).toBeUndefined();
    expect(flow.userEmail).toBeUndefined();
  });
});

describe("AuthFlow — display getters", () => {
  it("userName and userEmail are undefined when no JWT is parked", () => {
    const flow = new AuthFlow({ trackLastUsed: false });
    expect(flow.userName).toBeUndefined();
    expect(flow.userEmail).toBeUndefined();
  });

  it("configIssuer is undefined when no OIDC sign-up is parked", () => {
    const flow = new AuthFlow({ trackLastUsed: false });
    expect(flow.configIssuer).toBeUndefined();
  });

  it("pendingMethodSwitch is undefined by default", () => {
    const flow = new AuthFlow({ trackLastUsed: false });
    expect(flow.pendingMethodSwitch).toBeUndefined();
  });
});

describe("AuthFlow — continueWithOpenId disambiguation", () => {
  // The OpenIdConfig generated IDL uses Candid optional tuples (`[]` / `[v]`)
  // for optional fields; we leave them empty for these tests.
  const testConfig: OpenIdConfig = {
    issuer: "https://accounts.google.com",
    name: "Google",
    logo: "<svg/>",
    client_id: "test-client",
    auth_uri: "https://accounts.google.com/auth",
    fedcm_uri: [],
    auth_scope: ["openid", "email"],
    jwks_uri: "",
    email_verification: [],
    seed_jwks: [],
  };

  const noSuchAnchorError = { type: "NoSuchAnchor", value: () => ({}) };

  beforeEach(() => {
    requestJWTMock.mockReset();
    decodeJWTMock.mockReset();
    authenticateWithJWTMock.mockReset();
  });

  it("parks JWT + configIssuer and transitions to openIdNotConnected when mode=signin and result is signUp", async () => {
    requestJWTMock.mockResolvedValue("fake-jwt");
    decodeJWTMock.mockReturnValue({
      iss: testConfig.issuer,
      sub: "user-1",
      name: "Alice",
      email: "alice@example.com",
    });
    authenticateWithJWTMock.mockRejectedValue(noSuchAnchorError);

    const flow = new AuthFlow({ trackLastUsed: false });
    flow.setMode("signin");
    const result = await flow.continueWithOpenId(testConfig);

    expect(result).toBeUndefined();
    expect(flow.view).toBe("openIdNotConnected");
    expect(flow.configIssuer).toBe(testConfig.issuer);
    expect(flow.userName).toBe("Alice");
    expect(flow.userEmail).toBe("alice@example.com");
  });

  it("returns signUp result without disambiguation when mode is signup", async () => {
    requestJWTMock.mockResolvedValue("fake-jwt");
    decodeJWTMock.mockReturnValue({
      iss: testConfig.issuer,
      sub: "user-1",
      name: "Bob",
      email: "bob@example.com",
    });
    authenticateWithJWTMock.mockRejectedValue(noSuchAnchorError);

    const flow = new AuthFlow({ trackLastUsed: false });
    flow.setMode("signup");
    const result = await flow.continueWithOpenId(testConfig);

    expect(result).toEqual({
      type: "signUp",
      name: "Bob",
      email: "bob@example.com",
    });
    expect(flow.view).not.toBe("openIdNotConnected");
    expect(flow.configIssuer).toBe(testConfig.issuer);
  });

  it("returns signUp result without disambiguation when mode is both", async () => {
    requestJWTMock.mockResolvedValue("fake-jwt");
    decodeJWTMock.mockReturnValue({
      iss: testConfig.issuer,
      sub: "user-1",
      email: "carol@example.com",
    });
    authenticateWithJWTMock.mockRejectedValue(noSuchAnchorError);

    const flow = new AuthFlow({ trackLastUsed: false });
    flow.setMode("both");
    const result = await flow.continueWithOpenId(testConfig);

    expect(result).toEqual({
      type: "signUp",
      name: undefined,
      email: "carol@example.com",
    });
    expect(flow.view).not.toBe("openIdNotConnected");
  });

  it("transitions to setupNewIdentity when confirmOpenIdSignUp runs with no name in JWT", async () => {
    requestJWTMock.mockResolvedValue("fake-jwt");
    decodeJWTMock.mockReturnValue({
      iss: testConfig.issuer,
      sub: "user-1",
      email: "dave@example.com",
    });
    authenticateWithJWTMock.mockRejectedValue(noSuchAnchorError);

    const flow = new AuthFlow({ trackLastUsed: false });
    flow.setMode("signin");
    await flow.continueWithOpenId(testConfig);
    expect(flow.view).toBe("openIdNotConnected");

    const outcome = await flow.confirmOpenIdSignUp();
    expect(outcome).toBe("needs-name");
    expect(flow.view).toBe("setupNewIdentity");
  });

  it("cancelOpenIdDisambiguation clears parked JWT and configIssuer", async () => {
    requestJWTMock.mockResolvedValue("fake-jwt");
    decodeJWTMock.mockReturnValue({
      iss: testConfig.issuer,
      sub: "user-1",
      name: "Eve",
    });
    authenticateWithJWTMock.mockRejectedValue(noSuchAnchorError);

    const flow = new AuthFlow({ trackLastUsed: false });
    flow.setMode("signin");
    await flow.continueWithOpenId(testConfig);
    expect(flow.configIssuer).toBe(testConfig.issuer);

    flow.cancelOpenIdDisambiguation();
    expect(flow.configIssuer).toBeUndefined();
    expect(flow.userName).toBeUndefined();
    expect(flow.view).toBe("chooseMethod");
  });
});
