import { HttpAgent } from "@icp-sdk/core/agent";
import { IDL } from "@icp-sdk/core/candid";
import { Principal } from "@icp-sdk/core/principal";
import { init as internetIdentityFrontendInit } from "$lib/generated/internet_identity_frontend_idl";
import type { InternetIdentityFrontendInit } from "$lib/generated/internet_identity_frontend_types";
import { toBase64 } from "$lib/utils/utils";
import { initGlobals, notificationsEnabled } from "./globals";

const CANISTER_ID = "rdmx6-jaaaa-aaaaa-aaadq-cai";
const BACKEND_ORIGIN = "https://backend.example.com";

const FRONTEND_INIT: InternetIdentityFrontendInit = {
  fetch_root_key: [],
  featured_dashboard_apps: [],
  backend_canister_id: Principal.fromText(CANISTER_ID),
  analytics_config: [],
  related_origins: [],
  backend_origin: BACKEND_ORIGIN,
  dev_csp: [],
  dummy_auth: [],
  feature_flags: [],
};

/// Serves the backend's `.config.did.bin`, with `notifications_enabled` left out
/// where `enabled` is `undefined`, as a canister older than the field does.
const serveConfig = (enabled: boolean | undefined) => {
  const config = new Uint8Array(
    enabled === undefined
      ? IDL.encode([IDL.Record({})], [{}])
      : IDL.encode(
          [IDL.Record({ notifications_enabled: IDL.Opt(IDL.Bool) })],
          [{ notifications_enabled: [enabled] }],
        ),
  );
  vi.stubGlobal(
    "fetch",
    vi.fn((url: string) =>
      url === `${BACKEND_ORIGIN}/.config.did.bin`
        ? Promise.resolve(new Response(config))
        : Promise.reject(new Error(`unexpected fetch of ${url}`)),
    ),
  );
};

describe("notificationsEnabled", () => {
  beforeEach(() => {
    document.body.dataset.canisterId = CANISTER_ID;
    document.body.dataset.canisterConfig = toBase64(
      IDL.encode(internetIdentityFrontendInit({ IDL }), [FRONTEND_INIT]),
    );
    // `initGlobals` warms the anonymous agent's subnet keys, which would go to the IC.
    vi.spyOn(HttpAgent.prototype, "fetchSubnetKeys").mockReturnValue(
      new Promise(() => {}),
    );
  });

  afterEach(() => {
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
  });

  it("is on where the backend notifies", async () => {
    serveConfig(true);
    await initGlobals();

    expect(notificationsEnabled()).toBe(true);
  });

  it("is off where the backend does not", async () => {
    serveConfig(false);
    await initGlobals();

    expect(notificationsEnabled()).toBe(false);
  });

  it("is off against a backend older than the field", async () => {
    serveConfig(undefined);
    await initGlobals();

    expect(notificationsEnabled()).toBe(false);
  });
});
