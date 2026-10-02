import { HttpAgent } from "@icp-sdk/core/agent";
import { IDL } from "@icp-sdk/core/candid";
import { Principal } from "@icp-sdk/core/principal";
import { init as internetIdentityFrontendInit } from "$lib/generated/internet_identity_frontend_idl";
import type { InternetIdentityFrontendInit } from "$lib/generated/internet_identity_frontend_types";
import { toBase64 } from "$lib/utils/utils";
import { initGlobals, notificationsEnabledFor } from "./globals";

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

/// Serves `origins` as the backend's `.config.did.bin` does: the list exactly as the
/// operator configured it, since the canister stores its init arg verbatim.
const serveNotifyingOrigins = (origins: string[]) => {
  const config = new Uint8Array(
    IDL.encode(
      [
        IDL.Record({
          notifications_enabled_origins: IDL.Opt(IDL.Vec(IDL.Text)),
        }),
      ],
      [{ notifications_enabled_origins: [origins] }],
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

describe("notificationsEnabledFor", () => {
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

  it("matches an icp0.io entry by every gateway spelling of the app", async () => {
    serveNotifyingOrigins(["https://2vxsx-fae.icp0.io"]);
    await initGlobals();

    expect(notificationsEnabledFor("https://2vxsx-fae.ic0.app")).toBe(true);
    expect(notificationsEnabledFor("https://2vxsx-fae.icp0.io")).toBe(true);
    expect(notificationsEnabledFor("https://2vxsx-fae.icp.net")).toBe(true);
  });

  it("does not match another canister on the same gateway", async () => {
    serveNotifyingOrigins(["https://2vxsx-fae.icp0.io"]);
    await initGlobals();

    expect(notificationsEnabledFor("https://aaaaa-aa.icp0.io")).toBe(false);
    expect(notificationsEnabledFor("https://aaaaa-aa.ic0.app")).toBe(false);
  });
});
