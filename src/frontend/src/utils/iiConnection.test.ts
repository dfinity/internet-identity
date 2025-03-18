import {
  DeviceData,
  InternetIdentityInit,
  MetadataMapV2,
  _SERVICE,
} from "$generated/internet_identity_types";
import { DOMAIN_COMPATIBILITY } from "$src/featureFlags";
import {
  IdentityMetadata,
  RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
} from "$src/repositories/identityMetadata";
import { ActorSubclass, DerEncodedPublicKey, Signature } from "@dfinity/agent";
import { DelegationIdentity } from "@dfinity/identity";
import { IDBFactory } from "fake-indexeddb";
import { clear as idbClear } from "idb-keyval";
import {
  CredentialData,
  convertToValidCredentialData,
} from "./credential-devices";
import { AuthenticatedConnection, Connection } from "./iiConnection";
import { MultiWebAuthnIdentity } from "./multiWebAuthnIdentity";
import { WebAuthnIdentity } from "./webAuthnIdentity";

const createMockDevice = (origin?: string): DeviceData => ({
  alias: "mockDevice",
  metadata: [],
  origin: origin !== undefined ? [origin] : [],
  protection: { protected: null },
  pubkey: new Uint8Array(),
  key_type: { platform: null },
  purpose: { authentication: null },
  credential_id: [Uint8Array.from([0, 0, 0, 0, 0])],
});
const mockDevice = createMockDevice();

const mockDelegationIdentity = {
  getDelegation() {
    return {
      delegations: [],
    };
  },
} as unknown as DelegationIdentity;

const recoveryPageShownTimestampMillis = 1234567890;
const mockRawMetadata: MetadataMapV2 = [
  [
    RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
    { String: String(recoveryPageShownTimestampMillis) },
  ],
];
const mockIdentityMetadata: IdentityMetadata = {
  recoveryPageShownTimestampMillis,
};

// Used to await that the getter has resolved.
let infoResponse: MetadataMapV2 | null | undefined = null;

const DEFAULT_INIT: InternetIdentityInit = {
  analytics_config: [],
  archive_config: [],
  assigned_user_number_range: [],
  canister_creation_cycles_cost: [],
  captcha_config: [],
  openid_google: [],
  register_rate_limit: [],
  related_origins: [
    [
      "https://identity.ic0.app",
      "https://identity.internetcomputer.org",
      "https://identity.icp0.io",
    ],
  ],
  fetch_root_key: [],
  enable_dapps_explorer: [],
  is_production: [],
};

const mockActor = {
  identity_info: vi.fn().mockImplementation(async () => {
    // The `await` is necessary to make sure that the `getterResponse` is set before the test continues.
    infoResponse = await mockRawMetadata;
    return { Ok: { metadata: mockRawMetadata } };
  }),
  identity_metadata_replace: vi.fn().mockResolvedValue({ Ok: null }),
  lookup: vi.fn().mockResolvedValue([mockDevice]),
} as unknown as ActorSubclass<_SERVICE>;

const currentOrigin = "https://identity.internetcomputer.org";

beforeAll(() => {
  // Initialize the IndexedDB global
  global.indexedDB = new IDBFactory();
});

beforeEach(async () => {
  await idbClear();
  infoResponse = undefined;
  vi.clearAllMocks();
  vi.stubGlobal("location", {
    origin: currentOrigin,
  });
  DOMAIN_COMPATIBILITY.reset();
});

test("initializes identity metadata repository", async () => {
  const connection = new AuthenticatedConnection(
    "12345",
    DEFAULT_INIT,
    MultiWebAuthnIdentity.fromCredentials([], undefined, undefined),
    mockDelegationIdentity,
    BigInt(1234),
    mockActor
  );

  await vi.waitFor(() => expect(infoResponse).toEqual(mockRawMetadata));

  expect(await connection.getIdentityMetadata()).toEqual(mockIdentityMetadata);
});

test("commits changes on identity metadata", async () => {
  const userNumber = BigInt(1234);
  const connection = new AuthenticatedConnection(
    "12345",
    DEFAULT_INIT,
    MultiWebAuthnIdentity.fromCredentials([], undefined, undefined),
    mockDelegationIdentity,
    userNumber,
    mockActor
  );

  expect(infoResponse).toBeUndefined();
  await vi.waitFor(() => expect(infoResponse).toEqual(mockRawMetadata));

  expect(await connection.getIdentityMetadata()).toEqual(mockIdentityMetadata);

  const newRecoveryPageShownTimestampMillis = 9876543210;
  connection.updateIdentityMetadata({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
  });

  expect(mockActor.identity_metadata_replace).not.toHaveBeenCalled();
  await connection.commitMetadata();

  expect(mockActor.identity_metadata_replace).toHaveBeenCalledTimes(1);
  expect(mockActor.identity_metadata_replace).toHaveBeenCalledWith(userNumber, [
    [
      RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
      { String: String(newRecoveryPageShownTimestampMillis) },
    ],
  ]);
});

describe("Connection.login", () => {
  let failSign = false;
  beforeEach(() => {
    failSign = false;
    vi.spyOn(MultiWebAuthnIdentity, "fromCredentials").mockImplementation(
      () => {
        const mockIdentity = {
          getPublicKey: () => {
            return {
              toDer: () => new ArrayBuffer(0) as DerEncodedPublicKey,
              toRaw: () => new ArrayBuffer(0),
              rawKey: () => new ArrayBuffer(0),
              derKey: () => new ArrayBuffer(0) as DerEncodedPublicKey,
            };
          },
        } as unknown as WebAuthnIdentity;

        class MockMultiWebAuthnIdentity extends MultiWebAuthnIdentity {
          static fromCredentials(
            credentials: CredentialData[],
            rpId: string | undefined,
            iframe: boolean | undefined
          ) {
            return new MockMultiWebAuthnIdentity(credentials, rpId, iframe);
          }

          override sign() {
            if (failSign) {
              throw new DOMException("Error test", "NotAllowedError");
            }
            this._actualIdentity = mockIdentity;
            return Promise.resolve(new ArrayBuffer(0) as Signature);
          }
        }

        return MockMultiWebAuthnIdentity.fromCredentials(
          [],
          undefined,
          undefined
        );
      }
    );
  });

  describe("domains compatibility flag enabled and browser support", () => {
    beforeEach(() => {
      DOMAIN_COMPATIBILITY.set(true);
      vi.stubGlobal("navigator", {
        // Supports RoR
        userAgent:
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
      });
    });

    it("login returns authenticated connection with expected rpID", async () => {
      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);

      const loginResult = await connection.login(BigInt(12345));

      expect(loginResult.kind).toBe("loginSuccess");
      if (loginResult.kind === "loginSuccess") {
        expect(loginResult.connection).toBeInstanceOf(AuthenticatedConnection);
        expect(loginResult.showAddCurrentDevice).toBe(false);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
          [convertToValidCredentialData(mockDevice)],
          "identity.ic0.app",
          true
        );
      }
    });

    it("login returns undefined RP ID if no related origins are in the config", async () => {
      const config: InternetIdentityInit = {
        ...DEFAULT_INIT,
        related_origins: [],
      };
      const connection = new Connection("aaaaa-aa", config, mockActor);

      const loginResult = await connection.login(BigInt(12345));

      expect(loginResult.kind).toBe("loginSuccess");
      if (loginResult.kind === "loginSuccess") {
        expect(loginResult.connection).toBeInstanceOf(AuthenticatedConnection);
        expect(loginResult.showAddCurrentDevice).toBe(false);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
          [convertToValidCredentialData(mockDevice)],
          undefined,
          false
        );
      }
    });

    it("connection excludes rpId when user cancels", async () => {
      // This one would fail because it's not the device the user is using at the moment.
      const currentOriginDevice: DeviceData = createMockDevice(currentOrigin);
      const currentOriginCredentialData =
        convertToValidCredentialData(currentOriginDevice);
      const currentDevice: DeviceData = createMockDevice();
      const currentDeviceCredentialData =
        convertToValidCredentialData(currentDevice);
      const mockActor = {
        identity_info: vi.fn().mockResolvedValue({ Ok: { metadata: [] } }),
        lookup: vi.fn().mockResolvedValue([currentOriginDevice, currentDevice]),
      } as unknown as ActorSubclass<_SERVICE>;
      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);

      failSign = true;
      const firstLoginResult = await connection.login(BigInt(12345));

      expect(firstLoginResult.kind).toBe("possiblyWrongWebAuthnFlow");
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
        expect.arrayContaining([
          currentOriginCredentialData,
          currentDeviceCredentialData,
        ]),
        undefined,
        // Do not use iframe
        false
      );

      failSign = false;
      const secondLoginResult = await connection.login(BigInt(12345));

      expect(secondLoginResult.kind).toBe("loginSuccess");
      if (secondLoginResult.kind === "loginSuccess") {
        expect(secondLoginResult.showAddCurrentDevice).toBe(true);
        expect(secondLoginResult.connection).toBeInstanceOf(
          AuthenticatedConnection
        );
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(2);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenNthCalledWith(
          2,
          expect.arrayContaining([
            currentDeviceCredentialData,
            currentDeviceCredentialData,
          ]),
          "identity.ic0.app",
          // Use iframe
          true
        );
      }
    });

    it("connection doesn't exclude rpId if user has only one domain", async () => {
      const currentOriginDevice: DeviceData = createMockDevice(currentOrigin);
      const currentOriginCredentialData =
        convertToValidCredentialData(currentOriginDevice);
      const currentOriginDevice2: DeviceData = createMockDevice(currentOrigin);
      const currentOriginCredentialData2 =
        convertToValidCredentialData(currentOriginDevice2);
      const mockActor = {
        identity_info: vi.fn().mockResolvedValue({ Ok: { metadata: [] } }),
        lookup: vi
          .fn()
          .mockResolvedValue([currentOriginDevice, currentOriginDevice2]),
      } as unknown as ActorSubclass<_SERVICE>;
      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);

      failSign = true;
      const firstLoginResult = await connection.login(BigInt(12345));

      expect(firstLoginResult.kind).toBe("webAuthnFailed");
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
        expect.arrayContaining([
          currentOriginCredentialData,
          currentOriginCredentialData2,
        ]),
        undefined,
        false
      );

      failSign = false;
      const secondLoginResult = await connection.login(BigInt(12345));

      expect(secondLoginResult.kind).toBe("loginSuccess");
      if (secondLoginResult.kind === "loginSuccess") {
        expect(secondLoginResult.showAddCurrentDevice).toBe(false);
        expect(secondLoginResult.connection).toBeInstanceOf(
          AuthenticatedConnection
        );
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(2);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenNthCalledWith(
          2,
          expect.arrayContaining([
            currentOriginCredentialData,
            currentOriginCredentialData2,
          ]),
          undefined,
          false
        );
      }
    });
  });

  describe("domains compatibility flag enabled and browser doesn't support", () => {
    beforeEach(() => {
      DOMAIN_COMPATIBILITY.set(true);
      vi.stubGlobal("navigator", {
        // Does NOT Supports RoR
        userAgent:
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:133.0) Gecko/20100101 Firefox/133.0",
      });
    });

    it("login returns authenticated connection with expected rpID", async () => {
      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);

      const loginResult = await connection.login(BigInt(12345));

      expect(loginResult.kind).toBe("loginSuccess");
      if (loginResult.kind === "loginSuccess") {
        expect(loginResult.showAddCurrentDevice).toBe(false);
        expect(loginResult.connection).toBeInstanceOf(AuthenticatedConnection);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
          [convertToValidCredentialData(mockDevice)],
          "identity.ic0.app",
          true
        );
      }
    });
  });

  describe("domains compatibility flag disabled", () => {
    beforeEach(() => {
      DOMAIN_COMPATIBILITY.set(false);
      vi.stubGlobal("navigator", {
        // Supports RoR
        userAgent:
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
      });
    });

    it("login returns authenticated connection without rpID if flag is not enabled", async () => {
      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);

      const loginResult = await connection.login(BigInt(12345));

      expect(loginResult.kind).toBe("loginSuccess");
      if (loginResult.kind === "loginSuccess") {
        expect(loginResult.showAddCurrentDevice).toBe(false);
        expect(loginResult.connection).toBeInstanceOf(AuthenticatedConnection);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
          [convertToValidCredentialData(mockDevice)],
          undefined,
          false
        );
      }
    });

    it("connection does not exclude rpId when user cancels", async () => {
      const currentOriginDevice: DeviceData = createMockDevice(currentOrigin);
      const currentOriginCredentialData =
        convertToValidCredentialData(currentOriginDevice);
      const currentDevice: DeviceData = createMockDevice();
      const currentDeviceCredentialData =
        convertToValidCredentialData(currentDevice);
      const mockActor = {
        identity_info: vi.fn().mockResolvedValue({ Ok: { metadata: [] } }),
        lookup: vi.fn().mockResolvedValue([currentOriginDevice, currentDevice]),
      } as unknown as ActorSubclass<_SERVICE>;
      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);

      failSign = true;
      const firstLoginResult = await connection.login(BigInt(12345));

      expect(firstLoginResult.kind).toBe("webAuthnFailed");
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
        expect.arrayContaining([
          currentOriginCredentialData,
          currentDeviceCredentialData,
        ]),
        undefined,
        false
      );

      failSign = false;
      const secondLoginResult = await connection.login(BigInt(12345));

      expect(secondLoginResult.kind).toBe("loginSuccess");
      if (secondLoginResult.kind === "loginSuccess") {
        expect(secondLoginResult.showAddCurrentDevice).toBe(false);
        expect(secondLoginResult.connection).toBeInstanceOf(
          AuthenticatedConnection
        );
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(2);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenNthCalledWith(
          2,
          expect.arrayContaining([
            currentDeviceCredentialData,
            currentOriginCredentialData,
          ]),
          undefined,
          false
        );
      }
    });
  });

  describe("domains compatibility flag disabled", () => {
    beforeEach(() => {
      DOMAIN_COMPATIBILITY.set(false);
      vi.stubGlobal("navigator", {
        // Supports RoR
        userAgent:
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
      });
    });

    it("login returns authenticated connection without rpID if flag is not enabled", async () => {
      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);

      const loginResult = await connection.login(BigInt(12345));

      expect(loginResult.kind).toBe("loginSuccess");
      if (loginResult.kind === "loginSuccess") {
        expect(loginResult.connection).toBeInstanceOf(AuthenticatedConnection);
        expect(loginResult.showAddCurrentDevice).toBe(false);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
          [convertToValidCredentialData(mockDevice)],
          undefined,
          false
        );
      }
    });

    it("connection does not exclude rpId when user cancels", async () => {
      const currentOriginDevice: DeviceData = createMockDevice(currentOrigin);
      const currentOriginCredentialData =
        convertToValidCredentialData(currentOriginDevice);
      const currentDevice: DeviceData = createMockDevice();
      const currentDeviceCredentialData =
        convertToValidCredentialData(currentDevice);
      const mockActor = {
        identity_info: vi.fn().mockResolvedValue({ Ok: { metadata: [] } }),
        lookup: vi.fn().mockResolvedValue([currentOriginDevice, currentDevice]),
      } as unknown as ActorSubclass<_SERVICE>;
      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);

      failSign = true;
      const firstLoginResult = await connection.login(BigInt(12345));

      expect(firstLoginResult.kind).toBe("webAuthnFailed");
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
        expect.arrayContaining([
          currentOriginCredentialData,
          currentDeviceCredentialData,
        ]),
        undefined,
        false
      );

      failSign = false;
      const secondLoginResult = await connection.login(BigInt(12345));

      expect(secondLoginResult.kind).toBe("loginSuccess");
      if (secondLoginResult.kind === "loginSuccess") {
        expect(secondLoginResult.connection).toBeInstanceOf(
          AuthenticatedConnection
        );
        expect(secondLoginResult.showAddCurrentDevice).toBe(false);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(2);
        expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenNthCalledWith(
          2,
          expect.arrayContaining([
            currentDeviceCredentialData,
            currentOriginCredentialData,
          ]),
          undefined,
          false
        );
      }
    });
  });

  describe("when a device credential id is missing", () => {
    it("connection does not use this device to authenticate", async () => {
      const deviceWithCredentialId: DeviceData =
        createMockDevice(currentOrigin);
      const deviceWithoutCredentialId: DeviceData = createMockDevice();
      deviceWithoutCredentialId.credential_id = [];
      const mockActor = {
        identity_info: vi.fn().mockResolvedValue({ Ok: { metadata: [] } }),
        lookup: vi
          .fn()
          .mockResolvedValue([
            deviceWithCredentialId,
            deviceWithoutCredentialId,
          ]),
      } as unknown as ActorSubclass<_SERVICE>;
      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);
      await connection.login(BigInt(12345));
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
        [convertToValidCredentialData(deviceWithCredentialId)],
        undefined,
        false
      );
    });
  });

  describe("when device credential id is invalid", () => {
    it("connection does not use this device to authenticate", async () => {
      const deviceValidCredentialId: DeviceData =
        createMockDevice(currentOrigin);
      const deviceInvalidCredentialId: DeviceData = createMockDevice();
      deviceInvalidCredentialId.credential_id = [Uint8Array.from([])];
      const mockActor = {
        identity_info: vi.fn().mockResolvedValue({ Ok: { metadata: [] } }),
        lookup: vi
          .fn()
          .mockResolvedValue([
            deviceValidCredentialId,
            deviceInvalidCredentialId,
          ]),
      } as unknown as ActorSubclass<_SERVICE>;
      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);
      await connection.login(BigInt(12345));
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
        [convertToValidCredentialData(deviceValidCredentialId)],
        undefined,
        false
      );
    });
  });

  describe("only pin device is available", () => {
    it("returns a custom error", async () => {
      const pinDevice: DeviceData = {
        alias: "mockDevice",
        metadata: [],
        origin: origin !== undefined ? [origin] : [],
        protection: { protected: null },
        pubkey: new Uint8Array(),
        key_type: { browser_storage_key: null },
        purpose: { authentication: null },
        credential_id: [Uint8Array.from([0, 0, 0, 0, 0])],
      };
      const mockActor = {
        lookup: vi.fn().mockResolvedValue([pinDevice]),
      } as unknown as ActorSubclass<_SERVICE>;

      const connection = new Connection("aaaaa-aa", DEFAULT_INIT, mockActor);

      const loginResult = await connection.login(BigInt(12345));

      expect(loginResult.kind).toBe("pinUserOtherDomain");
    });
  });

  describe("AuthenticatedConnection#add", () => {
    const alias = "alias";
    const keyType = { platform: null };
    const purpose = { authentication: null };
    const newPublicKey = new ArrayBuffer(0) as DerEncodedPublicKey;
    const protection = { protected: null };

    it("passes origin if origin less than 50 characters", async () => {
      const mockActor = {
        identity_info: vi.fn().mockResolvedValue({ Ok: { metadata: [] } }),
        add: vi.fn().mockResolvedValue(undefined),
      } as unknown as ActorSubclass<_SERVICE>;
      const userNumber = BigInt(12345);
      const connection = new AuthenticatedConnection(
        "aaaaa-aa",
        DEFAULT_INIT,
        MultiWebAuthnIdentity.fromCredentials([], undefined, undefined),
        mockDelegationIdentity,
        userNumber,
        mockActor
      );

      const origin = "https://identity.ic0.app";
      await connection.add(
        alias,
        keyType,
        purpose,
        newPublicKey,
        protection,
        origin
      );

      expect(mockActor.add).toHaveBeenCalledTimes(1);
      expect(mockActor.add).toHaveBeenCalledWith(userNumber, {
        alias,
        pubkey: Array.from(new Uint8Array(newPublicKey)),
        credential_id: [],
        key_type: keyType,
        purpose,
        protection,
        origin: [origin],
        metadata: [],
      });
    });

    it("doesn't pass origin if origin more than 50 characters", async () => {
      const mockActor = {
        identity_info: vi.fn().mockResolvedValue({ Ok: { metadata: [] } }),
        add: vi.fn().mockResolvedValue(undefined),
      } as unknown as ActorSubclass<_SERVICE>;
      const userNumber = BigInt(12345);
      const connection = new AuthenticatedConnection(
        "aaaaa-aa",
        DEFAULT_INIT,
        MultiWebAuthnIdentity.fromCredentials([], undefined, undefined),
        mockDelegationIdentity,
        userNumber,
        mockActor
      );

      const longOrigin = "https://thisisalongdominathatshouldbe50plus.ic0.app";
      await connection.add(
        alias,
        keyType,
        purpose,
        newPublicKey,
        protection,
        longOrigin
      );

      expect(mockActor.add).toHaveBeenCalledTimes(1);
      expect(mockActor.add).toHaveBeenCalledWith(userNumber, {
        alias,
        pubkey: Array.from(new Uint8Array(newPublicKey)),
        credential_id: [],
        key_type: keyType,
        purpose,
        protection,
        origin: [],
        metadata: [],
      });
    });

    it("handles no origin being passed", async () => {
      const mockActor = {
        identity_info: vi.fn().mockResolvedValue({ Ok: { metadata: [] } }),
        add: vi.fn().mockResolvedValue(undefined),
      } as unknown as ActorSubclass<_SERVICE>;
      const userNumber = BigInt(12345);
      const connection = new AuthenticatedConnection(
        "aaaaa-aa",
        DEFAULT_INIT,
        MultiWebAuthnIdentity.fromCredentials([], undefined, undefined),
        mockDelegationIdentity,
        userNumber,
        mockActor
      );

      await connection.add(
        alias,
        keyType,
        purpose,
        newPublicKey,
        protection,
        undefined
      );

      expect(mockActor.add).toHaveBeenCalledTimes(1);
      expect(mockActor.add).toHaveBeenCalledWith(userNumber, {
        alias,
        pubkey: Array.from(new Uint8Array(newPublicKey)),
        credential_id: [],
        key_type: keyType,
        purpose,
        protection,
        origin: [],
        metadata: [],
      });
    });

    it("passes credential id if present", async () => {
      const mockActor = {
        identity_info: vi.fn().mockResolvedValue({ Ok: { metadata: [] } }),
        add: vi.fn().mockResolvedValue(undefined),
      } as unknown as ActorSubclass<_SERVICE>;
      const userNumber = BigInt(12345);
      const connection = new AuthenticatedConnection(
        "aaaaa-aa",
        DEFAULT_INIT,
        MultiWebAuthnIdentity.fromCredentials([], undefined, undefined),
        mockDelegationIdentity,
        userNumber,
        mockActor
      );

      const credentialId = new Uint8Array([1, 2, 3, 4, 5]);
      await connection.add(
        alias,
        keyType,
        purpose,
        newPublicKey,
        protection,
        undefined,
        credentialId.buffer
      );

      expect(mockActor.add).toHaveBeenCalledTimes(1);
      expect(mockActor.add).toHaveBeenCalledWith(userNumber, {
        alias,
        pubkey: Array.from(new Uint8Array(newPublicKey)),
        credential_id: [Array.from(credentialId)],
        key_type: keyType,
        purpose,
        protection,
        origin: [],
        metadata: [],
      });
    });
  });
});
