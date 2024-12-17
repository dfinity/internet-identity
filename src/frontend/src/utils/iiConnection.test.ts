import {
  DeviceData,
  MetadataMapV2,
  _SERVICE,
} from "$generated/internet_identity_types";
import { DOMAIN_COMPATIBILITY } from "$src/featureFlags";
import {
  IdentityMetadata,
  RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
} from "$src/repositories/identityMetadata";
import { ActorSubclass, DerEncodedPublicKey, Signature } from "@dfinity/agent";
import { DelegationIdentity, WebAuthnIdentity } from "@dfinity/identity";
import { CredentialData, convertToCredentialData } from "./credential-devices";
import { AuthenticatedConnection, Connection } from "./iiConnection";
import { MultiWebAuthnIdentity } from "./multiWebAuthnIdentity";

const mockDevice: DeviceData = {
  alias: "mockDevice",
  metadata: [],
  origin: [],
  protection: { protected: null },
  pubkey: new Uint8Array(),
  key_type: { platform: null },
  purpose: { authentication: null },
  credential_id: [],
};

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

const mockActor = {
  identity_info: vi.fn().mockImplementation(async () => {
    // The `await` is necessary to make sure that the `getterResponse` is set before the test continues.
    infoResponse = await mockRawMetadata;
    return { Ok: { metadata: mockRawMetadata } };
  }),
  identity_metadata_replace: vi.fn().mockResolvedValue({ Ok: null }),
  lookup: vi.fn().mockResolvedValue([mockDevice]),
} as unknown as ActorSubclass<_SERVICE>;

beforeEach(() => {
  infoResponse = undefined;
  vi.clearAllMocks();
  vi.stubGlobal("location", {
    origin: "https://identity.internetcomputer.org",
  });
  DOMAIN_COMPATIBILITY.reset();
});

test("initializes identity metadata repository", async () => {
  const connection = new AuthenticatedConnection(
    "12345",
    MultiWebAuthnIdentity.fromCredentials([], undefined),
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
    MultiWebAuthnIdentity.fromCredentials([], undefined),
    mockDelegationIdentity,
    userNumber,
    mockActor
  );

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
  beforeEach(() => {
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
            rpId: string | undefined
          ) {
            return new MockMultiWebAuthnIdentity(credentials, rpId);
          }
          override sign() {
            this._actualIdentity = mockIdentity;
            return Promise.resolve(new ArrayBuffer(0) as Signature);
          }
        }
        return MockMultiWebAuthnIdentity.fromCredentials([], undefined);
      }
    );
  });

  it("login returns authenticated connection with expected rpID", async () => {
    DOMAIN_COMPATIBILITY.set(true);
    vi.stubGlobal("navigator", {
      // Supports RoR
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
    });
    const connection = new Connection("aaaaa-aa", mockActor);

    const loginResult = await connection.login(BigInt(12345));

    expect(loginResult.kind).toBe("loginSuccess");
    if (loginResult.kind === "loginSuccess") {
      expect(loginResult.connection).toBeInstanceOf(AuthenticatedConnection);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
        [convertToCredentialData(mockDevice)],
        "identity.ic0.app"
      );
    }
  });

  it("login returns authenticated connection without rpID if flag is not enabled", async () => {
    DOMAIN_COMPATIBILITY.set(false);
    vi.stubGlobal("navigator", {
      // Supports RoR
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
    });
    const connection = new Connection("aaaaa-aa", mockActor);

    const loginResult = await connection.login(BigInt(12345));

    expect(loginResult.kind).toBe("loginSuccess");
    if (loginResult.kind === "loginSuccess") {
      expect(loginResult.connection).toBeInstanceOf(AuthenticatedConnection);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
        [convertToCredentialData(mockDevice)],
        undefined
      );
    }
  });

  it("login returns authenticated connection without rpID if browser doesn't support it", async () => {
    DOMAIN_COMPATIBILITY.set(true);
    vi.stubGlobal("navigator", {
      // Supports RoR
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:133.0) Gecko/20100101 Firefox/133.0",
    });
    const connection = new Connection("aaaaa-aa", mockActor);

    const loginResult = await connection.login(BigInt(12345));

    expect(loginResult.kind).toBe("loginSuccess");
    if (loginResult.kind === "loginSuccess") {
      expect(loginResult.connection).toBeInstanceOf(AuthenticatedConnection);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledTimes(1);
      expect(MultiWebAuthnIdentity.fromCredentials).toHaveBeenCalledWith(
        [convertToCredentialData(mockDevice)],
        undefined
      );
    }
  });
});
