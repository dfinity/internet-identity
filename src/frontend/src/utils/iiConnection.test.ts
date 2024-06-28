import { MetadataMapV2, _SERVICE } from "$generated/internet_identity_types";
import {
  AnchorMetadata,
  RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
} from "$src/repositories/anchorMetadata";
import { ActorSubclass } from "@dfinity/agent";
import { DelegationIdentity } from "@dfinity/identity";
import { AuthenticatedConnection } from "./iiConnection";
import { MultiWebAuthnIdentity } from "./multiWebAuthnIdentity";

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
const mockAnchorMetadata: AnchorMetadata = {
  recoveryPageShownTimestampMillis,
};
const expectedAnchorInfo = {
  devices: [],
  device_registration: [],
};

// Used to await that the getter has resolved.
let infoResponse: MetadataMapV2 | null | undefined = null;

const mockActor = {
  get_anchor_info: vi.fn().mockResolvedValue(expectedAnchorInfo),
  identity_info: vi.fn().mockImplementation(async () => {
    // The `await` is necessary to make sure that the `getterResponse` is set before the test continues.
    infoResponse = await mockRawMetadata;
    return { Ok: { metadata: mockRawMetadata } };
  }),
  identity_metadata_replace: vi.fn().mockResolvedValue({ Ok: null }),
} as unknown as ActorSubclass<_SERVICE>;

test("gets anchor info from actor", async () => {
  const connection = new AuthenticatedConnection(
    "12345",
    MultiWebAuthnIdentity.fromCredentials([]),
    mockDelegationIdentity,
    BigInt(1234),
    mockActor
  );
  const anchorInfo = await connection.getAnchorInfo();
  expect(anchorInfo).toEqual(expectedAnchorInfo);
  expect(mockActor.get_anchor_info).toHaveBeenCalledTimes(1);
});

test("initializes anchor metadata repository", async () => {
  const connection = new AuthenticatedConnection(
    "12345",
    MultiWebAuthnIdentity.fromCredentials([]),
    mockDelegationIdentity,
    BigInt(1234),
    mockActor
  );

  await vi.waitFor(() => expect(infoResponse).toEqual(mockRawMetadata));

  expect(await connection.getAnchorMetadata()).toEqual(mockAnchorMetadata);
});

test("comits changes on anchor metadata", async () => {
  const userNumber = BigInt(1234);
  const connection = new AuthenticatedConnection(
    "12345",
    MultiWebAuthnIdentity.fromCredentials([]),
    mockDelegationIdentity,
    userNumber,
    mockActor
  );

  await vi.waitFor(() => expect(infoResponse).toEqual(mockRawMetadata));

  expect(await connection.getAnchorMetadata()).toEqual(mockAnchorMetadata);

  const newRecoveryPageShownTimestampMillis = 9876543210;
  await connection.setPartialMetadata({
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
