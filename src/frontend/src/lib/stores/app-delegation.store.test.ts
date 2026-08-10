import {
  afterEach,
  beforeAll,
  beforeEach,
  describe,
  expect,
  it,
  vi,
} from "vitest";
import { IDBFactory } from "fake-indexeddb";
import { createStore as idbCreateStore, clear as idbClear } from "idb-keyval";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import {
  type AppDelegationRecord,
  appDelegationsForOrigin,
  discardAppDelegation,
  forgetAppDelegations,
  purgeAppDelegations,
  storeAppDelegation,
} from "./app-delegation.store";

const NOW = 1_700_000_000_000;
const FAR_FUTURE = NOW + 60 * 60 * 1000;
// Inside the 5 minute margin, so treated as already expired.
const NEAR_EXPIRY = NOW + 4 * 60 * 1000;

const ORIGIN = "https://docs.example.com";
const OTHER_ORIGIN = "https://chat.example.com";
const IDENTITY = BigInt(42);
const OTHER_IDENTITY = BigInt(43);

const TEST_STORE = idbCreateStore("ii-app-delegations", "origins");

let keyPair: CryptoKeyPair;

const record = (
  principal: string,
  overrides: Partial<AppDelegationRecord> = {},
): AppDelegationRecord => ({
  principal,
  identityNumber: IDENTITY,
  keyPair,
  chainJson: "{}",
  expiresAtMillis: FAR_FUTURE,
  ...overrides,
});

const principalsFor = async (origin: string): Promise<string[]> =>
  (await appDelegationsForOrigin(origin)).map(({ principal }) => principal);

beforeAll(async () => {
  global.indexedDB = new IDBFactory();
  keyPair = (
    await ECDSAKeyIdentity.generate({ extractable: false })
  ).getKeyPair();
});

beforeEach(async () => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW);
  await idbClear(TEST_STORE);
});

afterEach(() => {
  vi.useRealTimers();
});

describe("appDelegationsForOrigin", () => {
  it("round-trips a record", async () => {
    await storeAppDelegation(ORIGIN, record("aaaaa-aa"));

    const [stored] = await appDelegationsForOrigin(ORIGIN);
    expect(stored.principal).toBe("aaaaa-aa");
    // The non-extractable key pair has to survive structured clone, which is the
    // whole reason a record can be stored at all.
    expect(stored.keyPair.privateKey.extractable).toBe(false);
  });

  it("keeps origins apart", async () => {
    await storeAppDelegation(ORIGIN, record("aaaaa-aa"));

    expect(await principalsFor(OTHER_ORIGIN)).toEqual([]);
  });

  it("holds several records for one origin", async () => {
    await storeAppDelegation(ORIGIN, record("aaaaa-aa"));
    await storeAppDelegation(
      ORIGIN,
      record("bbbbb-bb", { identityNumber: OTHER_IDENTITY }),
    );

    expect(await principalsFor(ORIGIN)).toEqual(["aaaaa-aa", "bbbbb-bb"]);
  });

  it("replaces an earlier record for the same principal", async () => {
    await storeAppDelegation(ORIGIN, record("aaaaa-aa", { chainJson: "old" }));
    await storeAppDelegation(ORIGIN, record("aaaaa-aa", { chainJson: "new" }));

    const stored = await appDelegationsForOrigin(ORIGIN);
    expect(stored).toHaveLength(1);
    expect(stored[0].chainJson).toBe("new");
  });

  it("withholds a record inside the expiry margin", async () => {
    await storeAppDelegation(
      ORIGIN,
      record("aaaaa-aa", { expiresAtMillis: NEAR_EXPIRY }),
    );

    // Still nominally valid, but too close to expiry to hand out: it could die
    // between here and the IC validating it.
    expect(await principalsFor(ORIGIN)).toEqual([]);
  });

  it("keeps usable records when dropping expired ones", async () => {
    await storeAppDelegation(
      ORIGIN,
      record("aaaaa-aa", { expiresAtMillis: NEAR_EXPIRY }),
    );
    await storeAppDelegation(ORIGIN, record("bbbbb-bb"));

    expect(await principalsFor(ORIGIN)).toEqual(["bbbbb-bb"]);
  });
});

describe("forgetting and purging", () => {
  it("forgets every record for one origin and no others", async () => {
    await storeAppDelegation(ORIGIN, record("aaaaa-aa"));
    await storeAppDelegation(
      ORIGIN,
      record("bbbbb-bb", { identityNumber: OTHER_IDENTITY }),
    );
    await storeAppDelegation(OTHER_ORIGIN, record("ccccc-cc"));

    await forgetAppDelegations(ORIGIN);

    expect(await principalsFor(ORIGIN)).toEqual([]);
    expect(await principalsFor(OTHER_ORIGIN)).toEqual(["ccccc-cc"]);
  });

  it("discards one record and leaves its neighbours", async () => {
    await storeAppDelegation(ORIGIN, record("aaaaa-aa"));
    await storeAppDelegation(
      ORIGIN,
      record("bbbbb-bb", { identityNumber: OTHER_IDENTITY }),
    );

    await discardAppDelegation(ORIGIN, "aaaaa-aa");

    expect(await principalsFor(ORIGIN)).toEqual(["bbbbb-bb"]);
  });

  it("purges an identity across every origin, sparing the others", async () => {
    await storeAppDelegation(ORIGIN, record("aaaaa-aa"));
    await storeAppDelegation(
      ORIGIN,
      record("bbbbb-bb", { identityNumber: OTHER_IDENTITY }),
    );
    await storeAppDelegation(OTHER_ORIGIN, record("ccccc-cc"));

    await purgeAppDelegations(IDENTITY);

    expect(await principalsFor(ORIGIN)).toEqual(["bbbbb-bb"]);
    expect(await principalsFor(OTHER_ORIGIN)).toEqual([]);
  });

  it("forgets an origin it holds nothing for without complaint", async () => {
    await expect(forgetAppDelegations(ORIGIN)).resolves.toBeUndefined();
  });
});
