import { beforeAll, describe, expect, it } from "vitest";
import {
  Delegation,
  DelegationChain,
  ECDSAKeyIdentity,
} from "@icp-sdk/core/identity";
import type { Signature } from "@icp-sdk/core/agent";
import { DelegationResultSchema } from "$lib/utils/transport/utils";
import type { AppDelegationRecord } from "$lib/stores/app-delegation.store";
import { chooseSilentDelegation } from "./delegation";

const IDENTITY = BigInt(42);
const OTHER_IDENTITY = BigInt(43);
const EXPIRES_AT = Date.now() + 60 * 60 * 1000;

let keyPair: CryptoKeyPair;

beforeAll(async () => {
  keyPair = (
    await ECDSAKeyIdentity.generate({ extractable: false })
  ).getKeyPair();
});

const record = (
  principal: string,
  identityNumber = IDENTITY,
): AppDelegationRecord => ({
  principal,
  identityNumber,
  keyPair,
  chainJson: "{}",
  expiresAtMillis: EXPIRES_AT,
});

const never = () => false;
const always = () => true;

describe("chooseSilentDelegation", () => {
  it("re-issues for a single identity with the accounts toggle off", () => {
    const only = record("aaaaa-aa");
    expect(
      chooseSilentDelegation({
        records: [only],
        hint: undefined,
        identityCount: 1,
        isMultipleAccountsEnabled: never,
      }),
    ).toEqual({ record: only });
  });

  it("needs a sign-in when nothing is stored for the origin", () => {
    expect(
      chooseSilentDelegation({
        records: [],
        hint: undefined,
        identityCount: 1,
        isMultipleAccountsEnabled: never,
      }),
    ).toEqual({ denial: "login_required" });
  });

  it("needs a sign-in when a hint names a principal it does not hold", () => {
    // Not `account_selection_required`: the app named an identity there is
    // nothing stored for, so no choice would help.
    expect(
      chooseSilentDelegation({
        records: [record("aaaaa-aa")],
        hint: "bbbbb-bb",
        identityCount: 1,
        isMultipleAccountsEnabled: never,
      }),
    ).toEqual({ denial: "login_required" });
  });

  it("asks the user to choose when the device knows several identities", () => {
    expect(
      chooseSilentDelegation({
        records: [record("aaaaa-aa")],
        hint: undefined,
        identityCount: 2,
        isMultipleAccountsEnabled: never,
      }),
    ).toEqual({ denial: "account_selection_required" });
  });

  it("asks the user to choose when the origin holds several delegations", () => {
    expect(
      chooseSilentDelegation({
        records: [record("aaaaa-aa"), record("bbbbb-bb", OTHER_IDENTITY)],
        hint: undefined,
        identityCount: 1,
        isMultipleAccountsEnabled: never,
      }),
    ).toEqual({ denial: "account_selection_required" });
  });

  it("asks the user to choose when they picked accounts per sign-in", () => {
    expect(
      chooseSilentDelegation({
        records: [record("aaaaa-aa")],
        hint: undefined,
        identityCount: 1,
        isMultipleAccountsEnabled: always,
      }),
    ).toEqual({ denial: "account_selection_required" });
  });

  it("re-issues on a matching hint despite every ambiguity", () => {
    // A principal names an identity and an account together, so it answers both
    // questions the guards above are asking.
    const wanted = record("bbbbb-bb", OTHER_IDENTITY);
    expect(
      chooseSilentDelegation({
        records: [record("aaaaa-aa"), wanted],
        hint: "bbbbb-bb",
        identityCount: 3,
        isMultipleAccountsEnabled: always,
      }),
    ).toEqual({ record: wanted });
  });
});

describe("extending a stored chain", () => {
  const canisterChain = (permissions?: string): DelegationChain =>
    DelegationChain.fromDelegations(
      [
        {
          delegation: new Delegation(
            new Uint8Array(32).fill(1),
            BigInt(EXPIRES_AT) * BigInt(1_000_000),
            undefined,
            permissions,
          ),
          signature: new Uint8Array(64) as unknown as Signature,
        },
      ],
      new Uint8Array(32).fill(2),
    );

  it("keeps a read-only restriction across storage and a local extension", async () => {
    // The restriction lives in what the canister signed, so it has to survive
    // both the JSON round-trip through IndexedDB and the second hop added here.
    // If it did not, a re-issued delegation would silently grant more than the
    // one the user approved, and would not verify against the canister's
    // signature either.
    const stored = JSON.stringify(canisterChain("queries").toJSON());
    const key = await ECDSAKeyIdentity.generate({ extractable: false });

    const extended = await DelegationChain.create(
      key,
      key.getPublicKey(),
      new Date(EXPIRES_AT),
      { previous: DelegationChain.fromJSON(JSON.parse(stored)) },
    );

    const encoded = DelegationResultSchema.encode(extended);
    expect(encoded.signerDelegation).toHaveLength(2);
    expect(encoded.signerDelegation[0].delegation).toMatchObject({
      permissions: "queries",
    });
  });

  it("does not outlive the delegation it extends", async () => {
    const key = await ECDSAKeyIdentity.generate({ extractable: false });
    const parent = canisterChain();

    const extended = await DelegationChain.create(
      key,
      key.getPublicKey(),
      new Date(EXPIRES_AT),
      { previous: parent },
    );

    const [inner, outer] = extended.delegations;
    expect(outer.delegation.expiration).toBeLessThanOrEqual(
      inner.delegation.expiration,
    );
  });
});
