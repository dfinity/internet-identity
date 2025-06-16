import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { get } from "svelte/store";
import {
  lastUsedIdentitiesStore,
  lastUsedIdentityStore,
} from "./last-used-identities.store";
import type {
  LastUsedIdentity,
  LastUsedIdentities,
} from "./last-used-identities.store";

// Mock the dependency: writableStored
vi.mock("$app/environment", () => ({
  browser: true, // Or false, depending on the test case
}));

// Helper function to create Uint8Array from string
const strToUint8Array = (str: string): Uint8Array =>
  new TextEncoder().encode(str);

describe("lastUsedIdentitiesStore", () => {
  const mockTimestamp1 = 1700000000000;
  const mockTimestamp2 = 1700000001000;
  const mockTimestamp3 = 1700000002000;

  const identity1 = BigInt("111");
  const name1 = "Test ID 1";
  const credId1 = strToUint8Array("cred-111");
  const identity2 = BigInt("222");
  const name2 = "Test ID 2";
  const credId2 = strToUint8Array("cred-222");

  beforeEach(() => {
    // Reset the store state and time before each test
    vi.useFakeTimers();
    vi.setSystemTime(mockTimestamp1);
    localStorage.clear();
    lastUsedIdentitiesStore.reset(); // Use the store's reset method
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("should initialize with an empty object", () => {
    expect(get(lastUsedIdentitiesStore).identities).toEqual({});
  });

  it("should add the first identity correctly", () => {
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
    });

    const expected: LastUsedIdentities = {
      [identity1.toString()]: {
        identityNumber: identity1,
        name: name1,
        authMethod: { passkey: { credentialId: credId1 } },
        lastUsedTimestampMillis: mockTimestamp1,
      },
    };
    expect(get(lastUsedIdentitiesStore).identities).toEqual(expected);
  });

  it("should add multiple identities with correct timestamps", () => {
    // Add first identity
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
    });

    // Advance time and add second identity
    vi.setSystemTime(mockTimestamp2);
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity2,
      name: name2,
      authMethod: { passkey: { credentialId: credId2 } },
    });

    const expected: LastUsedIdentities = {
      [identity1.toString()]: {
        identityNumber: identity1,
        name: name1,
        authMethod: { passkey: { credentialId: credId1 } },
        lastUsedTimestampMillis: mockTimestamp1,
      },
      [identity2.toString()]: {
        identityNumber: identity2,
        name: name2,
        authMethod: { passkey: { credentialId: credId2 } },
        lastUsedTimestampMillis: mockTimestamp2,
      },
    };
    expect(get(lastUsedIdentitiesStore).identities).toEqual(expected);
  });

  it("should update the timestamp when adding an existing identity", () => {
    // Add identity initially
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
    });
    expect(
      get(lastUsedIdentitiesStore).identities[identity1.toString()]
        .lastUsedTimestampMillis,
    ).toBe(mockTimestamp1);

    // Advance time and add the same identity again
    vi.setSystemTime(mockTimestamp3);
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
    });

    const expected: LastUsedIdentities = {
      [identity1.toString()]: {
        identityNumber: identity1,
        name: name1, // Name should remain the same from the *last* call
        authMethod: { passkey: { credentialId: credId1 } },
        lastUsedTimestampMillis: mockTimestamp3,
      },
    };
    expect(get(lastUsedIdentitiesStore).identities).toEqual(expected);
    expect(
      get(lastUsedIdentitiesStore).identities[identity1.toString()]
        .lastUsedTimestampMillis,
    ).toBe(mockTimestamp3);
  });

  it("should reset the store to an empty object", () => {
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
    });
    expect(get(lastUsedIdentitiesStore).identities).not.toEqual({}); // Ensure it's not empty

    lastUsedIdentitiesStore.reset();
    expect(get(lastUsedIdentitiesStore).identities).toEqual({});
  });
});

describe("lastUsedIdentityStore (derived store)", () => {
  const mockTimestamp1 = 1700000000000;
  const mockTimestamp2 = 1700000001000;
  const mockTimestamp3 = 1700000002000;

  const identity1 = BigInt("101");
  const name1 = "Derived ID 1";
  const credId1 = strToUint8Array("cred-101");
  const identity2 = BigInt("202");
  const name2 = "Derived ID 2";
  const credId2 = strToUint8Array("cred-202");
  const identity3 = BigInt("303");
  const name3 = "Derived ID 3";
  const credId3 = strToUint8Array("cred-303");

  beforeEach(() => {
    vi.useFakeTimers();
    vi.setSystemTime(mockTimestamp1);
    localStorage.clear();
    lastUsedIdentitiesStore.reset(); // Reset the source store
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  it("should be undefined when the source store is empty", () => {
    expect(get(lastUsedIdentityStore)).toBeUndefined();
  });

  it("should return the only identity when one is added", () => {
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
    });

    const expected: LastUsedIdentity = {
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
      lastUsedTimestampMillis: mockTimestamp1,
    };
    expect(get(lastUsedIdentityStore)).toEqual(expected);
  });

  it("should return the latest identity when multiple are added", () => {
    vi.setSystemTime(mockTimestamp1);
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity2,
      name: name2,
      authMethod: { passkey: { credentialId: credId2 } },
    });

    vi.setSystemTime(mockTimestamp2);
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
    });

    const expectedLatest: LastUsedIdentity = {
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
      lastUsedTimestampMillis: mockTimestamp2,
    };
    expect(get(lastUsedIdentityStore)).toEqual(expectedLatest);

    // Add identity 3 (at time 3) - Should become the latest
    vi.setSystemTime(mockTimestamp3);
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity3,
      name: name3,
      authMethod: { passkey: { credentialId: credId3 } },
    });
    const expectedNewest: LastUsedIdentity = {
      identityNumber: identity3,
      name: name3,
      authMethod: { passkey: { credentialId: credId3 } },
      lastUsedTimestampMillis: mockTimestamp3,
    };
    expect(get(lastUsedIdentityStore)).toEqual(expectedNewest);
  });

  it("should update when an existing identity becomes the latest again", () => {
    // Add 1 at time 1
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
    });

    // Add 2 at time 2 (latest is now 2)
    vi.setSystemTime(mockTimestamp2);
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity2,
      name: name2,
      authMethod: { passkey: { credentialId: credId2 } },
    });
    expect(get(lastUsedIdentityStore)?.identityNumber).toBe(identity2);

    // Add 1 again at time 3 (latest is now 1 again)
    vi.setSystemTime(mockTimestamp3);
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
    });

    const expected: LastUsedIdentity = {
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
      lastUsedTimestampMillis: mockTimestamp3,
    };
    expect(get(lastUsedIdentityStore)).toEqual(expected);
  });

  it("should become undefined after the source store is reset", () => {
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: identity1,
      name: name1,
      authMethod: { passkey: { credentialId: credId1 } },
    });
    expect(get(lastUsedIdentityStore)).toBeDefined();

    lastUsedIdentitiesStore.reset();
    expect(get(lastUsedIdentityStore)).toBeUndefined();
  });
});
