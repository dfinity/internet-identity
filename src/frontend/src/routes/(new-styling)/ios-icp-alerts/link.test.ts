import { describe, expect, it, vi } from "vitest";

const linkKeyActor = vi.fn<(linkKey: Uint8Array) => Promise<unknown>>();

vi.mock("$lib/utils/notifications/notificationAppKey", () => ({
  appPublicKey: () => Promise.resolve(new Uint8Array([1, 2, 3])),
}));
vi.mock("./appKeyActor", () => ({
  linkKeyActor: (linkKey: Uint8Array) => linkKeyActor(linkKey),
}));

const { linkWithToken } = await import("./link");

type KeyRecord = Parameters<typeof linkWithToken>[0];

/** The key is read through a mock, so the record the call carries is never looked at. */
const RECORD = {} as unknown as KeyRecord;

const TOKEN = {
  identityNumber: BigInt(10_000),
  linkKey: new Uint8Array(96).fill(7),
};

const answering = (response: unknown): void => {
  linkKeyActor.mockResolvedValue({
    link_notification_app: () => Promise.resolve(response),
  });
};

describe("linkWithToken", () => {
  it("links where the canister took the key", async () => {
    answering({ Ok: null });

    await expect(linkWithToken(RECORD, TOKEN)).resolves.toEqual({
      status: "linked",
    });
  });

  it("names the identity and the app's own key in the call", async () => {
    const link = vi.fn(() => Promise.resolve({ Ok: null }));
    linkKeyActor.mockResolvedValue({ link_notification_app: link });

    await linkWithToken(RECORD, TOKEN);

    expect(link).toHaveBeenCalledWith({
      anchor_number: TOKEN.identityNumber,
      app_key: new Uint8Array([1, 2, 3]),
    });
  });

  it("signs as the key the install URL carried", async () => {
    answering({ Ok: null });

    await linkWithToken(RECORD, TOKEN);

    expect(linkKeyActor).toHaveBeenCalledWith(TOKEN.linkKey);
  });

  it("asks for a fresh token on NoPendingLink", async () => {
    answering({ Err: { NoPendingLink: null } });

    await expect(linkWithToken(RECORD, TOKEN)).resolves.toEqual({
      status: "token-refused",
      reason: "NoPendingLink",
    });
  });

  it("ends the launch on the app's own key for KeyAlreadyInUse", async () => {
    answering({ Err: { KeyAlreadyInUse: null } });

    await expect(linkWithToken(RECORD, TOKEN)).resolves.toEqual({
      status: "key-refused",
      reason: "KeyAlreadyInUse",
    });
  });

  it("reports what the canister could not do as a failure", async () => {
    answering({ Err: { InternalCanisterError: "out of memory" } });

    await expect(linkWithToken(RECORD, TOKEN)).resolves.toEqual({
      status: "failed",
      reason: "InternalCanisterError",
    });
  });

  it("reports a call that never arrived as a failure", async () => {
    linkKeyActor.mockResolvedValue({
      link_notification_app: () => Promise.reject(new Error("no consensus")),
    });

    await expect(linkWithToken(RECORD, TOKEN)).resolves.toEqual({
      status: "failed",
      reason: "no consensus",
    });
  });

  it("asks for a fresh install URL where the key cannot build an actor", async () => {
    linkKeyActor.mockRejectedValue(new Error("the link key is damaged"));

    await expect(linkWithToken(RECORD, TOKEN)).resolves.toEqual({
      status: "token-refused",
      reason: "the link key is damaged",
    });
  });
});
