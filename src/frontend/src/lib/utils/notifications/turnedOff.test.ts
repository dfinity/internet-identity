import { beforeEach, describe, expect, it, vi } from "vitest";

vi.mock("idb-keyval", () => ({
  createStore: vi.fn(() => ({})),
  get: vi.fn(),
  set: vi.fn(),
  del: vi.fn(),
}));

import { del, get, set } from "idb-keyval";
import {
  clearNotificationsOff,
  notificationsTurnedOff,
  recordNotificationsOff,
} from "./turnedOff";

const IDENTITY = BigInt(10_000);

describe("notificationsTurnedOff", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("is off where this identity said so", async () => {
    vi.mocked(get).mockResolvedValue(true);

    await expect(notificationsTurnedOff(IDENTITY)).resolves.toBe(true);
  });

  it("is on where nothing was ever stored", async () => {
    vi.mocked(get).mockResolvedValue(undefined);

    await expect(notificationsTurnedOff(IDENTITY)).resolves.toBe(false);
  });

  it("is off where the browser's storage could not be read", async () => {
    vi.mocked(get).mockRejectedValue(new Error("storage is blocked"));

    await expect(notificationsTurnedOff(IDENTITY)).resolves.toBe(true);
  });
});

describe("recording and forgetting", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("reports a word it could not store", async () => {
    vi.mocked(set).mockRejectedValue(new Error("storage is full"));

    await expect(recordNotificationsOff(IDENTITY)).rejects.toThrow(
      "storage is full",
    );
  });

  it("reports a word it could not forget", async () => {
    vi.mocked(del).mockRejectedValue(new Error("storage is blocked"));

    await expect(clearNotificationsOff(IDENTITY)).rejects.toThrow(
      "storage is blocked",
    );
  });

  it("keeps the word per identity", async () => {
    vi.mocked(set).mockResolvedValue(undefined);

    await recordNotificationsOff(IDENTITY);

    expect(set).toHaveBeenCalledWith("10000", true, expect.anything());
  });
});
