import { describe, expect, it } from "vitest";
import {
  dataOf,
  refOf,
  tagOf,
  type NotificationRef,
} from "$lib/utils/notifications/shownNotification";

const ref: NotificationRef = {
  identityNumber: 10_000n,
  origin: "https://app.example",
  accountNumber: 3n,
  canisterId: "un4fu-tqaaa-aaaab-qadjq-cai",
  id: 42n,
};

describe("tagOf", () => {
  it("names one notification of one app and account", () => {
    expect(tagOf(ref)).toBe("https://app.example|3|42");
  });

  it("tells the default account apart from a numbered one", () => {
    expect(tagOf({ ...ref, accountNumber: undefined })).not.toBe(tagOf(ref));
  });

  it("tells two notifications of the same app apart", () => {
    expect(tagOf({ ...ref, id: 43n })).not.toBe(tagOf(ref));
  });
});

describe("refOf", () => {
  it("reads back what was shown", () => {
    expect(refOf(dataOf(ref, "https://app.example/chats/7"))).toEqual(ref);
  });

  it("reads back the default account as no account", () => {
    const withoutAccount = { ...ref, accountNumber: undefined };
    expect(refOf(dataOf(withoutAccount, "https://app.example"))).toEqual(
      withoutAccount,
    );
  });

  it("reads nothing from data this worker did not write", () => {
    expect(refOf(undefined)).toBeUndefined();
    expect(refOf({})).toBeUndefined();
    expect(refOf({ ...dataOf(ref, "x"), id: "not a number" })).toBeUndefined();
  });
});
