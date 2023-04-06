import { addDeviceLink, getAddDeviceAnchor } from "./addDeviceLink";

function onOrigin<T>(origin: string, fn: () => T): T {
  const oldOrigin = window.origin;
  Object.defineProperty(window, "origin", {
    writable: true,
    value: origin,
  });

  const res = fn();
  Object.defineProperty(window, "origin", {
    writable: true,
    value: oldOrigin,
  });

  return res;
}

test("add device link looks as expected", () => {
  onOrigin("https://identity.ic0.app", () => {
    expect(addDeviceLink({ userNumber: BigInt(10000) })).toBe(
      "https://identity.ic0.app/?action=add-device&anchor=10000"
    );
  });
  onOrigin("https://identity.internetcomputer.org", () => {
    expect(addDeviceLink({ userNumber: BigInt(10000) })).toBe(
      "https://identity.internetcomputer.org/?action=add-device&anchor=10000"
    );
  });
});

test("anchor is read from add-device link", () => {
  const link = onOrigin("https://identity.ic0.app", () =>
    addDeviceLink({ userNumber: BigInt(10000) })
  );

  const url = new URL(link);

  Object.defineProperty(window, "location", { value: url });
  expect(getAddDeviceAnchor()).toBe(BigInt(10000));
});
