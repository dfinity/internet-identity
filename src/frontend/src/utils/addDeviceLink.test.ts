import { addDeviceLink, getAddDeviceAnchor } from "./addDeviceLink";

test("add device link looks as expected", () => {
  expect(
    addDeviceLink({
      userNumber: BigInt(10000),
      origin: "https://identity.ic0.app",
    })
  ).toBe("https://identity.ic0.app/?action=add-device&anchor=10000");
  expect(
    addDeviceLink({
      userNumber: BigInt(10000),
      origin: "https://identity.internetcomputer.org",
    })
  ).toBe(
    "https://identity.internetcomputer.org/?action=add-device&anchor=10000"
  );
});

test("anchor is read from add-device link", () => {
  const link = addDeviceLink({
    userNumber: BigInt(10000),
    origin: "https://identity.ic0.app",
  });

  const url = new URL(link);

  Object.defineProperty(window, "location", { value: url });
  expect(getAddDeviceAnchor()).toBe(BigInt(10000));
});
