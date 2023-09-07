import { constructPinIdentity, reconstructPinIdentity } from "./pinIdentity";

describe("pin identity", () => {
  test("identity can be reconstructed", async () => {
    const { identity, pinIdentityMaterial } = await constructPinIdentity({
      pin: "123456",
    });
    const reconstructed = await reconstructPinIdentity({
      pin: "123456",
      pinIdentityMaterial,
    });

    expect(reconstructed.getPrincipal().toText()).toBe(
      identity.getPrincipal().toText()
    );
    expect(new Uint8Array(reconstructed.getPublicKey().toDer())).toStrictEqual(
      new Uint8Array(identity.getPublicKey().toDer())
    );
  });
});
