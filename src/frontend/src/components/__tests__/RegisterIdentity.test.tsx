import * as React from "react";
import { render, fireEvent, waitFor, screen } from "@testing-library/react";
import "@testing-library/jest-dom/extend-expect";
import RegisterIdentity from "../RegisterIdentity";
import { testGlobal } from "../../../../../setupTests";
const { mockActor } = testGlobal;
describe("RegisterIdentity", () => {
  const testUser = BigInt(1234);
  const testAlias = "my desktop";
  const testPublicKey = [1, 2, 3];
  const testCredential = "X9FrwMfmzj";
  it("should render without crashing", async () => {
    const wrapper = render(<RegisterIdentity />);
    const registerForm = await screen.findByTestId("registerForm");
    expect(registerForm).toBeTruthy();
  });
  it("should allow a user to register an identity", async () => {
    const wrapper = render(<RegisterIdentity />);
    const registerForm = (await screen.findByTestId(
      "registerForm"
    )) as HTMLFormElement;
    const registerUser = registerForm.querySelector(
      "#registerUser"
    ) as HTMLInputElement;
    const registerAlias = registerForm.querySelector(
      "#registerAlias"
    ) as HTMLInputElement;
    const registerPublicKey = registerForm.querySelector(
      "#registerPublicKey"
    ) as HTMLInputElement;
    if (!registerUser || !registerAlias || !registerPublicKey)
      throw new Error("missing element");

    registerUser.value = testUser.toString();
    registerAlias.value = testAlias.toString();
    registerPublicKey.value = testPublicKey.toString();

    registerForm.submit();

    expect(mockActor.register).toBeCalledWith(
      BigInt(1234),
      "my desktop",
      [1, 2, 3],
      []
    );
  });
});
