import * as React from "react";
import { render, fireEvent, waitFor, screen } from "@testing-library/react";
import "@testing-library/jest-dom/extend-expect";
import RemoveIdentity from "../RemoveIdentity";
import { testGlobal } from "../../../../../setupTests";
const { mockActor } = testGlobal;
describe("RemoveIdentity", () => {
  const testUser = BigInt(1234);
  const testPublicKey = [1, 2, 3];

  it("should render without crashing", async () => {
    const wrapper = render(<RemoveIdentity />);
    const removeForm = await screen.findByTestId("removeForm");
    expect(removeForm).toBeTruthy();
  });
  it("should allow a user to remove an identity", async () => {
    const wrapper = render(<RemoveIdentity />);
    const removeForm = (await screen.findByTestId(
      "removeForm"
    )) as HTMLFormElement;
    const removeUser = removeForm.querySelector(
      "#removeUser"
    ) as HTMLInputElement;
    const removePublicKey = removeForm.querySelector(
      "#removePublicKey"
    ) as HTMLInputElement;

    if (!removeUser || !removePublicKey) throw new Error("missing element");

    removeUser.value = testUser.toString();
    removePublicKey.value = testPublicKey.join(",");

    removeForm.submit();

    expect(mockActor.remove).toBeCalledWith(BigInt(1234), [1, 2, 3]);
  });
});
