import * as React from "react";
import { render, fireEvent, waitFor, screen } from "@testing-library/react";
import "@testing-library/jest-dom/extend-expect";
import AddIdentity from "../AddIdentity";
const { mockActor } = globalThis;
describe("AddIdentity", () => {
  const testUser = BigInt(1234);
  const testAlias = "my desktop";
  const testPublicKey = [1, 2, 3];
  const testCredential = "X9FrwMfmzj";
  it("should render without crashing", async () => {
    const wrapper = render(<AddIdentity />);
    const addForm = await screen.findByTestId("addForm");
    expect(addForm).toBeTruthy();
  });
  it("should allow a user to add an identity", async () => {
    const wrapper = render(<AddIdentity />);
    const addForm = (await screen.findByTestId("addForm")) as HTMLFormElement;
    const addUser = addForm.querySelector("#addUser") as HTMLInputElement;
    const addAlias = addForm.querySelector("#addAlias") as HTMLInputElement;
    const addPublicKey = addForm.querySelector(
      "#addPublicKey"
    ) as HTMLInputElement;
    if (!addUser || !addAlias || !addPublicKey)
      throw new Error("missing element");

    addUser.value = testUser.toString();
    addAlias.value = testAlias.toString();
    addPublicKey.value = testPublicKey.toString();

    addForm.submit();

    expect(mockActor.add).toBeCalledWith(
      BigInt(1234),
      "my desktop",
      [1, 2, 3],
      []
    );
  });
});
