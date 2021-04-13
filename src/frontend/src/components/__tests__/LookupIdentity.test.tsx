import * as React from "react";
import { render, fireEvent, waitFor, screen } from "@testing-library/react";
import "@testing-library/jest-dom/extend-expect";
import LookupIdentity from "../LookupIdentity";
import { testGlobal } from "../../../../../setupTests";
const { mockActor } = testGlobal;
describe("LookupIdentity", () => {
  const testUser = BigInt(1234);

  it("should render without crashing", async () => {
    const wrapper = render(<LookupIdentity />);
    const lookupForm = await screen.findByTestId("lookupForm");
    expect(lookupForm).toBeTruthy();
  });
  it("should allow a user to lookup an identity", async () => {
    const wrapper = render(<LookupIdentity />);
    const lookupForm = (await screen.findByTestId(
      "lookupForm"
    )) as HTMLFormElement;
    const lookupUser = lookupForm.querySelector(
      "#lookupUser"
    ) as HTMLInputElement;

    if (!lookupUser) throw new Error("missing element");

    lookupUser.value = testUser.toString();

    lookupForm.submit();

    expect(mockActor.lookup).toBeCalledWith(BigInt(1234));
  });
});
