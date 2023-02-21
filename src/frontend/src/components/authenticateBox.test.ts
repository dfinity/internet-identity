import { render, html } from "lit-html";
import { authnTemplates } from "./authenticateBox";

test("anchors are forwarded", async () => {
  const addDevice: (anchor?: bigint) => void = jest.fn();
  const recover: (anchor?: bigint) => void = jest.fn();
  const pages = authnTemplates({
    register: () => {},
    onSubmit: () => {},
    addDevice,
    recover,
    firstTime: {
      slot: html``,
      useExistingText: "",
      createAnchorText: "",
    },
    useExisting: {
      slot: html``,
    },
    pick: {
      slot: html``,
    },
  });

  const useExisting = pages.useExisting();
  render(useExisting, document.body);
  const addDeviceButton = document.querySelector(
    "#addNewDeviceButton"
  ) as HTMLButtonElement;
  addDeviceButton.click();
  expect(addDevice).toHaveBeenCalledWith(undefined);
  const input = document.querySelector("input") as HTMLInputElement;
  input.value = "123456";
  const recoverButton = document.querySelector(
    "#recoverButton"
  ) as HTMLButtonElement;
  addDeviceButton.click();
  expect(addDevice).toHaveBeenCalledWith(BigInt(123456));
  recoverButton.click();
  expect(recover).toHaveBeenCalledWith(BigInt(123456));
});
