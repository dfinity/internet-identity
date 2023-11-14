import { I18n } from "$src/utils/i18n";
import { IDBFactory } from "fake-indexeddb";
import { html, render } from "lit-html";
import { vi } from "vitest";
import { authnTemplates } from "./authenticateBox";

beforeEach(() => {
  // Create a fresh IDB before each test
  global.indexedDB = new IDBFactory();
});

test("anchors are forwarded", () => {
  const addDevice: (anchor?: bigint) => void = vi.fn();
  const recover: (anchor?: bigint) => void = vi.fn();
  const pages = authnTemplates(new I18n("en"), {
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

  const useExisting = pages.useExisting({
    register: () => {},
    onSubmit: () => {},
    addDevice,
    recover,
  });
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
