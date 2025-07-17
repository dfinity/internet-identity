import { authnTemplates } from "$lib/templates/authenticateBox";
import { I18n } from "$lib/utils/i18n";
import { IDBFactory } from "fake-indexeddb";
import { html, render } from "lit-html";
import { vi } from "vitest";

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
      landingType: "firstTime",
    },
    useExisting: {
      slot: html``,
      landingType: "useExisting",
    },
    pick: {
      slot: html``,
      landingType: "pick",
    },
  });

  const useExisting = pages.useExisting({
    register: () => {},
    onSubmit: () => {},
    addDevice,
    recover,
    loginOpenIDGoogle: () => {},
  });
  render(useExisting, document.body);
  const addDeviceButton = document.querySelector(
    "#addNewDeviceButton",
  ) as HTMLButtonElement;
  addDeviceButton.click();
  expect(addDevice).toHaveBeenCalledWith(undefined);
  const input = document.querySelector("input") as HTMLInputElement;
  input.value = "123456";
  const recoverButton = document.querySelector(
    "#recoverButton",
  ) as HTMLButtonElement;
  addDeviceButton.click();
  expect(addDevice).toHaveBeenCalledWith(BigInt(123456));
  recoverButton.click();
  expect(recover).toHaveBeenCalledWith(BigInt(123456));
});
