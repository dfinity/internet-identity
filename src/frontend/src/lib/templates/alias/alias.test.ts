import { promptDeviceAliasPage } from "$lib/templates/alias/index";
import { I18n } from "$lib/legacy/i18n";
import { vi } from "vitest";

test("can be canceled", () => {
  const cancel = vi.fn();
  const ctn = vi.fn((_str) => {
    /* */
  });
  promptDeviceAliasPage(
    {
      title: "Title",
      continue: ctn,
      cancel,
      i18n: new I18n(),
    },
    document.body,
  );

  const elem = document.querySelector("#pickAliasCancel") as HTMLElement;
  elem.click();

  expect(cancel.mock.calls.length).toBe(1);
});

test("can be picked", () => {
  const cancel = vi.fn();
  const ctn = vi.fn((_str) => {
    /* */
  });
  promptDeviceAliasPage(
    {
      title: "Title",
      continue: ctn,
      cancel,
      i18n: new I18n(),
    },
    document.body,
  );

  const input = document.querySelector("#pickAliasInput") as HTMLInputElement;
  input.value = "foo";

  const button = document.querySelector("#pickAliasSubmit") as HTMLInputElement;
  button.click();

  expect(ctn.mock.calls.length).toBe(1);
  expect(ctn.mock.calls[0][0]).toBe("foo");
});

test("alias can contain special characters", () => {
  const cancel = vi.fn();
  const ctn = vi.fn((_str) => {
    /* */
  });
  promptDeviceAliasPage(
    {
      title: "Title",
      continue: ctn,
      cancel,
      i18n: new I18n(),
    },
    document.body,
  );

  const input = document.querySelector("#pickAliasInput") as HTMLInputElement;
  input.value = "paşśkey 🚀";

  const button = document.querySelector("#pickAliasSubmit") as HTMLInputElement;
  button.click();

  expect(ctn.mock.calls.length).toBe(1);
  expect(ctn.mock.calls[0][0]).toBe("paşśkey 🚀");
});

test("alias should be trimmed", () => {
  const cancel = vi.fn();
  const ctn = vi.fn((_str) => {
    /* */
  });
  promptDeviceAliasPage(
    {
      title: "Title",
      continue: ctn,
      cancel,
      i18n: new I18n(),
    },
    document.body,
  );

  const input = document.querySelector("#pickAliasInput") as HTMLInputElement;
  input.value = " foo ";

  const button = document.querySelector("#pickAliasSubmit") as HTMLInputElement;
  button.click();

  expect(ctn.mock.calls.length).toBe(1);
  expect(ctn.mock.calls[0][0]).toBe("foo");
});

test("alias must not be whitespace only", () => {
  const cancel = vi.fn();
  const ctn = vi.fn((_str) => {
    /* */
  });
  promptDeviceAliasPage(
    {
      title: "Title",
      continue: ctn,
      cancel,
      i18n: new I18n(),
    },
    document.body,
  );

  const input = document.querySelector("#pickAliasInput") as HTMLInputElement;
  input.value = "  ";

  const button = document.querySelector("#pickAliasSubmit") as HTMLInputElement;
  button.click();

  expect(ctn.mock.calls.length).toBe(0);
});
