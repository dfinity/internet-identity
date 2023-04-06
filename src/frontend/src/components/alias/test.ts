import { I18n } from "../../i18n";
import { promptDeviceAliasPage } from "../alias";

test("can be canceled", () => {
  const cancel = jest.fn();
  const ctn = jest.fn((_str) => {
    /* */
  });
  promptDeviceAliasPage(
    {
      title: "Title",
      continue: ctn,
      cancel,
      i18n: new I18n(),
    },
    document.body
  );

  const elem = document.querySelector("#pickAliasCancel") as HTMLElement;
  elem.click();

  expect(cancel.mock.calls.length).toBe(1);
});

test("can be picked", () => {
  const cancel = jest.fn();
  const ctn = jest.fn((_str) => {
    /* */
  });
  promptDeviceAliasPage(
    {
      title: "Title",
      continue: ctn,
      cancel,
      i18n: new I18n(),
    },
    document.body
  );

  const input = document.querySelector("#pickAliasInput") as HTMLInputElement;
  input.value = "foo";

  const button = document.querySelector("#pickAliasSubmit") as HTMLInputElement;
  button.click();

  expect(ctn.mock.calls.length).toBe(1);
  expect(ctn.mock.calls[0][0]).toBe("foo");
});
